package de.fosd.typechef.cifdeftoif

import java.io._
import java.util.{Collections, IdentityHashMap}

import de.fosd.typechef.ConfigurationHandling
import de.fosd.typechef.conditional._
import de.fosd.typechef.error.TypeChefError
import de.fosd.typechef.featureexpr._
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.featureexpr.sat._
import de.fosd.typechef.lexer.FeatureExprLib
import de.fosd.typechef.parser.c.CASTEnv._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CTypeSystemFrontend, IdentityIdHashMap}
import org.apache.logging.log4j.LogManager
import org.kiama.rewriting.Rewriter._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * #ifdef-to-if transforms compile-time variability using preprocessor based #ifdefs into run-time
 * variability using if statements of C. The process is called variability encoding and the result
 * is a product simulator. The simulator is a single program capable of "simulating" all variants,
 * which have been represented with #ifdefs in the input program.
 *
 * During the transformation, #ifdef-annotated code that cannot be transformed directly is duplicated
 * and renamed. For example, if a function for example has two different return types (e.g., long and ing),
 * we duplicate the function and rename it by adding an ifdeftoif name prefix. Additionally all usages
 * of the function are renamed (and duplicated) accordingly.
 *
 * // Original source code
 * #ifdef X64
 * long
 * #else
 * int
 * #endif
 * foo () { return 0; }
 *
 * // Ifdeftoif code
 * long _X64_foo() { return 0; }
 * int _NotX64_foo() { return 0; }
 */
class IfdefToIf extends ASTNavigation with ConditionalNavigation with IfdefToIfStatisticsInterface with IOUtilities {
    private lazy val logger = LogManager.getLogger(this.getClass.getName)
    // Default configuration flag, can either be '0' or '1'
    val defaultValue = "0"
    private val trueF = FeatureExprFactory.True
    // conversion factor: nanoseconds to milliseconds
    private val nstoms = 1000000
    private val tb = java.lang.management.ManagementFactory.getThreadMXBean
    private val fs = System.getProperty("file.separator")
    private val defaultConfigurationParameter = Constant(defaultValue)
    // Default feature selection state for features which do not appear in the .config file, false = off, true = on
    private val defaultFeatureSelection = false
    /* Path variables */
    private val path = new File("..").getCanonicalPath ++ fs ++ "ifdeftoif" ++ fs
    // Path used to serialize the features found in the current AST
    private val serializedFeaturePath = path ++ "featureSet.txt"
    // Path to an extern struct declaration
    private val externOptionStructPath = path ++ "id2i_optionstruct.h"
    // Path to the statistics file
    private val statisticsPath = path ++ "statistics.csv"
    // Path to a file containing skipped program elements where the variant number exceeds the duplicationThreshold
    private val skippedDuplicationsPath = path ++ "skipped_duplications.txt"
    // Path to the top level statistics file
    private val topLevelStatisticsPath = path ++ "top_level_statistics.csv"
    // Path to the top levelstatistics file
    private val typeErrorPath = path ++ "type_errors.txt"
    // Default config path
    private val defConfig = new File("..").getCanonicalPath ++ fs ++ "TypeChef-BusyboxAnalysis" ++ fs ++ "BusyBoxDefConfig.config"
    // Name of the initialized ifdeftoif configuration struct
    private val featureStructInitializedName = "id2i"
    // nName of the function used to assign values to the struct members in the configuration struct
    private val initFunctionName = "id2i_init"
    // Name of the ifdeftoif configuration struct type
    private val featureStructName = "ifdef_options"
    // Suffix under which the ifdeftoif file is saved
    private val ifdeftoifFileSuffix = "_ifdeftoif.c"
    // Threshold for a list size for computation of cartesian product
    private val duplicationThreshold = 200
    // Data structure used to map Identifiers, which have to be renamed, to the presence conditions of the renamings
    private val idsToBeReplaced: IdentityHashMap[Id, Set[FeatureExpr]] = new IdentityHashMap()
    // Same data structure as above used for the second ifdeftoif run
    private val idsToBeReplacedSecondRun: IdentityHashMap[Id, Set[FeatureExpr]] = new IdentityHashMap()
    private val createFunctionsForModelChecking = false
    /* Variables used for the ifdeftoif transformation process */
    private var astEnv: ASTEnv = null
    private var fm: FeatureModel = FeatureExprFactory.empty
    private var parseFM: FeatureModel = FeatureExprFactory.empty
    // The current name of the file to be transformed without extensions or file path
    private var currentFileName = ""
    // SingleFeatureExpressions found in the current AST
    private var featureExpressions: Set[SingleFeatureExpr] = Set()
    private var featuresInAst: Int = 0
    private var isFirstRun = true
    // Data structure which maps definitions of variables to their usages
    private var defuse: IdentityIdHashMap = new IdentityIdHashMap(new IdentityHashMap())
    // Data structure which maps usages of variables to their definition(s)
    private var usedef: IdentityIdHashMap = new IdentityIdHashMap(new IdentityHashMap())
    // Data structure which maps presence conditions to numbers used as prefixes in renamings
    private var presenceConditionNumberMap: Map[FeatureExpr, Int] = Map()
    // Data structure used for exporting Identifier renaming data
    private var replaceId: IdentityHashMap[Id, FeatureExpr] = new IdentityHashMap()

    def setParseFM(smallFM: FeatureModel) = {
        parseFM = smallFM
    }

    def getIfdeftoifContext(context: FeatureExpr, functionContext: FeatureExpr, isTopLevel: Boolean = false): IfdeftoifContext = {
        new IfdeftoifContext(context, functionContext, isTopLevel)
    }

    /**
     * Retrieves an abstract syntax tree for a given file.
     */
    def getAstFromFile(fileToAnalyse: File): TranslationUnit = {
        def parseFile(stream: InputStream, file: String, dir: String): TranslationUnit = {
            // TODO @fgarbe: add feature model to parser.
            val ast: TranslationUnit = new ParserMain(new CParser).parserMain(
                () => CLexerAdapter.lexStream(stream, file, Collections.singletonList(dir), null), new CTypeContext, SilentParserOptions, null)
            ast.asInstanceOf[TranslationUnit]
        }
        val fis = new FileInputStream(fileToAnalyse)
        val ast = parseFile(fis, fileToAnalyse.getName, fileToAnalyse.getParent)
        fis.close()
        ast
    }

    /**
     * Checks given AST for type errors, prints errors onto the console and returns if the type check was successful.
     */
    def checkAst(ast: TranslationUnit): Boolean = {
        getTypeSystem(ast).checkAST()
    }

    /**
     * Returns a new CTypeSystemFrontend for a given AST.
     */
    private def getTypeSystem(ast: TranslationUnit): CTypeSystemFrontend = {
        new CTypeSystemFrontend(ast)
    }

    /**
     * Checks given AST for type errors.
     */
    def checkAstSilent(ast: TranslationUnit): Boolean = {
        getTypeSystem(ast).checkASTSilent
    }

    /**
     * Returns the errors retrieved from a typecheck on given AST.
     */
    def getAstErrors(ast: TranslationUnit): List[TypeChefError] = {
        getTypeSystem(ast).getASTerrors()
    }

    /**
     * Creates an AST representing run-time options generated by the transformation.
     * The AST includes:
     * 1. an external int (as requested by Alexander von Rhein)
     * 2. an option structure with all configuration options to parametrize the product simulator
     * 3. functions for initializing the the option structure.
     */
    def generateIfdefOptionsTUnit(ast: AST): TranslationUnit = {
        val features = IfdeftoifUtils.getSingleFeatures(ast)
        val optionsAst = getInitialTranslationUnit(features)
        optionsAst
    }

    /**
     * Returns the initial TranslationUnit with the ifdeftoif option struct and a function which initializes
     * the features inside the ifdeftoif option struct with values.
     */
    private def getInitialTranslationUnit(defExSet: Set[SingleFeatureExpr], featureConfigPath: String = ""): TranslationUnit = {
        val structDeclaration = Opt(trueF, getOptionStruct(defExSet))
        if (!createFunctionsForModelChecking) {
            TranslationUnit(List(structDeclaration, Opt(trueF, getInitFunction(defExSet, featureConfigPath))))
        } else {
            val initialFunctions = getFunctionsForModelChecking()
            val initFunction = Opt(trueF, getModelCheckInitFunction(defExSet))
            TranslationUnit(initialFunctions ++ List(structDeclaration, initFunction))
        }
    }

    /**
     * Returns the AST representation of functions and declarations used for model checking.
     */
    private def getFunctionsForModelChecking(): List[Opt[ExternalDef]] = {
        // extern int __VERIFIER_nondet_int(void);
        val externDeclaration = Declaration(
            List(Opt(trueF, ExternSpecifier()), Opt(trueF, IntSpecifier())),
            List(Opt(trueF,
                InitDeclaratorI(
                    AtomicNamedDeclarator(
                        List(),
                        Id("__VERIFIER_NONDET_INT"),
                        List(Opt(trueF, DeclParameterDeclList(List(Opt(trueF,
                            PlainParameterDeclaration(List(Opt(trueF,
                                VoidSpecifier())), List()))))))), List(), None))))

        // int select_one() {
        //   if (__VERIFIER_nondet_int())
        //     return 1;
        //   else
        //     return 0;
        // }
        val selectOneFunction = FunctionDef(List(Opt(trueF, IntSpecifier())),
            AtomicNamedDeclarator(List(), Id("select_one"), List(Opt(trueF, DeclIdentifierList(List())))), List(),
            CompoundStatement(List(Opt(trueF,
                IfStatement(
                    One(PostfixExpr(Id("__VERIFIER_NONDET_INT"), FunctionCall(ExprList(List())))),
                    One(CompoundStatement(List(Opt(trueF, ReturnStatement(Some(Constant("1"))))))), List(),
                    Some(One(CompoundStatement(List(Opt(trueF, ReturnStatement(Some(Constant("0")))))))))))))

        List(Opt(trueF, externDeclaration), Opt(trueF, selectOneFunction))
    }

    /**
     * Returns a function used to initialize the feature values of the ifdeftoif option struct with the value
     * 'select_one' from the function 'getFunctionsForModelChecking'.
     */
    private def getModelCheckInitFunction(defExSet: Set[SingleFeatureExpr]): FunctionDef = {
        val cmpStmt = defExSet.map(x => {
            Opt(trueF, ExprStatement(AssignExpr(PostfixExpr(Id(featureStructInitializedName), PointerPostfixSuffix(".", Id(x.feature.toLowerCase))), "=", PostfixExpr(Id("select_one"), FunctionCall(ExprList(List()))))))
        }).toList
        FunctionDef(List(Opt(trueF, VoidSpecifier())), AtomicNamedDeclarator(List(), Id("initOptions"), List(Opt(trueF, DeclIdentifierList(List())))), List(), CompoundStatement(cmpStmt))
    }

    /**
     * Loads the currently serialized features from @serializedFeaturePath and updates it with the features found in
     * given ast.
     */
    def loadAndUpdateFeatures(ast: TranslationUnit): Unit = {
        loadAndUpdateFeatures(IfdeftoifUtils.getSingleFeatures(ast))
    }

    /**
     * Loads the currently serialized features from @serializedFeaturePath and updates it with the features found in
     * given feature expression set.
     */
    def loadAndUpdateFeatures(newFeatures: Set[SingleFeatureExpr]): Unit = {
        featureExpressions ++= newFeatures
        featuresInAst = featureExpressions.size
        var allFeatureExpressions: Set[SingleFeatureExpr] = Set() // all fexp (this file and previous files)
        if (new File(serializedFeaturePath).exists) {
            val loadedFeatures = loadSerializedFeatureNames(serializedFeaturePath)
            allFeatureExpressions = featureExpressions ++ loadedFeatures
        } else {
            allFeatureExpressions = featureExpressions
        }
        serializeFeatureNames(allFeatureExpressions.map(_.feature.toString), serializedFeaturePath)
    }

    /**
     * Creates an id2i_optionstruct.h file with the ifdeftoif option struct and the init function for
     * assigning selection states to features. The feature selection states are read from the given .config file path.
     */
    def writeExternIfdeftoIfStruct(featureConfigPath: String, defaultConfigExpr: Expr = defaultConfigurationParameter, prefix: String = "") = {
        val featureSet = loadSerializedFeatureNames(serializedFeaturePath)
        val structDeclaration = Opt(trueF, getOptionStruct(featureSet))
        val externDeclaration = Opt(trueF, Declaration(List(Opt(trueF, ExternSpecifier()), Opt(trueF, StructOrUnionSpecifier(false, Some(Id(featureStructName)), None, List(), List()))), List(Opt(trueF, InitDeclaratorI(AtomicNamedDeclarator(List(), Id(featureStructInitializedName), List()), List(), None)))))
        val initFunction = Opt(trueF, getInitFunction(featureSet, featureConfigPath, defaultConfigExpr))

        PrettyPrinter.printF(TranslationUnit(List(structDeclaration, externDeclaration, initFunction)), externOptionStructPath, prefix)
    }

    /**
     * Converts a set of FeatureExpressions into a struct declaration.
     */
    private def getOptionStruct(defExSet: Set[SingleFeatureExpr]): Declaration = {
        val structDeclList = defExSet.map(x => {
            featureToStructDeclaration(x.feature)
        }).toList
        createDeclaration(structDeclList)
    }

    /**
     * Converts the name of a feature to an AST element of the type StructDeclaration. Ex: "CONFIG_X64" -> int config_x64
     * @param featureName
     * @return
     */
    private def featureToStructDeclaration(featureName: String): Opt[StructDeclaration] = {
        Opt(trueF, StructDeclaration(List(Opt(trueF, IntSpecifier())),
            List(Opt(trueF,
                StructDeclarator(
                    AtomicNamedDeclarator(List(), Id(featureName.toLowerCase), List()),
                    None,
                    List())))))
    }

    /**
     * Converts a list of StructDeclarations into a Declaration. Ex: List(int config_x64) -> struct ifdefoptions {int config_x64;} id2i;
     * @param structDeclList
     * @return
     */
    private def createDeclaration(structDeclList: List[Opt[StructDeclaration]]): Declaration = {
        Declaration(List(Opt(trueF,
            StructOrUnionSpecifier(false, Some(Id(featureStructName)), Some(structDeclList), List(), List()))),
            List(Opt(trueF, InitDeclaratorI(AtomicNamedDeclarator(List(), Id(featureStructInitializedName), List()),
                List(), None))))
    }

    /**
     * Takes a set of SingleFeatureExpr and a path to a feature configuration file and returns an init function which
     * assigns values to the struct members of the ifdeftoif config struct according to the feature selection states
     * from the given feature configuration file.
     * @param defExSet
     * @param featureConfigPath
     * @return
     */
    private def getInitFunction(defExSet: Set[SingleFeatureExpr], featureConfigPath: String = "", defaultConfiguration: Expr = defaultConfigurationParameter): FunctionDef = {
        var exprStmts: List[Opt[ExprStatement]] = List()

        if (!featureConfigPath.isEmpty) {
            val featureConfigFile = new File(featureConfigPath)
            val (trueFeats, falseFeats, otherFeats) = ConfigurationHandling.getFeaturesFromConfiguration(featureConfigFile, fm, defExSet)

            val trueExprs = trueFeats.map(x => featureToAssignment(x.feature, Constant("1")))
            val falseExprs = falseFeats.map(x => featureToAssignment(x.feature, Constant("0")))
            val otherExprs = otherFeats.map(x => featureToAssignment(x.feature, defaultConfiguration))
            exprStmts = trueExprs ++ otherExprs ++ falseExprs
        } else {
            exprStmts = defExSet.toList.map(x => featureToAssignment(x.feature, defaultConfiguration))
        }
        FunctionDef(List(Opt(trueF, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(initFunctionName), List(Opt(trueF, DeclIdentifierList(List())))), List(), CompoundStatement(exprStmts))
    }

    /**
     * Converts the name of a feature 'n' and a given target expression 'e' into an assignment in the form of id2i.'n' = 'e';
     * @param featureName
     * @param assignmentSource
     * @return
     */
    private def featureToAssignment(featureName: String, assignmentSource: Expr): Opt[ExprStatement] = {
        Opt(trueF,
            ExprStatement(AssignExpr(PostfixExpr(Id(featureStructInitializedName),
                PointerPostfixSuffix(".", Id(featureName.toLowerCase))), "=", assignmentSource)))
    }

    /**
     * Loads a serialized Set of SingleFeatureExpressions.
     */
    private def loadSerializedFeatureNames(filename: String): Set[SingleFeatureExpr] = try {
        if (!new File(filename).exists) {
            System.err.println("did not find a serialized feature set, initializing it in: " + filename)
            return Set()
        }
        val fr = new ObjectInputStream(new FileInputStream(filename)) {
            override protected def resolveClass(desc: ObjectStreamClass) = {
                /*println(desc);*/ super.resolveClass(desc)
            }
        }
        val sfe = fr.readObject().asInstanceOf[Set[String]]
        fr.close()
        sfe.map(FeatureExprFactory.createDefinedExternal)
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized FeatureSet: " + e.getMessage); Set()
    }

    /**
     * Function to lift/step up variability to the next parental variability node (Opt[_] or Conditional[_]).
     * This should simplify the transformation of #ifdef-to-if a lot because it provides a general pattern for
     * code duplication required as a result of insufficient transformation capabilities of low-level #ifdefs to
     * if statements.
     *
     * We always step up to the next parental variability node and resolve all variability (Opt[_] and Conditional[_])
     * in sub-elements the given input AST element a. We consider two scenarios during the transformation process:
     * 1. Lifting to the next Opt[_] o; replace o with List[Opt[_]].
     * 2. Lifting up to the next One[_] (leave node of Conditional[_]) c; replace c with a Choice[_].
     *
     * The function returns Left for Conditional[_] and Right for Opt[_]. Both include the element to replace in the
     * translation unit and the replacement (i.e., Choice[_] and List[Opt[_]]).
     */
    def liftVariability(a: AST, env: ASTEnv): Either[_, _] = {

        def validConfigurations(x: Product) = {
            // collect variability in sub-elements and compute all satisfiable configurations
            val ps = collectConfigurations(x, env)

            // generate power set to determine all possible combinations
            ps.subsets.toSet[Set[FeatureExpr]]
                // create configurations and filter unsatisfiable ones
                .map(s => s.fold(trueF)(_ and _)).filter(_.isSatisfiable(fm))
        }

        // get parental variability node
        parentVNode(a, env) match {
            case Left(o: One[_]) =>
                val cs = validConfigurations(o)

                // duplicate the variable, parental node and zip each element with a valid configuration
                val ol = List.fill(cs.size)(o).zip(cs)

                // remove variability from the duplicated nodes based on the given configuration
                val pol = ol.map { e => Opt(e._2, purgeVariability(e._1, e._2, env))}

                // create incrementally a Conditional[_] tree from the duplicated One[_] entries
                // we use the head element as the seed for the tree creation
                val cctx: FeatureExpr = pol.head.feature
                val cinit: Conditional[Any] = pol.head.entry
                val res = pol.tail.foldLeft(cinit)((t, e) => ConditionalLib.insert(t, cctx, e.feature, e.entry.value))

                Left(o, res)

            case Right(o: Opt[_]) =>
                val cs = validConfigurations(o)

                // duplicate the variable, parental node and zip each element with a valid configuration
                val ol = List.fill(cs.size)(o).zip(cs)

                // remove variability from the duplicated nodes based on the given configuration
                val res = ol.map {
                    e =>
                        val no = purgeVariability(e._1, e._2, env)
                        no.copy(feature = no.feature.and(e._2))
                }

                Right(o, res)
        }
    }

    /**
     * Replaces variability nodes in o with non-variable nodes (Opt(True, ...) or One(_))
     * according to the given feature expression config..
     */
    private def purgeVariability[T](o: T, config: FeatureExpr, env: ASTEnv): T = {
        manytd(rule {
            case l: List[Opt[_]] => l.flatMap {
                e =>
                    val fexp = env.featureExpr(e.entry)
                    if (config.implies(fexp).isTautology(fm))
                        Some(e.copy(feature = trueF))
                    else
                        None
            }
            case Choice(_, tb, eb) =>
                val fexp = env.featureExpr(tb)
                if (config.implies(fexp).isTautology(fm))
                    tb
                else
                    eb
        })(o).getOrElse(o).asInstanceOf[T]
    }

    /**
     * Return all configurations that arise from variability in sub-elements of a.
     */
    private def collectConfigurations(a: Product, env: ASTEnv) = {
        var res: Set[Set[FeatureExpr]] = Set()
        manytd(query {
            case Opt(_, x) => res += env.featureSet(x)
        })(a)
        res.map(s => s.fold(trueF)(_ and _))
    }

    /**
     * Transforms given Conditional[Expr] into a ConditionalExpr.
     * First we transform the conditional node into a List[Tuple2[Expr, FeatureExpr]] and then we check this tuple for valid
     * candidates in the given presence condition currentContext.
     * Case Nil: conditionalTuple is empty -> return the original expr
     * Case x :: Nil: we found one expression, return it
     * Case x :: xs: we found several expressions, turn them into a conditional expression
     *
     * If transformExpr is set to true, we also transform variability inside the given expression immediately.
     * Ex: Choice(def(A), 0, 1) -> id2i.a ? 0 : 1
     */
    def conditionalToConditionalExpr(choice: Conditional[Expr], curCtx: FeatureExpr, functionContext: FeatureExpr, transformExpr: Boolean = false): One[Expr] = {
        val tList = conditionalToList(choice, curCtx)
        tList match {
            case Nil =>
                choice match {
                    case o: One[Expr] => o
                    case c: Choice[Expr] => One(tList.find(e => e._1.implies(curCtx).isTautology()).getOrElse((trueF, Id("")))._2)
                }
            case (fExp, exp) :: Nil =>
                val currentExpr = replaceOptAndId(exp, fExp, functionContext)
                if (transformExpr) {
                    val features = computeFExpsForDuplication(exp, fExp)
                    One(convertToCondExpr(currentExpr, features, curCtx, functionContext))
                } else {
                    One(currentExpr)
                }
            case (fExp, exp) :: xs =>
                val innerExpr = replaceOptAndId(exp, fExp, functionContext)
                if (transformExpr) {
                    val transformedInnerExpr = convertToCondExpr(innerExpr, computeFExpsForDuplication(innerExpr, fExp), curCtx, functionContext)
                    val newInnerExpr = transformedInnerExpr
                    val resultExpr = xs.foldLeft(newInnerExpr)((expr, condTuple) => {
                        val newExpr = replaceOptAndId(condTuple._2, condTuple._1, functionContext)
                        val transformedNewExpr = convertToCondExpr(newExpr, computeFExpsForDuplication(newExpr, condTuple._1), curCtx, functionContext)
                        ConditionalExpr(toCExpr(fExprDiff(curCtx, condTuple._1)), Some(transformedNewExpr), expr)
                    })
                    One(resultExpr)
                } else {
                    val resultExpr = xs.foldLeft(innerExpr)((expr, condTuple) => {
                        val newExpr = replaceOptAndId(condTuple._2, condTuple._1, functionContext)
                        ConditionalExpr(toCExpr(fExprDiff(curCtx, condTuple._1)), Some(newExpr), expr)
                    })
                    One(resultExpr)
                }
        }
    }

    /**
     * Creates all possible 2 power n combinations for a list of n raw (single) feature expressions. List(def(x64), def(x86))
     * becomes List(def(x64)&def(x86),!def(x64)&def(x86),def(x64)&!def(x86),!def(x64)&!def(x86).
     * Also filters out features which are not satisfiable according to the feature model and the given context.
     */
    def getFeatureCombinationsFiltered(fList: List[FeatureExpr], context: FeatureExpr): List[FeatureExpr] = {
        getFeatureCombinations(fList).filter(x => x.and(context).isSatisfiable(fm))
    }

    /**
     * Whenever we rename a declaration of a variable/field/etc. we call this function.
     * Using the Declaration-Use-Map we store the usages of renamed declarations and the presence conditions under which
     * they were renamed. Later on this information will be used to rename the corresponding usages of identifiers.
     */
    def addIdUsages(i: Id, ft: FeatureExpr) {
        def putIntoIdsToBeReplaced(i: Id, feature: FeatureExpr) {
            if (idsToBeReplaced.containsKey(i)) {
                idsToBeReplaced.put(i, idsToBeReplaced.get(i) + feature)
                idsToBeReplacedSecondRun.put(i, idsToBeReplaced.get(i) + feature)
            } else {
                idsToBeReplaced.put(i, Set(feature))
                idsToBeReplacedSecondRun.put(i, Set(feature))
            }
        }
        incRenamings()
        if (defuse.containsKey(i)) {
            val idUsages = defuse.get(i)
            incRenamingUsages(idUsages.size)
            idUsages.foreach(x => {
                putIntoIdsToBeReplaced(x, ft)
            })
        } else if (usedef.containsKey(i)) {
            val idUsages = usedef.get(i).flatMap(x => {
              val usages = defuse.get(x) // can returned null iff no usage is known; if null is returned in flatMap then NPE is thrown
              if (usages != null) usages else Set()
            })
            idUsages.foreach(x => putIntoIdsToBeReplaced(x, ft))
        }
    }

    /**
     * Converts a given Choice[Expr] element to a ConditionalExpr. Example:
     * Choice(def(A),One(Id(a)),Choice(def(B),One(Id(b)),One(null))) -> (id2i_opt.a ? a : ((! id2i_opt.b) ?  : b))
     */
    def conditionalToCondExpr(current: Conditional[Expr], currentContext: FeatureExpr, functionContext: FeatureExpr): Conditional[Expr] = {
        def conditionalToCondExprHelper(condExpr: Conditional[Expr], currentFeature: FeatureExpr = trueF): Expr = {
            condExpr match {
                case One(expr: Expr) =>
                    replaceOptAndId(expr, currentFeature, functionContext)
                case Choice(feat, One(null), elseBranch) =>
                    ConditionalExpr(toCExpr(feat), None, conditionalToCondExprHelper(elseBranch, currentFeature.and(feat.not())))
                case Choice(feat, thenBranch, One(null)) =>
                    ConditionalExpr(toCExpr(feat.not), None, conditionalToCondExprHelper(thenBranch, currentFeature.and(feat)))
                case Choice(feat, thenBranch, elseBranch) =>
                    ConditionalExpr(toCExpr(feat), Some(conditionalToCondExprHelper(thenBranch, currentFeature.and(feat))), conditionalToCondExprHelper(elseBranch, currentFeature.and(feat.not())))
            }
        }
        current match {
            case One(_) => current
            case Choice(_, _, _) => One(conditionalToCondExprHelper(current))
        }
    }

    /**
     * Renames identifiers by adding the ifdeftoif prefix number for given FeatureExpr ft. Also updates
     * idsToBeReplacedSecondRun by removing identifiers which have already been renamed during this first run.
     */
    def prependCtxPrefix(id: Id, context: FeatureExpr): Id = {
        var actualContext = context
        var idname = id.name
        if (context.equivalentTo(trueF)) {
            id
        } else {
            // Identifier was renamed in the first run and already has an ifdeftoif prefix
            if (java.util.regex.Pattern.compile("_[0-9]+_.+").matcher(id.name).matches()) {
                val oldPrefix = id.name.split('_')(1)
                val intPrefix = oldPrefix.toInt
                idname = id.name.substring(2 + oldPrefix.length())
                val oldContext = getFeatureForId(intPrefix)
                oldContext match {
                    case Some(oldFeature: FeatureExpr) =>
                        actualContext = oldFeature and context
                    case _ =>
                }
            }
            if (idsToBeReplaced.containsKey(id)) {
                idsToBeReplacedSecondRun.remove(id)
            }
            Id(getPrefixFromIdMap(actualContext) + idname)
        }
    }

    def removeOptFeatures[S <: Product](current: S): S = {
        val r = manytd(rule {
            case Opt(ft, entry) =>
                Opt(trueF, entry)
        })
        r(current).getOrElse(current).asInstanceOf[S]
    }

    /**
     * Replaces given FeatureExpression recursively from given Element by True. Also removes Opt nodes which should not
     * occur in this given context. Also renames identifiers if they have a declaration annotated by given FeatureExpression.
     */
    def replaceOptAndId[S <: Product](current: S, feature: FeatureExpr, functionContext: FeatureExpr): S = {
        def replaceHelp[T <: Any](t: T, feat: FeatureExpr, fctCtx: FeatureExpr): T = {
            val r = manytd(rule {
                case l: List[Opt[_]] =>
                    l.flatMap {
                        case o@Opt(ft, LabelStatement(id, attrib)) =>
                            val composedFeature = ft.and(feat)
                            if (!composedFeature.isSatisfiable(fm)) {
                                List()
                            } else if (functionContext.implies(ft).isTautology(fm) && ft.implies(feat).isTautology(fm)) {
                                List(o.copy(feature = trueF))
                            } else {
                                // Labels have to be renamed to avoid having the same label name occur multiple times after a duplication
                                addIdUsages(id, feat)
                                List(Opt(trueF, LabelStatement(prependCtxPrefix(id, feat), attrib)))
                            }
                        case o: Opt[_] =>
                            // Feature in opt node is equal or less specific than the context, replace opt node feature with True
                            if (o.feature.equivalentTo(feat, fm) || feat.implies(o.feature).isTautology(fm)) {
                                List(o.copy(feature = trueF))
                            }
                            // Feature in opt node is more specific and still satisfiable in the context, don't change opt node
                            else if (feat.and(o.feature).isSatisfiable(fm)) {
                                List(o)
                            }
                            // Feature in opt node is not satisfiable in the current context, remove opt node
                            else {
                                List()
                            }
                    }
                case c: Conditional[Any] =>
                    c.simplify(feat)
                case i: Id if !idsToBeReplaced.containsKey(i) => i
                case i: Id =>
                    updateIdMap(feat)
                    val featureList = idsToBeReplaced.get(i)
                    val matchingId = featureList.filter(x => feat.implies(x).isTautology(fm)).toList
                    matchingId match {
                        case Nil =>
                            // Check for a matching ID in a context which is still satisfiable
                            val newMatchingId = featureList.filter(x => feat.and(x).isSatisfiable(fm)).toList
                            newMatchingId match {
                                case Nil =>
                                    // TODO: this should not happen?
                                    i
                                case (x: FeatureExpr) :: Nil =>
                                    prependCtxPrefix(i, x)
                                case _ =>
                                    i
                            }
                        case (x: FeatureExpr) :: Nil =>
                            prependCtxPrefix(i, x)
                        case _ =>
                            i
                    }
            })
            r(t).getOrElse(t).asInstanceOf[T]
        }

        if (feature.equivalentTo(trueF, fm) && isFirstRun) {
            current
        } else {
            current match {
                case Opt(ft, entry) =>
                    if (ft.equivalentTo(trueF, fm) || ft.equivalentTo(feature, fm)) {
                        Opt(trueF, replaceHelp(entry, feature, functionContext)).asInstanceOf[S]
                    } else {
                        Opt(ft, replaceHelp(entry, feature, functionContext)).asInstanceOf[S]
                    }
                case _ => replaceHelp(current, feature, functionContext)
            }
        }
    }

    /**
     * Transforms identifiers inside elements where the variant number exceeds the computation threshold if they can be
     * assigned to exactly one context.
     * Example:
     * #ifdef A
     * String foo;
     * #endif
     *
     * opt = getopt32(argv, "txC:f:Oopvk"
     * ... // several other Strings in #ifdefs which lead to an exceedance of variants for this element
     * #ifdef A
     * foo
     * #endif
     * );
     * This function now renames the identifier foo in the assignment of 'opt' because the original foo declaration
     * has been renamed because it was an optional declaration.
     */
    def transformPossibleIdentifiers[S <: Product](current: S, functionContext: FeatureExpr): S = {
        val r = manytd(rule {
            case i: Id if !idsToBeReplaced.containsKey(i) => i
            case i: Id =>
                val feat = astEnv.featureExpr(i)
                updateIdMap(feat)
                val featureList = idsToBeReplaced.get(i)
                val matchingId = featureList.find(x => feat.implies(x).isTautology(fm))
                matchingId match {
                    // TODO: this should not happen?
                    case None => i
                    case Some(x: FeatureExpr) =>
                        if (x.equivalentTo(trueF, fm)) {
                            i
                        } else {
                            prependCtxPrefix(i, x)
                        }
                }
        })
        current match {
            case Opt(feat, entry: Product) if !feat.equals(trueF) =>
                r(Opt(trueF, replaceOptAndId(transformDeclId(entry, feat), feat, functionContext))).getOrElse(current).asInstanceOf[S]
            case _ =>
                r(current).getOrElse(current).asInstanceOf[S]
        }
    }

    /**
     * Calls the replaceOptAndId function first and then the transformRecursive function on given Product.
     */
    def replaceAndTransform[T <: Product](t: T, feat: FeatureExpr, isTopLevel: Boolean, functionContext: FeatureExpr): T = {
        transformRecursive(replaceOptAndId(t, feat, functionContext), feat, isTopLevel, functionContext)
    }

    /**
     * Starts the ifdeftoif transformation process on the given TranslationUnit and additional information like
     * the featureModel, declUseMap etc.
     */
    def ifdeftoif(source_ast: TranslationUnit, decluse: IdentityIdHashMap, usedecl: IdentityIdHashMap, featureModel: FeatureModel = FeatureExprLib.featureModelFactory.empty, outputStem: String = "unnamed", lexAndParseTime: Long = 0, writeStatistics: Boolean = true, newPath: String = "", typecheckResult: Boolean = true, useExternConfigStruct: Boolean = true): (Option[AST], Long, List[TypeChefError]) = {
        new File(path).mkdirs()
        init(fm)

        // Set the feature model, declUseMap, useDeclMap, astEnv
        fm = featureModel
        defuse = decluse
        usedef = usedecl
        astEnv = createASTEnv(source_ast)

        fillIdMap(source_ast)
        loadAndUpdateFeatures(source_ast)

        val fileNameWithExt = basename(outputStem)
        currentFileName = getFileNameWithoutExtension(fileNameWithExt)

        val time = tb.getCurrentThreadCpuTime
        var new_ast = transformRecursive(source_ast, trueF, true)

        // Check if a second run is required, can be the case if structs get used before they get defined
        if (!idsToBeReplacedSecondRun.isEmpty) {
            isFirstRun = false
            new_ast = transformRecursive(new_ast, trueF)
        }
        val transformTime = (tb.getCurrentThreadCpuTime - time) / nstoms
        var result_ast: TranslationUnit = new_ast

        var typecheck_ast: TranslationUnit = TranslationUnit(List())
        if (useExternConfigStruct) {
            typecheck_ast = TranslationUnit(getInitialTranslationUnit(featureExpressions).defs ++ new_ast.defs)
        } else {
            result_ast = TranslationUnit(getInitialTranslationUnit(featureExpressions).defs ++ new_ast.defs)
            typecheck_ast = result_ast
        }
        exportRenamings()

        var ifdeftoif_file = ""
        if (newPath.equals("")) {
            ifdeftoif_file = ifdeftoifFilePath(outputStem)
        } else {
            ifdeftoif_file = newPath
        }

        if (useExternConfigStruct) {
            PrettyPrinter.printD(result_ast, ifdeftoif_file, createIncludeDirective(externOptionStructPath))
        } else {
            PrettyPrinter.printD(result_ast, ifdeftoif_file)
        }
        println("Printed ifdeftoif to file " + ifdeftoif_file)

        if (!typecheckResult) {
            println("Skipping typecheck of ifdeftoif result")
            (Some(result_ast), transformTime, List())
        } else {
            println("Typechecking result")
            val typeCheckSuccessful = checkAstSilent(typecheck_ast)

            val presenceConditions = presenceConditionNumberMap.-(trueF)
            if (!presenceConditions.isEmpty)
                writeToFile(path ++ "featureMap.csv", presenceConditions.map(x => x._1.toTextExpr + "," + x._2) mkString "\n")

            if (typeCheckSuccessful) {
                if (writeStatistics) {
                    exportStatistics(updateIfdeftoifStatistics(source_ast, typecheck_ast, fileNameWithExt, lexAndParseTime, transformTime, featuresInAst, statisticsPath, topLevelStatisticsPath))
                }
                (Some(result_ast), transformTime, List())
            } else {

                // Overwrite file with #include statement with a file which possesses the ifdeftoif config struct
                PrettyPrinter.printD(typecheck_ast, ifdeftoif_file)
                val result_ast_with_position = getAstFromFile(new File(ifdeftoif_file))
                // TODO fgarbe: New solution required!
                if (result_ast_with_position == null) {
                    val errorHeader = "-+ ParseErrors in " + fileNameWithExt + " +-\n"
                    addToFile(typeErrorPath, errorHeader + errorHeader + "\n\n")
                    (None, 0, List())
                } else {
                    val errors = getAstErrors(result_ast_with_position)
                    val errorHeader = "-+ TypeErrors in " + fileNameWithExt + " +-\n"
                    val errorString = errors mkString "\n"
                    addToFile(typeErrorPath, errorHeader + errorString + "\n\n")
                    (None, 0, errors)
                }
            }
        }
    }

    /**
     * Strips the directory (and an optional suffix) from a given file name!
     * Ex: /home/user/main.c -> main.c
     */
    def basename(fName: String, suffix: String = ""): String = {
        val lastSepIndex = fName.lastIndexOf(fs)
        var bn = fName
        if (lastSepIndex != -1)
            bn = fName.substring(lastSepIndex + 1)

        if (suffix != "" && bn.endsWith(suffix))
            bn = bn.substring(0, bn.length - suffix.length)

        bn
    }

    def getFileNameWithoutExtension(file: File): String = {
        file.getName().replaceFirst("[.][^.]+$", "")
    }

    def getFileNameWithoutExtension(strg: String): String = {
        strg.replaceFirst("[.][^.]+$", "")
    }

    /**
     * Runs #ifdef to if transformation on given translation unit.
     */
    def transformAst(tunit: TranslationUnit, decluse: IdentityIdHashMap, usedecl: IdentityIdHashMap,
                     parseTime: Long, featureModel: FeatureModel = FeatureExprLib.featureModelFactory.empty): (TranslationUnit, String) = {
        fm = featureModel
        init(fm)
        val tb = java.lang.management.ManagementFactory.getThreadMXBean

        fillIdMap(tunit)
        astEnv = createASTEnv(tunit)
        featureExpressions = IfdeftoifUtils.getSingleFeatures(tunit)
        defuse = decluse
        usedef = usedecl
        val time = tb.getCurrentThreadCpuTime
        val result = transformRecursive(tunit, trueF, true)
        val transformTime = (tb.getCurrentThreadCpuTime - time) / nstoms

        val csvEntry = createCsvEntry(tunit, result, "unnamed", parseTime, transformTime, featuresInAst)
        (TranslationUnit(getInitialTranslationUnit(featureExpressions).defs ++ result.asInstanceOf[TranslationUnit].defs), csvEntry)
    }

    /**
     * This is the core of the #ifdef to if transformation. This function is called recursively on all opt nodes inside the
     * given AST element. The general strategy is to look at opt nodes:
     * - statements which need to be duplicated or embedded inside if statements
     * - declarations which need to be duplicated/renamed
     */
    def transformRecursive[T <: Product](t: T, curCtx: FeatureExpr = trueF, isTopLevel: Boolean = false, functionContext: FeatureExpr = trueF): T = {
        val r = alltd(rule {
            case l: List[Opt[_]] =>
                l.flatMap {
                    case o@Opt(ft, entry) =>
                        if (entry.isInstanceOf[AST] && entry.asInstanceOf[AST].range.isDefined) {

                            /*
                            Exports the current code position in the source file. Can be used to find out
                            where the #ifdef to if progress stopped or gets stuck.
                             */
                            writeToFile("ifdeftoif_progress.txt", entry.asInstanceOf[AST].range.get.toString() + " from " + o)
                        }
                        val newCtx = curCtx.and(ft)
                        val fExps = computeFExpsForDuplication(entry, newCtx, isTopLevel)
                        if (exceedsThreshold(fExps)) {
                            List(transformPossibleIdentifiers(o, functionContext))
                        } else {
                            entry match {
                                case InitDeclaratorI(decl, attributes, Some(Initializer(initElemLabel, lc: LcurlyInitializer))) =>
                                    // Never transform initializations using curly braces with conditional expressions
                                    List(transformRecursive(o, newCtx, false, functionContext))
                                case InitDeclaratorI(decl, attributes, Some(init@Initializer(initElemLabel, expr))) =>
                                    if (!isTopLevel) {

                                        /*
                                        Local declarations with assignments can be transformed with conditional assignments.
                                        Ex: void main() { int a = id2i.x64 ? 0 : 1 }
                                        Not possible for assignments on global variables!
                                         */
                                        val exprFeatures = computeFExpsForDuplication(expr, newCtx)
                                        if (exceedsThreshold(exprFeatures)) {
                                            List(transformPossibleIdentifiers(o, curCtx))
                                        } else {
                                            val condExpr = convertToCondExpr(init, exprFeatures, curCtx, functionContext)
                                            List(transformRecursive(Opt(ft,
                                                InitDeclaratorI(decl, attributes, Some(condExpr))), newCtx, false, functionContext))
                                        }
                                    } else {
                                        List(transformRecursive(o, newCtx, false, functionContext))
                                    }
                                case DeclarationStatement(decl: Declaration) =>
                                    val result = handleDeclarations(Opt(ft, decl), curCtx, isTopLevel)
                                        .map(x => Opt(trueF, DeclarationStatement(x.entry)))
                                    countDuplications(decl, result.size, isTopLevel)
                                    result
                                case _: Declaration =>
                                    val result = handleDeclarations(o.asInstanceOf[Opt[Declaration]], curCtx, isTopLevel)
                                    countDuplications(o.entry, result.size, isTopLevel)
                                    result
                                case i@Initializer(elem, expr) =>
                                    if (fExps.isEmpty) {
                                        List(replaceAndTransform(Opt(trueF, Initializer(elem, expr)), newCtx, isTopLevel, functionContext))
                                    } else {
                                        List(Opt(trueF, convertToCondExpr(i, fExps, curCtx, functionContext)))
                                    }
                                case e: Enumerator =>
                                    if (!fExps.isEmpty) {
                                        countDuplications(o.entry, fExps.size, isTopLevel)
                                        fExps.map(x => Opt(trueF, transformRecursive(convertEnumId(replaceOptAndId(e, x, functionContext), x), x, false, functionContext)))
                                    } else if (ft.equals(trueF)) {
                                        List(transformRecursive(o, curCtx, false, functionContext))
                                    } else {
                                        List(Opt(trueF,
                                            transformRecursive(convertEnumId(replaceOptAndId(e, o.feature, functionContext), o.feature),
                                                newCtx, false, functionContext)))
                                    }
                                /*case e@Enumerator(_, Some(expr)) =>
                                    val exprFeatures = computeFExpsForDuplication(expr, o.feature.and(curCtx))
                                    if (!fExps.isEmpty) {
                                        countDuplications(o.entry, fExps.size, isTopLevel)
                                        fExps.map(x => Opt(trueF,
                                            transformRecursive(convertEnumId(replaceOptAndId(
                                                convertToCondExpr(e, exprFeatures, o.feature.and(curCtx)), x), x), x)))
                                    } else if (ft.equals(trueF)) {
                                        List(transformRecursive(Opt(trueF,
                                            convertToCondExpr(e, exprFeatures, o.feature.and(curCtx))), curCtx))
                                    } else {
                                        List(Opt(trueF,
                                            transformRecursive(convertEnumId(replaceOptAndId(
                                                convertToCondExpr(e, exprFeatures, o.feature.and(curCtx)),
                                                o.feature), o.feature), o.feature.and(curCtx))))
                                    }*/
                                case sd@StructDeclaration(qual, decl) =>
                                    if (!fExps.isEmpty) {
                                        countDuplications(entry, fExps.size, isTopLevel)
                                        fExps.map(x => replaceAndTransform(Opt(trueF, StructDeclaration(qual, convertStructId(decl, x))), x, isTopLevel, functionContext))
                                    } else if (ft.equals(trueF)) {
                                        List(transformRecursive(o, curCtx, false, functionContext))
                                    } else {
                                        List(replaceOptAndId(Opt(trueF,
                                            StructDeclaration(qual, convertStructId(decl, ft))), ft, functionContext))
                                    }
                                case _: FunctionDef | _: NestedFunctionDef =>
                                    val result = handleFunction(o)
                                    countDuplications(o.entry, result.size, isTopLevel)
                                    result
                                case _: IfStatement | _: WhileStatement | _: SwitchStatement | _: DoStatement | _: ForStatement =>
                                    val result = handleStatement(o, newCtx, functionContext)
                                    countDuplications(o.entry, result.size, isTopLevel)
                                    result
                                case r: ReturnStatement =>
                                    if (!fExps.isEmpty) {
                                        fExps.map(x => Opt(trueF, statementToIf(replaceAndTransform(r, x, isTopLevel, functionContext), x, curCtx, functionContext)))
                                    } else {
                                        if (ft.equals(trueF)) {
                                            List(transformRecursive(o, curCtx, false, functionContext))
                                        } else {
                                            List(Opt(trueF, statementToIf(replaceAndTransform(r, newCtx, isTopLevel, functionContext), newCtx, curCtx, functionContext)))
                                        }
                                    }
                                case g: GotoStatement =>
                                    if (!fExps.isEmpty) {
                                        fExps.map(x => Opt(trueF, statementToIf(replaceOptAndId(g, x, functionContext), ft.and(x), curCtx, functionContext)))
                                    } else if (ft.equals(trueF)) {
                                        List(transformRecursive(o, curCtx, false, functionContext))
                                    } else {
                                        List(Opt(trueF, statementToIf(replaceOptAndId(g, ft, functionContext), ft, curCtx, functionContext)))
                                    }
                                case e: ExprStatement =>
                                    val features = computeFExpsForDuplication(e, newCtx)
                                    if (!features.isEmpty) {
                                        countDuplications(entry, features.size, isTopLevel)
                                        features.map(x => Opt(trueF,
                                            IfStatement(One(toCExpr(fExprDiff(curCtx, x.and(newCtx)))),
                                                One(CompoundStatement(List(replaceAndTransform(Opt(trueF, e), x.and(newCtx), isTopLevel, functionContext)))),
                                                List(),
                                                None)))
                                    } else if (ft.equals(trueF)) {
                                        List(transformRecursive(o, curCtx, false, functionContext))
                                    } else {
                                        val result = List(Opt(trueF,
                                            IfStatement(One(toCExpr(newCtx)),
                                                One(CompoundStatement(List(replaceAndTransform(Opt(trueF, e), newCtx, isTopLevel, functionContext)))),
                                                List(),
                                                None)))
                                        result
                                    }
                                case LabelStatement(i: Id, attr) =>
                                    if (defuse.containsKey(i) && !ft.equivalentTo(trueF)) {
                                        addIdUsages(i, ft.and(curCtx))
                                        List(Opt(trueF, LabelStatement(prependCtxPrefix(i, newCtx), attr)))
                                    } else {
                                        List(o)
                                    }

                                /*val features = computeFExpsForDuplication(label, ft.and(curCtx))
                                if (!features.isEmpty) {
                                    features.map(x => Opt(trueF, statementToIf(replaceOptAndId(label, x), ft, curCtx)))
                                } else if (ft.equals(trueF)) {
                                    List(o)
                                } else {
                                    List(Opt(trueF, statementToIf(replaceOptAndId(label, ft), ft, curCtx)))
                                }*/
                                // TODO fgarbe: Transformation required!
                                case _: TypelessDeclaration => List(o)
                                // We do not support transformations of pragmas! We still remove their variability though.
                                case p@Pragma(_) =>
                                    List(Opt(trueF, removeOptFeatures(p)))
                                case _: EmptyStatement => List()
                                case _: EmptyExternalDef => List()
                                case cs: CompoundStatement =>
                                    if (ft.equivalentTo(trueF) || ft.equivalentTo(curCtx)) {
                                        List(Opt(trueF, transformRecursive(cs, newCtx, false, functionContext)))
                                    } else {
                                        List(Opt(trueF,
                                            IfStatement(
                                                One(toCExpr(fExprDiff(curCtx, newCtx))),
                                                One(replaceAndTransform(cs, newCtx, isTopLevel, functionContext)),
                                                List(),
                                                None)))
                                    }
                                case _ => List(transformRecursive(o, newCtx, false, functionContext))
                            }
                        }
                    case k => List(transformRecursive(k, curCtx, false, functionContext))
                }
        })
        r(t).getOrElse(t).asInstanceOf[T]
    }

    /**
     * Returns the and combination of given optional feature node and context.
     */
    def getFeatureForContext(feature: FeatureExpr, context: FeatureExpr): FeatureExpr = {
        if (feature.implies(context).isTautology(fm)) {
            feature
        } else {
            feature.and(context)
        }
        /*feature.and(context)*/
    }

    def prepareASTforIfdef[T <: Product](t: T, currentContext: FeatureExpr = trueF): T = {
        val r = alltd(rule {
            case l: List[Opt[_]] =>
                l.flatMap(x => x match {
                    case o@Opt(ft: FeatureExpr, entry) =>
                        if (ft.mex(currentContext).isTautology()) {
                            List()
                        } else if (ft.implies(currentContext).isTautology()) {
                            List(prepareASTforIfdef(o, ft))
                        } else {
                            List(prepareASTforIfdef(Opt(ft.and(currentContext), entry), ft.and(currentContext)))
                        }
                })
            case o@One(st: Statement) =>
                st match {
                    case cs: CompoundStatement =>
                        One(prepareASTforIfdef(st, currentContext))
                    case k =>
                        One(CompoundStatement(List(Opt(trueF, prepareASTforIfdef(k, currentContext)))))
                }
        })
        r(t) match {
            case None =>
                t
            case k =>
                k.get.asInstanceOf[T]
        }
    }

    /**
     * Computes the cartesian product of a list of lists of FeatureExpressions using the boolean 'and' operator.
     * Ex: List( List(a, b), List(c, d, e)) becomes List(a&c, a&d, a&e, b&c, b&d, b&e).
     */
    def computeCarthesianProduct(list: List[List[FeatureExpr]], context: FeatureExpr, a: Any = List()): List[FeatureExpr] = {
        def computeCarthesianProductHelper(listOfLists: List[List[FeatureExpr]], currentContext: FeatureExpr): List[FeatureExpr] = {
            listOfLists match {
                case Nil =>
                    List()
                case x :: Nil =>
                    x
                case x :: xs =>
                    xs.foldLeft(x)((first, second) => {
                        if (first.size > 1000) {
                            println("Currently computed " + first.size + " different features for element:\n" + a)
                        }
                        first.flatMap(y => {
                            second.flatMap(z => {
                                val andResult = z.and(y)
                                if (andResult.isSatisfiable() && andResult.isSatisfiable(fm)) {
                                    List(andResult)
                                } else {
                                    List()
                                }
                            })
                        })
                    })
            }
        }
        val preparedList = list.map(x => x.filterNot(y => y.equivalentTo(FeatureExprFactory.False) || y.equivalentTo(trueF) || y.equivalentTo(context))).filterNot(x => x.isEmpty)
        val distinctList = computeDistinctLists(preparedList)
        val potentialResultSize1 = distinctList.map(x => x.size.toLong).foldLeft(1.toLong)(_ * _)
        val distinctSingleFeatures = list.flatten.map(x => x.collectDistinctFeatureObjects).flatten.distinct
        val potentialResultSize2 = Math.pow(2, distinctSingleFeatures.size).toLong
        val potentialResultSize = Math.min(potentialResultSize1, potentialResultSize2)
        if (potentialResultSize > duplicationThreshold) {
            var errorMessage = ""
            if (a.isInstanceOf[AST]) {
                val currentElement = a.asInstanceOf[AST]
                if (currentElement.getPositionFrom.getLine.equals(-1))
                    errorMessage = "[Warning] list size exceeds computation threshold (potentially " + potentialResultSize + " variations) for element:\n" + PrettyPrinter.print(currentElement)
                else
                    errorMessage = "[Warning] list size exceeds computation threshold (potentially " + potentialResultSize + " variations) for element in line " + currentElement.getPositionFrom.getLine + ":\n" + PrettyPrinter.print(currentElement)
            } else {
                errorMessage = "[Warning] list size exceeds computation threshold (potentially " + potentialResultSize + " variations) for element:\n" + a
            }
            if (isFirstRun) {
                System.err.println(errorMessage)
                addToFile(skippedDuplicationsPath, currentFileName + " <-> " + errorMessage + "\n\n")
            }
            return List(FeatureExprFactory.False)
        }
        if (potentialResultSize2 < potentialResultSize1) {
            val result = getFeatureCombinationsFiltered(distinctSingleFeatures, context)
            result
        } else {
            val result = computeCarthesianProductHelper(preparedList, context)
            result
        }
        computeCarthesianProductHelper(preparedList, context)
    }

    /**
     * Checks whether given feature list contains exactly 1 False feature. Used to tag featuresForDuplications where
     * the number of features exceeds the variantThreshold.
     */
    def exceedsThreshold(lst: List[FeatureExpr]): Boolean = {
        lst.size >= 1 && lst.head.equals(FeatureExprFactory.False)
    }

    /**
     * Takes a list of lists and gives us a list of lists where each list is distinct to each other list.
     */
    def computeDistinctLists(listOfLists: List[List[FeatureExpr]]): List[List[FeatureExpr]] = {
        if (listOfLists.isEmpty) {
            List()
        } else if (listOfLists.size == 1) {
            listOfLists
        } else {
            var superList: List[FeatureExpr] = List()
            listOfLists.map(x => {
                val result = x.diff(superList)
                superList = (superList ++ x).distinct
                result
            }).filter(x => !x.isEmpty)
        }
    }

    /**
     * Returns a list of FeatureExpressions for the given AST Element a. This list contains FeatureExpressions that
     * require code duplications. Example: condition inside an IfStatement has a variable Identifier -> we have to create
     * two different IfStatements and the function returns these two distinct features.
     */
    def computeFExpsForDuplication(a: Any, curCtx: FeatureExpr, isTopLevel: Boolean = false): List[FeatureExpr] = {
        def computationHelper(a: Any, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
            val featureList = getNextOptFeatures(a, currentContext, isTopLevel).filterNot(x => x.equivalentTo(currentContext, fm)) ++ List(FeatureExprFactory.False)
            val identFeatureList = getNextIdFeatures(a, currentContext, isTopLevel)

            if (featureList.size == 1 && identFeatureList.isEmpty)
                return List()

            val featureBuffer: ListBuffer[List[FeatureExpr]] = ListBuffer()
            val currentFeatures: mutable.HashSet[FeatureExpr] = new mutable.HashSet

            // TODO fgarbe: May rewrite code using pattern matching!
            featureList.foldLeft(List(): List[FeatureExpr])((first, second) => {

                // Reached end of list
                if (second.equivalentTo(FeatureExprFactory.False)) {
                    if (!first.isEmpty) {
                        if (!currentFeatures.contains(first.head)) {
                            first.foreach(currentFeatures.add)
                            val or = first.foldLeft(FeatureExprFactory.False)(_ or _)
                            val remainingFeature = or.not().and(currentContext)
                            currentFeatures.add(remainingFeature)
                            featureBuffer += remainingFeature :: first
                        }
                    }
                    List()
                } else if (first.isEmpty) {
                    second :: first
                } else {
                    var result = true

                    // Change var result to reflect if all collected features mutually exclude each other
                    first.foldLeft(second)((a, b) => {
                        if (b.equivalentTo(FeatureExprFactory.False)) {
                            b
                        } else if (a.mex(b).isTautology(fm)) {
                            b
                        } else {
                            result = false
                            b
                        }
                    })
                    val orResult = first.foldLeft(second)(_ or _)
                    if (result && currentContext.implies(orResult).isTautology(fm)) {
                        // All collected features are mutually exclusive and the context implies the or result of all of them
                        featureBuffer += (second :: first)
                        List()
                    } else if (result) {
                        // Continue collecting mutually exclusive expressions
                        second :: first
                    } else {
                        first.foreach(x => currentFeatures.add(x))
                        val or = first.foldLeft(FeatureExprFactory.False)(_ or _)
                        val remainingFeature = or.not().and(currentContext)
                        currentFeatures.add(remainingFeature)
                        featureBuffer += remainingFeature :: first

                        if (second.equivalentTo(FeatureExprFactory.False)) {
                            if (!currentFeatures.contains(second)) {
                                currentFeatures += second
                                currentFeatures += second.or(currentContext.not()).not()
                                featureBuffer += List(second, second.or(currentContext.not()).not())
                            }
                        }
                        List(second)
                    }
                }
            })

            currentFeatures.clear()
            if (featureBuffer.isEmpty) {
                if (!identFeatureList.isEmpty) {
                    identFeatureList
                } else {
                    List()
                }
            } else if (featureBuffer.size == 1) {
                val firstResult = featureBuffer.toList.head
                val result = computeCarthesianProduct(List(firstResult, identFeatureList.diff(firstResult)), currentContext, a)
                result
            } else {
                val featureBufferList = featureBuffer.toList
                val optResult = computeCarthesianProduct(featureBufferList, currentContext, a)
                if (exceedsThreshold(optResult)) {
                    List(FeatureExprFactory.False)
                } else {
                    val result = computeCarthesianProduct(List(optResult, identFeatureList.diff(optResult)), currentContext, a)
                    result
                }
            }
        }
        def handleLists(listOfLists: List[List[FeatureExpr]], currentContext: FeatureExpr, a: Any = List()): List[FeatureExpr] = {
            val potentialResultSize1 = listOfLists.map(x => x.size).foldLeft(1)(_ * _)
            val distinctSingleFeatures = listOfLists.flatten.map(x => x.collectDistinctFeatureObjects).flatten.distinct
            val potentialResultSize2 = Math.pow(distinctSingleFeatures.size, 2).toInt
            val potentialResultSize = Math.min(potentialResultSize1, potentialResultSize2)
            if (listOfLists.exists(x => exceedsThreshold(x)) || potentialResultSize > duplicationThreshold) {
                List(FeatureExprFactory.False)
            } else {
                listOfLists match {
                    case Nil => Nil
                    case x :: Nil => x
                    case k => computeCarthesianProduct(k, currentContext, a)
                }
            }
        }
        a match {
            case o: Opt[_] =>
                if (curCtx.implies(o.feature).isTautology(fm)) {
                    computeFExpsForDuplication(o.entry, curCtx)
                } else {
                    val features = computeFExpsForDuplication(o.entry, o.feature)
                    if (features.isEmpty) {
                        List(o.feature)
                    } else {
                        features
                    }
                }
            case ws: WhileStatement =>
                computationHelper(ws.expr, curCtx)
            case fs: ForStatement =>
                val featureLists = List(computationHelper(fs.expr1, curCtx), computationHelper(fs.expr2, curCtx), computationHelper(fs.expr3, curCtx))
                val result = handleLists(featureLists, curCtx, a)
                result
            case is@IfStatement(One(statement), thenBranch, elif, els) =>
                computationHelper(statement, curCtx)
            case is@IfStatement(c: Choice[Product], thenBranch, elif, els) =>
                val choices = conditionalToList(c, curCtx)
                choices.flatMap(x => computationHelper(x._2, x._1)).distinct
            case ss@SwitchStatement(e, One(stmt: CompoundStatement)) =>
                computationHelper(ss.expr, curCtx)
            case es: ExprStatement =>
                computationHelper(es.expr, curCtx)
            case ds: DoStatement =>
                computationHelper(ds.expr, curCtx)
            case rs@ReturnStatement(Some(CompoundStatementExpr(CompoundStatement(stmts)))) =>
                computeCarthesianProduct(computeDistinctLists(stmts.map(x => computationHelper(x.entry, curCtx.and(x.feature)))), curCtx, a)
            case rs@ReturnStatement(Some(x)) =>
                computationHelper(x, curCtx)
            case gs: GotoStatement =>
                computationHelper(gs.target, curCtx)
            case fd: FunctionDef =>
                val specList = computationHelper(fd.specifiers, curCtx)
                val decList = computationHelper(fd.declarator, curCtx)
                val paramList = computationHelper(fd.oldStyleParameters, curCtx)
                val featureLists = List(specList, decList, paramList)
                val result = handleLists(featureLists, curCtx, a)
                result
            case d@Declaration(declSpecs, init) =>
                // Handled by handleDeclarations
                val featureLists = List(computationHelper(declSpecs, curCtx), computationHelper(init, curCtx))
                val result = handleLists(featureLists, curCtx, a)
                result
            case nfd: NestedFunctionDef =>
                val featureLists = List(computationHelper(nfd.specifiers, curCtx), computationHelper(nfd.declarator, curCtx), computationHelper(nfd.parameters, curCtx))
                val result = handleLists(featureLists, curCtx, a)
                result
            case Enumerator(id: Id, Some(expr: Expr)) =>
                val featureList = List(computationHelper(id, curCtx), computationHelper(expr, curCtx))
                val result = handleLists(featureList, curCtx, a)
                result
            case Enumerator(id: Id, _) =>
                computationHelper(id, curCtx)
            case k =>
                computationHelper(k, curCtx)
        }
    }

    /**
     * Returns the next relevant features, if no new relevant features are computed returns the current context.
     * Used in order to not lose any elements when using features.map(x => someStatement(x)).
     */
    def computeNextRelevantFeaturesUnempty(a: Any, currentContext: FeatureExpr = trueF, isTopLevel: Boolean = false): List[FeatureExpr] = {
        var result = computeFExpsForDuplication(a, currentContext, isTopLevel)
        if (result.isEmpty) {
            result = List(currentContext)
        }
        result
    }

    /**
     * Takes a look at the CaseStatements and CompoundStatements inside a SwitchStatement in order to determine
     * the list of FeatureExpressions needed for duplication.
     */
    // TODO fgarbe: Unused function!
    def computeCaseFeatures(cmpStmt: CompoundStatement, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
        /*def collectCaseStatements(compStmt: CompoundStatement, currentList: List[List[Opt[CaseStatement]]] = List(List())) : List[List[Opt[CaseStatement]]] = {
            val stmts = compStmt.innerStatements
            if (stmts.isEmpty){
                currentList
            } else if (stmts.head.entry.isInstanceOf[CaseStatement]) {
                collectCaseStatements(CompoundStatement(stmts.tail), ((stmts.head.asInstanceOf[Opt[CaseStatement]] :: currentList.head) :: currentList.tail))
            } else if (stmts.head.entry.isInstanceOf[CompoundStatement]) {
                collectCaseStatements(CompoundStatement(stmts.tail), (List() :: currentList))
            } else {
                currentList.drop(1)
            }
        }
        val caseStatements = cmpStmt.innerStatements.filter(x => x.entry.isInstanceOf[CaseStatement]).map(x => computeNextRelevantFeatures(x, currentContext))
        val defaultStatements = cmpStmt.innerStatements.filter(x => x.entry.isInstanceOf[DefaultStatement]).map(x => computeNextRelevantFeatures(x, currentContext))
        val totalStatements = (caseStatements ++ defaultStatements).filter(x => !x.isEmpty)
        computeCarthesianProduct(totalStatements)*/
        val caseFeatures = getFeatureCombinations(cmpStmt.innerStatements.map(x => {
            x.feature
        }).filter(x => !x.equivalentTo(trueF)).flatMap(x => x.collectDistinctFeatureObjects).distinct).filter(x => x.implies(currentContext).isTautology(fm))
        caseFeatures
    }

    /**
     * Creates all possible 2 power n combinations for a list of n raw (single) feature expressions. List(def(x64), def(x86))
     * becomes List(def(x64)&def(x86),!def(x64)&def(x86),def(x64)&!def(x86),!def(x64)&!def(x86).
     */
    private def getFeatureCombinations(fList: List[FeatureExpr]): List[FeatureExpr] = {
        if (fList.size == 0) {
            List()
        } else {
            fList.tail.foldLeft(List(fList.head, fList.head.not()))((curRes, curFExpr) => {
                curRes.flatMap(x => List(x.and(curFExpr), x.and(curFExpr.not())))
            })
        }
    }

    /**
     * Takes a look at the CompoundStatements and CaseStatements AS WELL as the expression inside the CaseStatements
     * in a SwitchStatement in order to determine the list of FeatureExpressions needed for duplication purposes.
     */
    def computeTotalCaseFeatures(cmpStmt: CompoundStatement, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
        val caseFeatures = getFeatureCombinations(cmpStmt.innerStatements.flatMap(x => {
            x.entry match {
                case cs: CaseStatement =>
                    val features = computeFExpsForDuplication(x, currentContext)
                    features
                case _ =>
                    if (x.feature.equivalentTo(trueF, fm)) {
                        List()
                    } else {
                        List(x.feature)
                    }
            }
        }).distinct.flatMap(x => x.collectDistinctFeatureObjects).distinct).filter(x => x.implies(currentContext).isTautology(fm))
        caseFeatures.filter(x => x.isSatisfiable(fm))
    }

    /**
     * Retrieves a list of feature expressions which represent the different variants according to the feature
     * expressions that are found inside the Opt nodes in given element a.
     * This also checks subelements of a unless they are new statements like for example an ExpressionStatement inside an IfStatement.
     */
    def getNextOptFeatures(a: Any, currentContext: FeatureExpr = trueF, isTopLevel: Boolean = false): List[FeatureExpr] = {
        def getOptFeature(a: Any, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
            a match {
                case Some(Initializer(initElemLabel, lc: LcurlyInitializer)) =>
                    initElemLabel.productIterator.toList.flatMap(getOptFeature(_, currentContext))
                    lc.productIterator.toList.flatMap(getOptFeature(_, currentContext))
                case Some(d: Initializer) =>
                    //println("Stopping at: " + d)
                    if (isTopLevel) {
                        d.productIterator.toList.flatMap(getOptFeature(_, currentContext))
                    } else {
                        List()
                    }
                case d@Opt(ft, entry: Statement) =>
                    //println("Stopping at: " + d)
                    List()
                case d@Opt(ft, entry@Initializer(initElemLabel, expr)) =>
                    //println("Stopping at: " + d)
                    val realFeature = getFeatureForContext(ft, currentContext)
                    if (ft.equivalentTo(trueF) || ft.equivalentTo(FeatureExprFactory.False) || realFeature.equivalentTo(currentContext)) {
                        getOptFeature(entry, currentContext)
                    } else {
                        List(realFeature) ++ entry.productIterator.toList.flatMap(getOptFeature(_, realFeature))
                    }
                case d@Opt(ft, entry: Declaration) =>
                    //println("Stopping at: " + d)
                    List()
                case d@Opt(ft, entry: StructDeclaration) =>
                    //println("Stopping at: " + d)
                    List()
                case d@Opt(ft, entry: Enumerator) =>
                    //println("Stopping at: " + d)
                    List()
                case d@Opt(ft, entry: FunctionDef) =>
                    //println("Stopping at: " + d)
                    List()
                case d@Opt(ft, entry: Product) =>
                    if (ft.equivalentTo(trueF) || ft.equivalentTo(FeatureExprFactory.False)) entry.productIterator.toList.flatMap(getOptFeature(_, currentContext)) else List(getFeatureForContext(ft, currentContext)) ++ entry.productIterator.toList.flatMap(getOptFeature(_, getFeatureForContext(ft, currentContext)))
                case d@Opt(ft, entry) =>
                    if (!(ft.equivalentTo(trueF) || ft.equivalentTo(FeatureExprFactory.False))) List(getFeatureForContext(ft, currentContext)) else List()
                case l: List[_] =>
                    l.flatMap(getOptFeature(_, currentContext))
                case p: Product =>
                    p.productIterator.toList.flatMap(getOptFeature(_, currentContext))
                case k =>
                    //println("Stopping at: " + k)
                    List()
            }
        }
        getOptFeature(a, currentContext).distinct
    }

    /**
     * Retrieves a list of feature expressions which represent the different variants according to the feature
     * expressions that are found for identifiers within given element a. This also checks subelements of a unless
     * they are new statements like for example an ExpressionStatement inside an IfStatement.
     */
    def getNextIdFeatures(a: Any, currentContext: FeatureExpr = trueF, isTopLevel: Boolean = false): List[FeatureExpr] = {
        def getVariableIds(a: Any, currentContext: FeatureExpr = trueF): List[Id] = {
            a match {
                case d@Initializer(initElemLabel, lc: LcurlyInitializer) =>
                    initElemLabel.productIterator.toList.flatMap(getVariableIds(_, currentContext))
                    lc.productIterator.toList.flatMap(getVariableIds(_, currentContext))
                case d: Initializer =>
                    //println("Stopping at: " + d)
                    if (isTopLevel) {
                        d.productIterator.toList.flatMap(getVariableIds(_, currentContext))
                    } else {
                        List()
                    }
                case d@Opt(ft, entry: Initializer) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, entry: Enumerator) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, entry: Statement) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, entry: Declaration) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, entry: StructDeclaration) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, entry: FunctionDef) =>
                    //println("IdFeat canceled at: " + entry)
                    List()
                case d@Opt(ft, i: Id) =>
                    if (idsToBeReplaced.containsKey(i)) {
                        List(i)
                    } else {
                        //println("IdFeat canceled at: " + i)
                        List()
                    }
                case d@Opt(ft, entry: Product) =>
                    entry.productIterator.toList.flatMap(getVariableIds(_, currentContext))
                case i: Id =>
                    if (idsToBeReplaced.containsKey(i)) {
                        List(i)
                    } else {
                        //println("IdFeat canceled at: " + i)
                        List()
                    }
                case p: Product =>
                    p.productIterator.toList.flatMap(getVariableIds(_, currentContext))
                case k =>
                    //println("IdFeat canceled at: " + k)
                    List()
            }
        }
        val ids = getVariableIds(a, currentContext)
        val listOfLists = ids.map(x => idsToBeReplaced.get(x).toList.map(y => y.and(currentContext)).filterNot(x => x.equivalentTo(FeatureExprFactory.False)))
        val result = computeCarthesianProduct(listOfLists, currentContext, a).filter(z => z.isSatisfiable(fm) && !z.equivalentTo(trueF))
        if (exceedsThreshold(result)) {
            return List()
        }
        result
    }

    /**
     * Transforms given Statement e into an IfStatement with ifdeftoif condition according to given FeatureExpr ft.
     * Ex: 3 + 1; -> if (id2i.ft) { 3 + 1; }
     */
    def statementToIf(e: Statement, ft: FeatureExpr, currentContext: FeatureExpr, functionContext: FeatureExpr): IfStatement = {
        IfStatement(One(toCExpr(fExprDiff(currentContext, ft))), One(CompoundStatement(List(Opt(trueF, replaceOptAndId(e, ft, functionContext))))), List(), None)
    }

    /**
     * Converts a non CompoundStatement into a CompoundStatement.
     * Ex: 3 + 1; -> { 3 + 1; }
     */
    def convertStatementToCompound(stmt: Statement): CompoundStatement = {
        stmt match {
            case cs: CompoundStatement =>
                cs
            case k =>
                CompoundStatement(List(Opt(trueF, stmt)))
        }
    }

    /**
     * Calls the proper function to transform a statement st depending on the type of st.
     */
    def handleStatement(opt: Opt[_], currentContext: FeatureExpr, functionContext: FeatureExpr): List[Opt[_]] = {
        opt.entry match {
            case i: IfStatement =>
                //handleIfStatementConditional(opt, currentContext)
                handleIfStatement(opt, currentContext, functionContext)
            case f: ForStatement =>
                handleForStatement(opt.asInstanceOf[Opt[Statement]], currentContext, functionContext)
            case w: WhileStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext, functionContext)
            case d: DoStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext, functionContext)
            case s: SwitchStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext, functionContext)

            case k =>
                List()
        }
    }

    /**
     * Handles IfStatements in different steps (utilizing conditional expressions):
     * 1. Transform optional IfStatements
     * 2. Transform conditionals in the if-condition and thenBranch
     * 3. Transform usual if-statement (possible variable ID definition in the condition!) recursive call for thenBranch
     * 4. Transform ElifStatements
     */
    def handleIfStatementConditional(optIf: Opt[_], currentContext: FeatureExpr, functionContext: FeatureExpr): List[Opt[_]] = {

        // 1. Step
        if (!optIf.feature.equivalentTo(trueF)) {
            optIf.entry match {
                case IfStatement(cond, thenBranch, elifs, elseBranch) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, optIf.feature))), One(CompoundStatement(handleIfStatementConditional(replaceOptAndId(optIf, optIf.feature, functionContext), optIf.feature, functionContext).asInstanceOf[List[Opt[Statement]]])), List(), None)))
                case _ =>
                    List()
            }
        } else {
            optIf.entry match {

                // 2. Step with conditionalExpressions
                case i@IfStatement(c: Conditional[Expr], thenBranch: Conditional[Statement], elif, els) =>
                    var newCond: Expr = null
                    val statementTuple = conditionalToList(thenBranch, currentContext)
                    var elseTuple = List((trueF, None.asInstanceOf[Option[Conditional[Statement]]]))
                    els match {
                        case None =>
                        case s@Some(One(stmt)) => elseTuple = List((trueF, s))
                        case Some(c: Choice[Statement]) =>
                            elseTuple = conditionalToList(c, currentContext).map(x => (x._1, Some(One(x._2))))
                    }
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val elsFeatures = elseTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(stmtFeatures, elsFeatures), currentContext, optIf.entry)
                    if (exceedsThreshold(carthProduct)) {
                        return List(transformPossibleIdentifiers(optIf, currentContext))
                    }
                    carthProduct match {
                        case Nil =>
                            newCond = conditionalToConditionalExpr(c, currentContext, functionContext, true).value
                            val elsBranch = elseTuple.find(e => currentContext.implies(e._1).isTautology(fm)).get._2
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            List(Opt(trueF, IfStatement(One(newCond), replaceAndTransform(stmt, currentContext, false, functionContext), elif.flatMap(y => handleIfStatementConditional(y, currentContext, functionContext).asInstanceOf[List[Opt[ElifStatement]]]), transformRecursive(elsBranch, currentContext, false, functionContext))))
                        case k =>
                            carthProduct.flatMap(x => {

                                // Narrow down if condition to the current context
                                newCond = conditionalToConditionalExpr(c, x, functionContext, true).value
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                val elsBranch = elseTuple.find(e => x.implies(e._1).isTautology(fm)).getOrElse((currentContext, None.asInstanceOf[Option[Conditional[Statement]]]))._2
                                List(Opt(trueF, IfStatement(One(newCond), replaceAndTransform(stmt, x, false, functionContext), replaceOptAndId(elif.flatMap(y => handleIfStatementConditional(y, x, functionContext).asInstanceOf[List[Opt[ElifStatement]]]), x, functionContext), replaceAndTransform(elsBranch, x, false, functionContext))))
                            })

                    }

                // 4. Step: ElifStatements
                case elif@ElifStatement(One(e: Expr), thenBranch: One[Statement]) =>
                    val exprFeatures = computeFExpsForDuplication(e, currentContext)
                    val newCond = convertToCondExpr(e, exprFeatures, currentContext, functionContext)
                    List(Opt(trueF, ElifStatement(One(newCond), transformRecursive(thenBranch, currentContext, false, functionContext))))
                case elif@ElifStatement(c: Conditional[Expr], thenBranch) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    val statementTuple = conditionalToList(thenBranch, currentContext)
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(condFeatures, stmtFeatures), currentContext, elif)
                    if (exceedsThreshold(carthProduct)) {
                        return List(transformPossibleIdentifiers(optIf, currentContext))
                    }
                    carthProduct match {
                        case Nil =>
                            val cond = conditionalTuple.find(y => currentContext.implies(y._1).isTautology(fm)).get._2
                            val exprFeatures = computeNextRelevantFeaturesUnempty(cond, currentContext)
                            val newCond = convertToCondExpr(cond, exprFeatures, currentContext, functionContext)
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            List(Opt(trueF, ElifStatement(One(replaceOptAndId(newCond, currentContext, functionContext)), replaceAndTransform(stmt, currentContext, false, functionContext))))
                        case k =>
                            carthProduct.map(x => {
                                val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                                val exprFeatures = computeNextRelevantFeaturesUnempty(cond, x)
                                val newCond = convertToCondExpr(cond, exprFeatures, currentContext, functionContext)
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                Opt(trueF, ElifStatement(One(replaceOptAndId(newCond, x, functionContext)), replaceAndTransform(stmt, x, false, functionContext)))
                            })
                    }
            }
        }
    }

    /**
     * Handles IfStatements in different steps:
     * 1. Transform optional IfStatements
     * 2. Transform conditionals in the if-condition and thenBranch
     * 3. Transform usual if-statement (possible variable ID definition in the condition!) recursive call for thenBranch
     * 4. Transform ElifStatements
     */
    // TODO fgarbe: Unused function?
    def handleIfStatement(optIf: Opt[_], currentContext: FeatureExpr, functionContext: FeatureExpr): List[Opt[_]] = {

        // 1. Step
        if (!optIf.feature.equivalentTo(trueF)) {
            optIf.entry match {
                case IfStatement(cond, thenBranch, elifs, elseBranch) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, optIf.feature))), One(CompoundStatement(handleIfStatement(replaceOptAndId(optIf, optIf.feature, functionContext), optIf.feature, functionContext).asInstanceOf[List[Opt[Statement]]])), List(), None)))
                case _ =>
                    List()
            }
        } else {
            optIf.entry match {

                // 3. Step
                case i@IfStatement(One(expr), One(stmt), elif, els@None) =>
                    val features = computeFExpsForDuplication(expr, currentContext)
                    if (features.isEmpty) {
                        List(Opt(trueF, IfStatement(One(replaceOptAndId(expr, currentContext, functionContext)), One(transformRecursive(stmt, currentContext, false, functionContext)), elif.flatMap(x => handleIfStatement(replaceOptAndId(x, currentContext, functionContext), currentContext, functionContext)).asInstanceOf[List[Opt[ElifStatement]]], transformRecursive(replaceOptAndId(els, currentContext, functionContext), currentContext, false, functionContext))))
                    } else {
                        features.flatMap(x => List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, x))), One(CompoundStatement(List(Opt(trueF, IfStatement(
                            One(replaceOptAndId(expr, x, functionContext)),
                            transformRecursive(replaceOptAndId(One(convertStatementToCompound(stmt)), x, functionContext), x, false, functionContext),
                            elif.flatMap(y => handleIfStatement(replaceOptAndId(y, x, functionContext), x, functionContext)).asInstanceOf[List[Opt[ElifStatement]]],
                            transformRecursive(replaceOptAndId(els, x, functionContext), x, false, functionContext)))))), List(), None))))
                    }

                // alternative 3. Step with elseBranch
                case i@IfStatement(One(expr), One(stmt), elif, els@Some(One(elseStmt))) =>
                    val features = computeFExpsForDuplication(expr, currentContext)
                    if (features.isEmpty) {
                        List(Opt(trueF, IfStatement(One(replaceOptAndId(expr, currentContext, functionContext)), One(transformRecursive(stmt, currentContext, false, functionContext)), elif.flatMap(x => handleIfStatement(replaceOptAndId(x, currentContext, functionContext), currentContext, functionContext)).asInstanceOf[List[Opt[ElifStatement]]], transformRecursive(replaceOptAndId(els, currentContext, functionContext), currentContext, false, functionContext))))
                    } else {
                        features.flatMap(x => List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, x))), One(CompoundStatement(List(Opt(trueF, IfStatement(
                            One(replaceOptAndId(expr, x, functionContext)),
                                transformRecursive(replaceOptAndId(One(convertStatementToCompound(stmt)), x, functionContext), x, false, functionContext),
                                elif.flatMap(y => handleIfStatement(replaceOptAndId(y, x, functionContext), x, functionContext)).asInstanceOf[List[Opt[ElifStatement]]],
                            transformRecursive(replaceOptAndId(els, x, functionContext), x, false, functionContext)))))), List(), None))))
                    }

                // 2. Step
                case ifs@IfStatement(c: Conditional[Expr], thenBranch: Conditional[Statement], elif, els) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    val statementTuple = conditionalToList(thenBranch, currentContext)
                    var elseTuple = List((trueF, None.asInstanceOf[Option[Conditional[Statement]]]))
                    els match {
                        case None =>
                        case s@Some(One(stmt)) =>
                            elseTuple = List((trueF, s))
                        case Some(c: Choice[Statement]) =>
                            elseTuple = conditionalToList(c, currentContext).map(x => (x._1, Some(One(x._2))))
                    }
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val elsFeatures = elseTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(condFeatures, stmtFeatures, elsFeatures), currentContext, ifs)
                    if (exceedsThreshold(carthProduct)) {
                        return List(transformPossibleIdentifiers(optIf, currentContext))
                    }
                    carthProduct.flatMap(x => {
                        val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                        val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                        val elsBranch = elseTuple.find(e => x.implies(e._1).isTautology(fm)).getOrElse((currentContext, None.asInstanceOf[Option[Conditional[Statement]]]))._2
                        List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, x))), One(CompoundStatement(
                            handleIfStatement(Opt(trueF, IfStatement(
                                One(replaceOptAndId(cond, x, functionContext)),
                            replaceAndTransform(stmt, x, false, functionContext),
                            replaceOptAndId(elif.flatMap(y => handleIfStatement(y, x, functionContext).asInstanceOf[List[Opt[ElifStatement]]]), x, functionContext),
                                replaceAndTransform(elsBranch, x, false, functionContext))), x, functionContext).asInstanceOf[List[Opt[Statement]]])), List(), None)))
                    })

                // 4. Step
                case elifs@ElifStatement(c: Conditional[Expr], thenBranch) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    if (conditionalTuple.isEmpty) {
                      /* conditionalTuple might be List() at this point.
                        In this case, conditionalTuple.find(y => x.implies(y._1).isTautology(fm)) is None (find does not find an entry with this property) and None.get causes an exception.
                       */
                      return List(optIf)
                    }
                    val statementTuple = conditionalToList(thenBranch, currentContext)
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(stmtFeatures, condFeatures), currentContext, elifs)
                    if (exceedsThreshold(carthProduct)) {
                        return List(transformPossibleIdentifiers(optIf, currentContext))
                    }
                    carthProduct match {
                        case Nil =>
                            val cond = conditionalTuple.find(y => currentContext.implies(y._1).isTautology(fm)).get._2
                            val exprFeatures = computeNextRelevantFeaturesUnempty(cond, currentContext)
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            exprFeatures.map(x => Opt(trueF, ElifStatement(One(replaceOptAndId(cond, x, functionContext)), replaceAndTransform(stmt, x, false, functionContext))))
                        case k =>
                            carthProduct.flatMap(x => {
                                val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                                val exprFeatures = computeNextRelevantFeaturesUnempty(cond, x)
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                exprFeatures.map(y => Opt(trueF, ElifStatement(One(replaceOptAndId(cond, y, functionContext)), replaceAndTransform(stmt, y, false, functionContext))))
                            })
                    }
            }
        }
    }

    /**
     * Handles ForStatements in different steps:
     * 1. Transform optional ForStatement
     * 2. Transform conditionals in the body of the ForStatement
     * 3. Transform usual ForStatement by looking at variability in its expressions
     */
    def handleForStatement(opt: Opt[Statement], currentContext: FeatureExpr, functionContext: FeatureExpr): List[Opt[Statement]] = {

        // 1. Step
        if (!opt.feature.equivalentTo(trueF)) {
            opt.entry match {
                case ForStatement(expr1, expr2, expr3, cond) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleForStatement(replaceOptAndId(opt, opt.feature, functionContext), opt.feature.and(currentContext), functionContext))), List(), None)))
                case _ =>
                    List()
            }
        } else {
            opt.entry match {

                // 2. Step
                case ForStatement(expr1, expr2, expr3, c: Choice[Statement]) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, x._1))), One(CompoundStatement(handleForStatement(Opt(trueF, ForStatement(expr1, expr2, expr3, One(x._2))), x._1, functionContext))), List(), None)))

                // 3. Step
                case ForStatement(expr1, expr2, expr3, One(stmt: Statement)) =>
                    val features1 = computeFExpsForDuplication(expr1.getOrElse(EmptyStatement()), currentContext)
                    val newExpr1 = convertToCondExpr(expr1, features1, currentContext, functionContext)
                    val features2 = computeFExpsForDuplication(expr2.getOrElse(EmptyStatement()), currentContext)
                    val newExpr2 = convertToCondExpr(expr2, features2, currentContext, functionContext)
                    val features3 = computeFExpsForDuplication(expr3.getOrElse(EmptyStatement()), currentContext)
                    val newExpr3 = convertToCondExpr(expr3, features3, currentContext, functionContext)
                    List(Opt(trueF, ForStatement(newExpr1, newExpr2, newExpr3, One(transformRecursive(replaceOptAndId(stmt, currentContext, functionContext), currentContext, false, functionContext)))))
            }
        }
    }

    /**
     * Handles while / switch / do statements (they have a similar structure).
     */
    def handleWSDStatements(opt: Opt[Statement], currentContext: FeatureExpr, functionContext: FeatureExpr): List[Opt[Statement]] = {
        // 1. Step
        if (!opt.feature.equivalentTo(trueF)) {
            opt.entry match {
                case WhileStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature, functionContext), opt.feature, functionContext))), List(), None)))
                case SwitchStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature, functionContext), opt.feature, functionContext))), List(), None)))
                case DoStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature, functionContext), opt.feature, functionContext))), List(), None)))
                case _ =>
                    List()
            }
        } else {
            opt.entry match {

                // 3. Step
                case WhileStatement(expr, One(stmt: Statement)) =>
                    val features = computeFExpsForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, features, currentContext, functionContext)
                    List(Opt(trueF, WhileStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext, functionContext), currentContext, false, functionContext)))))
                case sw@SwitchStatement(expr, One(stmt: Statement)) =>
                    val exprFeatures = computeFExpsForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, exprFeatures, currentContext, functionContext)
                    // val caseFeatures = computeCaseFeatures(stmt.asInstanceOf[CompoundStatement], currentContext)
                    val caseFeatures = computeTotalCaseFeatures(stmt.asInstanceOf[CompoundStatement], currentContext)
                    if (exceedsThreshold(caseFeatures)) {
                        return List(transformPossibleIdentifiers(Opt(trueF, sw), currentContext))
                    }
                    if (caseFeatures.isEmpty) {
                        List(Opt(trueF, SwitchStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext, functionContext), currentContext, false, functionContext)))))
                    } else {
                        caseFeatures.map(x => Opt(trueF, IfStatement(One(toCExpr(fExprDiff(currentContext, x))), One(CompoundStatement(List(Opt(trueF, SwitchStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, x, functionContext), x))))))), List(), None)))
                    }
                case DoStatement(expr, One(stmt: Statement)) =>
                    val features = computeFExpsForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, features, currentContext, functionContext)
                    List(Opt(trueF, DoStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext, functionContext), currentContext, false, functionContext)))))

                // 2. Step
                case WhileStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(toCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                WhileStatement(
                                    expr,
                                    One(x._2))), x._1, functionContext))),
                            List(),
                            None)))
                case SwitchStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(toCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                SwitchStatement(
                                    expr,
                                    One(x._2))), x._1, functionContext))),
                            List(),
                            None)))
                case DoStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToList(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(toCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                DoStatement(
                                    expr,
                                    One(x._2))), x._1, functionContext))),
                            List(),
                            None)))

                case k =>
                    logger.error("Missed statement transformation: " + k)
                    List()
            }
        }
    }

    /**
     * Transforms given declaration. Transformation has different effects, declaration could be duplicated / renamed etc.
     */
    def handleDeclarations(optDeclaration: Opt[Declaration], curCtx: FeatureExpr = trueF, isTopLevel: Boolean = false, functionContext: FeatureExpr = trueF): List[Opt[Declaration]] = {
        optDeclaration.entry match {
            case Declaration(declSpecs, init) =>
                val declarationFeature = optDeclaration.feature
                val newDeclSpecs = declSpecs.map(x => if (optDeclaration.feature.equivalentTo(curCtx) && curCtx.implies(x.feature).isTautology(fm)) x
                else {
                    val relevantFeature = x.feature.and(declarationFeature)
                    x match {
                        case o@Opt(ft, EnumSpecifier(Some(i: Id), Some(enums))) =>
                            val newEnums = Some(enums.map(x => Opt(trueF, convertEnumId(x.entry, relevantFeature))))
                            if (defuse.containsKey(i)) {
                                addIdUsages(i, declarationFeature)
                                Opt(ft, EnumSpecifier(Some(prependCtxPrefix(i, declarationFeature)), newEnums))
                            } else {
                                Opt(ft, EnumSpecifier(Some(i), newEnums))
                            }
                        case o@Opt(ft, e@EnumSpecifier(None, Some(enums))) =>
                            val specifierFeature = ft.and(relevantFeature)
                            val result = Opt(trueF, replaceOptAndId(transformRecursive(e, specifierFeature, false, functionContext), specifierFeature, functionContext))
                            result
                        case o@Opt(ft, EnumSpecifier(Some(i: Id), k)) =>
                            if (defuse.containsKey(i)) {
                                addIdUsages(i, relevantFeature)
                                Opt(ft, EnumSpecifier(Some(prependCtxPrefix(i, relevantFeature)), k))
                            } else {
                                o
                            }
                        case o@Opt(ft, StructOrUnionSpecifier(a, Some(i: Id), b, c, d)) =>
                            if (init.isEmpty || defuse.containsKey(i)) {
                                addIdUsages(i, relevantFeature)
                                Opt(ft, StructOrUnionSpecifier(a, Some(prependCtxPrefix(i, relevantFeature)), b, c, d))
                            } else {
                                o
                            }

                        case k =>
                            k
                    }
                })

                var tmpDecl = Declaration(newDeclSpecs, init)
                val features = computeFExpsForDuplication(tmpDecl, declarationFeature.and(curCtx), isTopLevel)
                val isStruct = tmpDecl.declSpecs.exists(x => x.entry.isInstanceOf[StructOrUnionSpecifier] && x.entry.asInstanceOf[StructOrUnionSpecifier].enumerators.isDefined)
                val isStructWithInit = isStruct && tmpDecl.init.exists(x => x.entry.isInstanceOf[InitDeclaratorI] && x.entry.asInstanceOf[InitDeclaratorI].i.isDefined)
                val specifierFeatures = computeFExpsForDuplication(tmpDecl.declSpecs, declarationFeature.and(curCtx), isTopLevel)
                val specifierSingleFeatures = IfdeftoifUtils.getSingleFeaturesFromList(specifierFeatures)
                val initdeclSingleFeatures = IfdeftoifUtils.getSingleFeaturesFromList(computeFExpsForDuplication(tmpDecl.init, declarationFeature.and(curCtx), isTopLevel))
                if (isStructWithInit && !initdeclSingleFeatures.diff(specifierSingleFeatures).isEmpty) {
                    /* Split variable struct initializers from the declaration, e.g.
                     * struct point {int x; int y;} apoint[] =
                     * #ifdef A
                     * {1,2}
                     * #else
                     * {2,3}
                     * #endif
                     * ;
                     *
                     * --> ifdeftoif
                     * struct point {int x; int y;};
                     * _A_apoint[] = {1,2};
                     * _!A_apoint[] = {2,3};
                     */
                    val declarationWithoutInit = Declaration(newDeclSpecs, List())
                    var declarationWithoutInitResult: List[Opt[Declaration]] = List()
                    var declarationInitResult = List()
                    val newInitialization = Declaration(newDeclSpecs.map(x => x.entry match {
                        case StructOrUnionSpecifier(isUnion, Some(id@Id(name)), enumerators, attributesbefore, attributesafter) =>
                            val newId = Id(name)
                            //specifierFeatures.foreach(x => addIdUsages(newId, x))
                            if (defuse.containsKey(id)) {
                                defuse.iIdHashMap.put(id, newId :: defuse.iIdHashMap.get(id))
                            }
                            Opt(x.feature, StructOrUnionSpecifier(isUnion, Some(newId), None, attributesbefore, attributesafter))
                        case k =>
                            Opt(x.feature, k)
                    }
                    ), init)
                    if (!specifierFeatures.isEmpty) {
                        declarationWithoutInitResult = specifierFeatures.flatMap(x => handleDeclarations(Opt(trueF, replaceOptAndId(convertStructSpecifier(declarationWithoutInit, x), x, functionContext)), curCtx, isTopLevel))
                    } else {
                        declarationWithoutInitResult = handleDeclarations(Opt(trueF, declarationWithoutInit), curCtx, isTopLevel)
                    }
                    //val declarationInitResult = features.map(x => )
                    //init.foreach(x => )
                    return declarationWithoutInitResult ++ handleDeclarations(Opt(trueF, newInitialization), curCtx, isTopLevel)
                }
                if ((features.isEmpty) && isTopLevel && !curCtx.and(optDeclaration.feature).equivalentTo(trueF, fm)) {
                    if (isTopLevel && !curCtx.and(optDeclaration.feature).equivalentTo(trueF, fm)) {
                        if (declSpecs.exists(x => x.entry.isInstanceOf[TypedefSpecifier])) {
                            incOptionalTypedefs
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[StructOrUnionSpecifier])) {
                            incOptionalStructUnions
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[EnumSpecifier])) {
                            incOptionalEnums
                        } else {
                            // Function forward declarations
                            if (isFunctionForwardDeclaration(init)) {
                                incOptionalForwardFunctions()
                            } else {
                                incOptionalVariables
                            }
                        }
                    }
                }
                if (exceedsThreshold(features)) {
                    List(transformPossibleIdentifiers(Opt(trueF, tmpDecl), declarationFeature.and(curCtx)))
                } else if (!features.isEmpty) {
                    if (isStruct && !specifierFeatures.isEmpty) {
                        val result = features.map(x => Opt(trueF, transformRecursive(convertId(replaceOptAndId(convertStructSpecifier(tmpDecl, specifierFeatures.find(y => x.implies(y).isTautology()).getOrElse(trueF)), x, functionContext), x), x, false, functionContext)))
                        result
                    } else {
                        val result = features.map(x => Opt(trueF, transformRecursive(convertId(replaceOptAndId(tmpDecl, x, functionContext), x), x, false, functionContext)))
                        result
                    }
                } else {
                    val tmp = convertId(replaceOptAndId(tmpDecl, declarationFeature, functionContext), declarationFeature)
                    /*var tmp = replaceOptAndId(tmpDecl, declarationFeature)
                    / Skip renaming function forward declarations which are only optional
                    if (!isFunctionForwardDeclaration(init)) {
                        tmp = convertId(tmp, declarationFeature)
                    }*/
                    val result = List(Opt(trueF, transformRecursive(tmp, curCtx, isTopLevel, functionContext)))
                    result
                }
        }
    }

    /**
     * Determines if given InitDeclarators from a Declaration belong to a function forward declaration or not.
     * typedef unsigned long uoff_t; -> false
     * int main(int argc, char *+argv); -> true
     */
    def isFunctionForwardDeclaration(init: List[Opt[InitDeclarator]]): Boolean = {
        init.exists(x => x.entry.declarator.extensions.exists(y => y.entry.isInstanceOf[DeclIdentifierList] || y.entry.isInstanceOf[DeclParameterDeclList]))
    }

    /**
     * Determines whether given declaration is an extern declaration. This can be used to avoid renaming extern
     * function declarations.
     */
    def isExternDeclaration(optDeclaration: Opt[Declaration]): Boolean = {
        optDeclaration.entry.init.exists(x => x.entry.declarator.extensions.exists(y => y.entry.isInstanceOf[DeclIdentifierList] || y.entry.isInstanceOf[DeclParameterDeclList])) && optDeclaration.entry.declSpecs.exists(x => x.entry.isInstanceOf[ExternSpecifier])
    }

    /**
     * Temporary function, needs improvement and verification. Ignore for now.
     */
    def handleDeclarations_new(optDeclaration: Opt[Declaration], currentContext: FeatureExpr = trueF, functionContext: FeatureExpr = trueF): List[Opt[Declaration]] = {
        def convertSpecifiers(declSpecs: List[Opt[Specifier]], feat: FeatureExpr = trueF): List[Opt[Specifier]] = {
            if (!feat.equivalentTo(trueF)) {
                declSpecs.map(x => x match {
                    case o@Opt(ft, EnumSpecifier(Some(i: Id), k)) =>
                        if (defuse.containsKey(i)) {
                            addIdUsages(i, feat)
                            Opt(ft, EnumSpecifier(Some(prependCtxPrefix(i, feat)), k))
                        } else {
                            o
                        }
                    case o@Opt(ft, StructOrUnionSpecifier(a, Some(i: Id), b, c, d)) =>
                        if (defuse.containsKey(i)) {
                            addIdUsages(i, feat)
                            Opt(ft, StructOrUnionSpecifier(a, Some(prependCtxPrefix(i, feat)), b, c, d))
                        } else {
                            o
                        }

                    case k =>
                        k
                })
            } else {
                declSpecs
            }
        }

        var newOptDecl = optDeclaration
        var context = currentContext

        // 1. Step
        if (!optDeclaration.feature.equivalentTo(trueF)) {
            newOptDecl = replaceOptAndId(Opt(trueF, optDeclaration.entry), optDeclaration.feature, functionContext)
            context = optDeclaration.feature
        } else {
            context = trueF
        }

        // 2. Step
        val features = computeFExpsForDuplication(newOptDecl.entry, context)
        val specs = convertSpecifiers(newOptDecl.entry.declSpecs, context)
        val inits = newOptDecl.entry.init
        if (!features.isEmpty) {
            features.map(x => replaceOptAndId(Opt(trueF, transformRecursive(Declaration(convertSpecifiers(specs, x), transformDeclIds(inits, x)), x, false, functionContext)), x, functionContext))
        } else {
            List(replaceOptAndId(Opt(trueF, transformRecursive(Declaration(convertSpecifiers(specs, context), transformDeclIds(inits, context)), context)), context, functionContext))
        }
    }

    /**
     * Handles FunctionDefs in different steps:
     * 1. Transform optional function (#ifdef A \n void main() {..] \n #endif)
     * 2. Transform function by looking at variability in specifiers, declarators and parameters
     */
    def handleFunction(oFunction: Opt[_], context: FeatureExpr = trueF): List[Opt[_]] = {
        var newFunction = oFunction

        var functionName: String = ""
        if (oFunction.entry.isInstanceOf[FunctionDef])
            functionName = oFunction.entry.asInstanceOf[FunctionDef].getName
        if (oFunction.entry.isInstanceOf[NestedFunctionDef])
            functionName = oFunction.entry.asInstanceOf[NestedFunctionDef].getName

        // Insert a call to the ifdeftoif init function as first statement into the main function.
        if (isMainFunction(functionName)) {
            val initCall = Opt(trueF, ExprStatement(PostfixExpr(Id(initFunctionName), FunctionCall(ExprList(List())))))
            oFunction.entry match {
                case fd@FunctionDef(spec, decl, par, stmt) =>
                    newFunction = Opt(oFunction.feature, FunctionDef(spec, decl, par, CompoundStatement(initCall :: stmt.innerStatements)))
                case nfd@NestedFunctionDef(isAuto, spec, decl, par, stmt) =>
                    newFunction = Opt(oFunction.feature, NestedFunctionDef(isAuto, spec, decl, par, CompoundStatement(initCall :: stmt.innerStatements)))
            }
        }
        def handleFunctionRec(optFunction: Opt[_], currentContext: FeatureExpr = trueF): List[Opt[_]] = {
            // 1. Step
            optFunction.entry match {
                case fd@FunctionDef(spec, decl, par, stmt) =>
                    val features = computeFExpsForDuplication(optFunction, currentContext).filterNot(x => x.equals(trueF) || x.and(optFunction.feature).isContradiction(fm))
                    if (features.isEmpty) {
                        List(Opt(trueF, FunctionDef(
                            replaceOptAndId(spec, currentContext, currentContext),
                            replaceOptAndId(decl, currentContext, currentContext),
                            replaceOptAndId(par, currentContext, currentContext),
                            transformRecursive(replaceOptAndId(stmt, currentContext, currentContext), currentContext, false, currentContext))))
                    } else {
                        if (!isMainFunction(fd.getName) && !features.isEmpty) {
                            // rename functions
                            features.map(x => Opt(trueF, FunctionDef(
                                replaceOptAndId(spec, x, x),
                                replaceOptAndId(convertStructId(decl, x), x, x),
                                replaceOptAndId(par, x, x),
                                transformRecursive(replaceOptAndId(stmt, x, x), x, false, x))))
                        } else {
                            // don't rename functions if they are not variable [features.size == 0] or they are the main function
                            features.map(x => Opt(trueF, FunctionDef(replaceOptAndId(spec, x, x), replaceOptAndId(decl, x, x), replaceOptAndId(par, x, x), transformRecursive(replaceOptAndId(stmt, x, x), x, false, x))))
                        }
                    }
                case nfd@NestedFunctionDef(isAuto, spec, decl, par, stmt) =>
                    val features = computeFExpsForDuplication(nfd, currentContext).filterNot(x => x.and(optFunction.feature).isContradiction(fm))
                    if (features.isEmpty) {
                        List(Opt(trueF, NestedFunctionDef(isAuto, replaceOptAndId(spec, currentContext, currentContext), replaceOptAndId(convertStructId(decl, currentContext), currentContext, currentContext), replaceOptAndId(par, currentContext, currentContext), transformRecursive(replaceOptAndId(stmt, currentContext, currentContext), currentContext, false, currentContext))))
                    } else {
                        features.map(x => Opt(trueF, NestedFunctionDef(isAuto, replaceOptAndId(spec, x, x), replaceOptAndId(convertStructId(decl, x), x, x), replaceOptAndId(par, x, x), transformRecursive(replaceOptAndId(stmt, x, x), x, false, x))))
                    }
            }
        }
        handleFunctionRec(newFunction, context)
    }

    /**
     * Creates a single boolean expression for given .config file.
     */
    // TODO fgarbe: Unused function!
    def getConfigFormula(@SuppressWarnings(Array("unchecked")) file: File, fm: FeatureModel = FeatureExprFactory.empty,
                         features: Set[SingleFeatureExpr] = Set()): FeatureExpr = {
        val featuresFromConfig = ConfigurationHandling.getFeaturesFromConfiguration(file, fm, features)
        val trueFeatures = featuresFromConfig._1
        val falseFeatures = featuresFromConfig._2.map(x => x.not())
        var otherFeatures: List[FeatureExpr] = featuresFromConfig._3
        if (!defaultFeatureSelection) {
            otherFeatures = otherFeatures.map(x => x.not())
        }
        (trueFeatures ++ falseFeatures ++ otherFeatures).foldLeft(trueF)(_ and _)
    }

    /**
     * Checks if the given ast contains any opt or choice nodes which contain variability in the form of #ifdefs.
     */
    def hasVariableNodes(ast: AST): Boolean = {
        val r = manytd(query {
            case Opt(ft, _) if !ft.equals(trueF) => return true
            case Choice(ft, _, _) if !ft.equals(trueF) => return true
        })
        r(ast)
        false
    }

    /**
     * Converts a feature expression to a condition in C. #ifdef x64 becomes options.x64.
     */
    private def toCExpr(feature: FeatureExpr): Expr = feature match {
        case d: DefinedExternal => PostfixExpr(Id(featureStructInitializedName),
            PointerPostfixSuffix(".", Id(d.feature.toLowerCase)))
        case d: DefinedMacro => toCExpr(d.presenceCondition)
        case b: BDDFeatureExpr =>
            toCExpr(b,
                (fName: String) => PostfixExpr(Id(featureStructInitializedName),
                    PointerPostfixSuffix(".", Id(fName.toLowerCase)))
            )
        case a: And =>
            val l = a.clauses.toList
            NAryExpr(toCExpr(l.head), l.tail.map(x => Opt(trueF, NArySubExpr("&&", toCExpr(x)))))
        case o: Or =>
            val l = o.clauses.toList
            NAryExpr(toCExpr(l.head), l.tail.map(x => Opt(trueF, NArySubExpr("||", toCExpr(x)))))
        case Not(n) => UnaryOpExpr("!", toCExpr(n))
    }

    private def toCExpr(bdd: BDDFeatureExpr, transformFName: String => Expr): Expr = {
        if (bdd.isTautology(fm)) Constant("1")
        else if (bdd.isContradiction(fm)) Constant("0")
        else {
            def tClause(d: Array[(Byte, String)]): Expr =
                NAryExpr(tClauseHead(d.head), tClauseTail(d.tail))

            def tClauseTail(d: Array[(Byte, String)]): List[Opt[NArySubExpr]] =
                d.toList.map {
                    case (0, name) => Opt(trueF, NArySubExpr("&&", UnaryOpExpr("!", transformFName(name))))
                    case (_, name) => Opt(trueF, NArySubExpr("&&", transformFName(name)))
                }

            def tClauseHead(x: (Byte, String)): Expr = {
                x match {
                    case (0, name) => UnaryOpExpr("!", transformFName(name))
                    case (_, name) => transformFName(name)
                }
            }

            val cnfClauses: List[Expr] = bdd.getBddAllSat.map(tClause).toList

            // TODO fgarbe: Please specify what the following line does.
            NAryExpr(cnfClauses.head,
                cnfClauses.tail.foldLeft(List[Opt[NArySubExpr]]())((a, b: Expr) => a ++ List(Opt(trueF, NArySubExpr("||", b))))
            )
        }
    }

    /**
     * Serialize a Set of SingleFeatureExpressions.
     */
    private def serializeFeatureNames(sfeSet: Set[String], filename: String) {
        val fw = new ObjectOutputStream(new FileOutputStream(filename))
        fw.writeObject(sfeSet)
        fw.close()
    }

    /**
     * Returns the distinct difference between a feature expression pc and its enclosing context.
     * fExprDiff(A, A&B) => B
     */
    private def fExprDiff(context: FeatureExpr, pc: FeatureExpr): FeatureExpr = {
        if (context.equivalentTo(trueF) || context.equivalentTo(pc) || !pc.implies(context).isTautology(fm)) {
            pc
        } else {
            val result = pc.diff(context)
            result
        }
    }

    /**
     * Returns a set of all configuration options in a.
     */
    private def getSingleFeatures(a: Any): Set[SingleFeatureExpr] = {
        var featureSet: Set[FeatureExpr] = Set()
        val r = manytd(query {
            case Opt(ft, _) =>
                featureSet += ft
            case Choice(ft, _, _) =>
                featureSet += ft
        })

        r(a)
        featureSet.flatMap(x => x.collectDistinctFeatureObjects)
    }

    /**
     * Returns a set of all configuration options in a.
     */
    private def getSingleFeaturesFromList(lst: List[FeatureExpr]): Set[SingleFeatureExpr] = {
        var featureSet: Set[FeatureExpr] = lst.toSet
        featureSet.flatMap(x => x.collectDistinctFeatureObjects)
    }

    /**
     * Creates an #include directive for a header file of the "own" program, i.e., #include "path".
     * Does not create an include for system header files!
     */
    private def createIncludeDirective(path: String): String = {
        if (!path.isEmpty) {
            "#include \"" + path + "\"\n"
        } else {
            ""
        }
    }

    /**
     * Flatten the conditional tree structure and filter contradictory elements.
     */
    private def conditionalToList[T <: Product](choice: Conditional[T], curCtx: FeatureExpr = trueF): List[(FeatureExpr, T)] = {
        // TODO fgarbe: Possible code simplification!
        // ConditionalLib.items(choice, currentContext).filter(_._1.isSatisfiable(fm))

        val choiceList = choice.toList
        if (curCtx.equals(trueF)) {
            choiceList
        } else {
            //val result = choiceList.filter(x => x._1.implies(currentContext).isTautology)
            val result = choiceList.filterNot(x => x._1.and(curCtx).isContradiction(fm)).map(x => (x._1.and(curCtx), x._2))
            result
        }
    }

    /**
     * This method fills the IdMap which is used to map a feature expression to a number. This number is used for
     * for renaming identifiers e.g. #ifdef A int a #endif -> int _1_a     feature A is mapped to number 1.
     */
    // TODO fgarbe: Parameter a is unused!
    private def fillIdMap(a: Any) {
        if (presenceConditionNumberMap.size == 0) {
            presenceConditionNumberMap += (trueF -> presenceConditionNumberMap.size)
        }

        if (new File(path ++ "featureMap.csv").exists) {
            val featureMap = scala.io.Source.fromFile(path ++ "featureMap.csv").mkString.split("\n")
            if (!featureMap.isEmpty) {
                featureMap.foreach(x => {
                    val tuple = x.split(",")

                    val feature = new FeatureExprParser().parse(tuple.head)
                    val number = tuple.tail.head.toInt
                    presenceConditionNumberMap += (feature -> number)
                })
            }
        }
    }

    private def updateIdMap(feat: FeatureExpr) = {
        if (!presenceConditionNumberMap.contains(feat)) {
            presenceConditionNumberMap += (feat -> presenceConditionNumberMap.size)
        }
    }

    /**
     * Creates a prefix for identifiers from the presence condition under which they occur.
     * Format is _x_ where x is an Integer which represents the presence condition.
     */
    private def getPrefixFromIdMap(feat: FeatureExpr): String = {
        def getFromIdMap(feat: FeatureExpr): Int = {
            updateIdMap(feat)
            presenceConditionNumberMap.get(feat).get
        }
        "_" + getFromIdMap(feat) + "_"
    }

    /**
     * Retrieves the FeatureExpression which is mapped to the given number. Used for the second run of the
     * ifdeftoif transformation to retrieve the context of an already renamed identifier.
     */
    private def getFeatureForId(id: Int): Option[FeatureExpr] = {
        if (presenceConditionNumberMap.size < id || id < 0) {
            None
        } else {
            val it = presenceConditionNumberMap.iterator
            while (it.hasNext) {
                val next = it.next()
                if (next._2.equals(id)) {
                    return Some(next._1)
                }
            }
            None
        }
    }

    // Helper function that consolidates different functions for single Id transformation!
    private def transformId(i: Id, ft: FeatureExpr) = {
        addIdUsages(i, ft)
        replaceId.put(i, ft)
        updateIdMap(ft)
        prependCtxPrefix(i, ft)
    }

    /**
     * Renames identifiers inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    private def transformDeclIds[T <: Product](t: T, ft: FeatureExpr): T = {
        if (ft.equivalentTo(trueF, fm))
            return t

        val rt = alltd(rule {
            // do not transform the identifier of the main function
            case init@InitDeclaratorI(AtomicNamedDeclarator(_, Id(name), _), _, _) if isMainFunction(name) => init
            case InitDeclaratorI(AtomicNamedDeclarator(a, i: Id, b), attr, inits) =>
                val rId = transformId(i, ft)
                InitDeclaratorI(AtomicNamedDeclarator(a, prependCtxPrefix(i, ft), b), attr, inits)
            // do not transform the identifier of the main function
            case init@InitDeclaratorI(NestedNamedDeclarator(_, AtomicNamedDeclarator(_, Id(name), _), _, _), _, _) => init
            case InitDeclaratorI(NestedNamedDeclarator(l, AtomicNamedDeclarator(a, i: Id, b), r, c), attr, inits) =>
                val rId = transformId(i, ft)
                InitDeclaratorI(NestedNamedDeclarator(l, AtomicNamedDeclarator(a, rId, b), r, c), attr, inits)
        })

        rt(t).getOrElse(t).asInstanceOf[T]
    }

    /**
     * Renames the first identifier inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    private def convertId[T <: Product](current: T, feat: FeatureExpr): T = {
        def convert[T <: Product](t: T, ft: FeatureExpr): T = {
            t match {
                case Declaration(declSpecs, init) =>
                    Declaration(declSpecs, init.map(x => convertId(x, ft))).asInstanceOf[T]
                case Opt(optFt, InitDeclaratorI(decl, attri, iniz)) =>
                    Opt(optFt, InitDeclaratorI(convertId(decl, ft), attri, iniz)).asInstanceOf[T]
                case a@AtomicNamedDeclarator(pointers, i: Id, extensions) =>

                    // Don't rename forward function declarations of the main function
                    if (isMainFunction(i.name)) {
                        a.asInstanceOf[T]
                    } else {
                        addIdUsages(i, ft)
                        replaceId.put(i, ft)
                        updateIdMap(ft)
                        AtomicNamedDeclarator(pointers, prependCtxPrefix(i, ft), extensions).asInstanceOf[T]
                    }
                case NestedNamedDeclarator(pointers, nestedDecl, extensions, attrib) =>
                    NestedNamedDeclarator(pointers, convertId(nestedDecl, ft), extensions, attrib).asInstanceOf[T]
            }
        }
        if (feat.equivalentTo(trueF)) {
            current
        } else {
            convert(current, feat)
        }
    }

    /**
     * Renames struct specifiers inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    private def convertStructSpecifier[T <: Product](current: T, feat: FeatureExpr): T = {
        def convert[T <: Product](t: T, ft: FeatureExpr): T = {
            t match {
                case Declaration(declSpecs, init) =>
                    Declaration(declSpecs.map(x => x match {
                        case Opt(optFt, StructOrUnionSpecifier(isUnion, Some(id: Id), enumerators, attributesbefore, attributesafter)) =>
                            addIdUsages(id, ft)
                            Opt(optFt, StructOrUnionSpecifier(isUnion, Some(prependCtxPrefix(id, ft)), enumerators, attributesbefore, attributesafter))
                        case k =>
                            k
                    }), init).asInstanceOf[T]
                case l: List[Opt[Specifier]] =>
                    l.map(x => x match {
                        case Opt(optFt, StructOrUnionSpecifier(isUnion, Some(id: Id), enumerators, attributesbefore, attributesafter)) =>
                            addIdUsages(id, ft)
                            Opt(optFt, StructOrUnionSpecifier(isUnion, Some(prependCtxPrefix(id, ft)), enumerators, attributesbefore, attributesafter))
                        case k =>
                            k
                    }).asInstanceOf[T]
                case k =>
                    k
            }
        }
        if (feat.equivalentTo(trueF)) {
            current
        } else {
            convert(current, feat)
        }
    }

    /**
     * TODO fgarbe: Possible replacement for convertId (see above).
     * Renames the first identifier inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    private def transformDeclId[T <: Product](t: T, ft: FeatureExpr): T = {
        if (ft.equivalentTo(trueF))
            return t

        t match {
            case Declaration(declSpecs, init) =>
                Declaration(declSpecs, init.map(x => convertId(x, ft))).asInstanceOf[T]
            case Opt(optFt, InitDeclaratorI(decl, attri, iniz)) =>
                Opt(optFt, InitDeclaratorI(convertId(decl, ft), attri, iniz)).asInstanceOf[T]
            // Do not rename the identifier of the main function
            case a@AtomicNamedDeclarator(_, Id(name), _) if isMainFunction(name) => a.asInstanceOf[T]
            case AtomicNamedDeclarator(pointers, i: Id, extensions) =>
                val rId = transformId(i, ft)
                AtomicNamedDeclarator(pointers, rId, extensions).asInstanceOf[T]
            case NestedNamedDeclarator(pointers, nestedDecl, extensions, attrib) =>
                NestedNamedDeclarator(pointers, convertId(nestedDecl, ft), extensions, attrib).asInstanceOf[T]
            case FunctionDef(a, decl, b, c) =>
                FunctionDef(a, transformDeclId(decl, ft), b, c).asInstanceOf[T]
            case _ =>
                return t
        }
    }

    /**
     * Checks if given functionName describes the call to a 'main' C-function.
     * "_main" check for BusyBox files: in BusyBox ls_main is the main function of ls functionality, which is
     * statically linked into the BusyBox binary.
     */
    private def isMainFunction(functionName: String): Boolean = {
        functionName.equals("main") || functionName.equals(currentFileName + "_main")
    }

    /**
     * Renames identifiers inside of StructDeclarations by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    // TODO fgarbe: T is too general. Function is called with t: Declarator only!
    private def convertStructId[T <: Product](t: T, ft: FeatureExpr): T = {
        val r = oncetd(rule {
            case decl@AtomicNamedDeclarator(a, i: Id, b) =>
                // TODO fgarbe: Is the isMainFunction check here really necessary? Use transformId to simplify code!
                addIdUsages(i, ft)
                replaceId.put(i, ft)
                updateIdMap(ft)
                AtomicNamedDeclarator(a, prependCtxPrefix(i, ft), b)
        })

        if (ft.equivalentTo(trueF, fm))
            return t

        r(t).getOrElse(t).asInstanceOf[T]
    }

    /**
     * Renames Enumerators by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    private def convertEnumId(enu: Enumerator, ft: FeatureExpr): Enumerator = {
        if (ft.equivalentTo(trueF)) {
            enu
        } else {
            addIdUsages(enu.id, ft)
            updateIdMap(ft)
            Enumerator(prependCtxPrefix(enu.id, ft), enu.assignment)
        }
    }

    /**
     * Converts a given enumerator or initializer element. If they contain variability a new initializer/enumerator
     * element is returned where the variability is encoded in the form of a conditional expression.
     */
    // TODO: @fgarbe: The name of the function is not very specific.
    private def convertToCondExpr[T <: Product](current: T, variantFeatures: List[FeatureExpr], currentContext: FeatureExpr, functionContext: FeatureExpr): T = {
        def condExprHelper(expr: Expr, features: List[FeatureExpr]): Expr = {
            val innerMostExpr = replaceOptAndId(expr, features.head, functionContext)
            features.tail.foldLeft(innerMostExpr)((first, second) =>
                ConditionalExpr(toCExpr(fExprDiff(currentContext, second)), Some(replaceOptAndId(expr, second, functionContext)), first))
        }
        variantFeatures match {
            case Nil =>
                current
            case x :: Nil =>
                replaceOptAndId(current, x, functionContext)
            case x :: xs =>
                current match {
                    case Enumerator(id: Id, Some(expr: Expr)) =>
                        val cond = condExprHelper(expr, variantFeatures)
                        Enumerator(id, Some(cond)).asInstanceOf[T]
                    case Initializer(elemLabel, expr: Expr) =>
                        val cond = condExprHelper(expr, variantFeatures)
                        Initializer(elemLabel, cond).asInstanceOf[T]
                    case expr: Expr =>
                        condExprHelper(expr, variantFeatures).asInstanceOf[T]
                    case Some(expr: Expr) =>
                        Some(condExprHelper(expr, variantFeatures)).asInstanceOf[T]
                    case k =>
                        current
                }
        }
    }

    /**
     * Returns the new absolute file path for the resulting transformation file.
     */
    private def ifdeftoifFilePath(fName: String): String = {
        def fileNameWithoutExtension(outputStem: String): String = {
            val indexOfLastFolderSep = outputStem.lastIndexOf(File.separatorChar)
            val lastPathElement = outputStem.substring(indexOfLastFolderSep);
            val lastSepIndex = indexOfLastFolderSep + lastPathElement.lastIndexOf(".")
            if (lastSepIndex == -1) {
                outputStem
            } else {
                outputStem.substring(0, lastSepIndex)
            }
        }

        if ((new File(fName)).getName.contains(".")) // if the filename has a extension, remove it
            fileNameWithoutExtension(fName) + ifdeftoifFileSuffix
        else
            fName + ifdeftoifFileSuffix
    }

    /**
     * Removes duplicate features by checking for boolean equality according to the featureModel.
     * List(A&&B, B&&A) -> List(A&&B)
     */
    private def removeDuplicates(fExps: List[FeatureExpr], ctx: FeatureExpr): List[FeatureExpr] = {
        fExps match {
            case Nil =>
                List()
            case x :: Nil =>
                List(x)
            case x :: xs =>
                if (xs.exists(y => y.equivalentTo(x, fm.and(ctx)))) {
                    removeDuplicates(xs, ctx)
                } else {
                    x :: removeDuplicates(xs, ctx)
                }
        }
    }

    /**
     * Exports all renamings of identifiers into a renamings.txt file.
     * Layout:
     * thisIsAVariable -> _1_thisIsAVariable
     * thisIsAVariable -> _2_thisIsAVariable etc.
     */
    private def exportRenamings() = {
        if (!replaceId.isEmpty) {
            writeToFile("renamings_Functions.txt",
                replaceId.keySet().toArray.toList.map(x => {
                    val id = x.asInstanceOf[Id]
                    id.name + "@" + id.getPositionFrom.getLine + " -> " +
                        getPrefixFromIdMap(replaceId.get(x)) + id.name + " if " + replaceId.get(x).toString
                }).sorted mkString "\n")
        } else {
            ""
        }
        if (!idsToBeReplaced.isEmpty) {
            writeToFile("renamings_StructsAndVars.txt",
                idsToBeReplaced.keySet().toArray.toList.map(x => {
                    val id = x.asInstanceOf[Id]
                    id.name + " -> \n" +
                        idsToBeReplaced.get(x).map(fex =>
                            "\t" + getPrefixFromIdMap(fex) + id.name + " if " + fex.toString
                        ).mkString("\n")
                }) mkString "\n")
        } else {
            ""
        }
    }

    /**
     * This class describes three different fields for a context of an AST element.
     * @param context               The feature expression of the AST element
     * @param functionContext       The feature expression of the function in which the AST element is part of
     * @param isTopLevel            Is AST element a top level AST element
     */
    class IfdeftoifContext(context: FeatureExpr, functionContext: FeatureExpr, isTopLevel: Boolean) {
        var ctx = context
        var fctCtx = functionContext
        var isTop = isTopLevel

        def updateFunctionContext(newFctCtx: FeatureExpr): Unit = {
            fctCtx = newFctCtx
        }
    }
}
