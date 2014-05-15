package de.fosd.typechef.cifdeftoif

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

import java.util.{Collections, IdentityHashMap}
import java.util.regex.Pattern
import java.io._
import io.Source

import org.apache.logging.log4j.LogManager

import org.kiama.rewriting.Rewriter._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr._
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.featureexpr.sat._
import de.fosd.typechef.conditional._
import de.fosd.typechef.lexer.FeatureExprLib
import de.fosd.typechef.typesystem.{IdentityIdHashMap, CTypeSystemFrontend}

import de.fosd.typechef.error.TypeChefError


/**
 * The ifdef to if process transforms compile-time variability using preprocessor based #ifdefs into run-time
 * variability by using different techniques, mainly code duplications and renamings.
 *
 * If a function for example has two different return types (i.e. Int and Long) we duplicate the function and rename it
 * by adding an ifdeftoif name prefix. Additionally all usages of the function are renamed (and duplicated) accordingly.
 *
 * // Original source code
 * #ifdef X64
 * long
 * #else
 * int
 * #endif
 * main () { return 0; }
 *
 * // Ifdeftoif code
 * long _X64_main() { return 0; }
 * int _NotX64_main() { return 0; }
 */
class IfdefToIf extends ASTNavigation with ConditionalNavigation with IfdefToIfStatisticsInterface with IOUtilities {
    private lazy val logger = LogManager.getLogger(this.getClass.getName)
    private val trueF = FeatureExprFactory.True
    private val CONFIGPREFIX = "v_"
    private val nstoms = 1000000
    // Used for nanosecond to milisecond conversion
    private val fs = System.getProperty("file.separator")

    /* Variables used for the ifdeftoif transformation process */
    private var fm: FeatureModel = FeatureExprFactory.empty
    // The current name of the file to be transformed without extensions or file path
    private var currentFileName = ""
    // SingleFeatureExpressions found in the current AST
    private var features: Set[SingleFeatureExpr] = Set()
    // Default configuration flag, can either be '0' or '1'
    private val defaultConfigurationParameter = Constant("0")
    // Default feature selection state for features which do not appear in the .config file, false = off, true = on
    private val defaultFeatureSelection = false
    private var isFirstRun = true

    /* Path variables */
    private val path = new File("..").getCanonicalPath ++ fs ++ "ifdeftoif" ++ fs
    // Path used to serialize the features found in the current AST
    private val serializedFeaturePath = path ++ "featureSet.txt"
    // Path to an extern struct declaration
    private val externOptionStructPath = path ++ "id2i_optionstruct.h"
    // Path to the statistics file
    private val statisticsPath = path ++ "statistics.csv"
    // Path to the top levelstatistics file
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
    // Threshold for a list size for computation of carthesian product
    private val duplicationThreshold = 200
    // Data structure which maps definitions of variables to their usages
    private var defuse: IdentityIdHashMap = new IdentityIdHashMap(new IdentityHashMap())
    // Data structure which maps usages of variables to their definition(s)
    private var usedef: IdentityIdHashMap = new IdentityIdHashMap(new IdentityHashMap())
    // Data structure which maps presence conditions to numbers used as prefixes in renamings
    private var featureNumberMap: Map[FeatureExpr, Int] = Map()
    // Data structure used for exporting Identifier renaming data
    private var replaceId: IdentityHashMap[Id, FeatureExpr] = new IdentityHashMap()
    // Data structure used to map Identifiers, which have to be renamed, to the presence conditions of the renamings
    private val idsToBeReplaced: IdentityHashMap[Id, Set[FeatureExpr]] = new IdentityHashMap()
    // Same data structure as above used for the second ifdeftoif run
    private val idsToBeReplacedSecondRun: IdentityHashMap[Id, Set[FeatureExpr]] = new IdentityHashMap()

    private val createFunctionsForModelChecking = false

    /**
     * Converts a feature expression to a condition in the c programming language. def(x64) becomes options.x64.
     */
    private def featureToCExpr(feature: FeatureExpr): Expr = feature match {
        case d: DefinedExternal => PostfixExpr(Id(featureStructInitializedName), PointerPostfixSuffix(".", Id(d.feature.toLowerCase)))
        case d: DefinedMacro => featureToCExpr(d.presenceCondition)
        case b: BDDFeatureExpr =>
            bddFexToCExpr(b,
                ((fName: String) => PostfixExpr(Id(featureStructInitializedName), PointerPostfixSuffix(".", Id(fName.toLowerCase))))
            )
        case a: And =>
            val l = a.clauses.toList
            NAryExpr(featureToCExpr(l.head), (l.tail.map(x => Opt(trueF, NArySubExpr("&&", featureToCExpr(x))))))
        case o: Or =>
            val l = o.clauses.toList
            NAryExpr(featureToCExpr(l.head), l.tail.map(x => Opt(trueF, NArySubExpr("||", featureToCExpr(x)))))
        case Not(n) => UnaryOpExpr("!", featureToCExpr(n))
    }

    /**
     * Same as featureToCExpr for BDDs.
     */
    private def bddFexToCExpr(bdd: BDDFeatureExpr, transformFName: String => Expr): Expr = {
        if (bdd.isTautology(fm)) Constant("1")
        else if (bdd.isContradiction(fm)) Constant("0")
        else {
            def clause(d: Array[(Byte, String)]): Expr = NAryExpr(clauseForHead(d.head), clauseForTailElements(d.tail))
            def clauseForTailElements(d: Array[(Byte, String)]): List[Opt[NArySubExpr]] = d.map(
                x => (if (x._1 == 0)
                    List(Opt(trueF, NArySubExpr("&&", UnaryOpExpr("!", transformFName(x._2)))))
                else
                    List(Opt(trueF, NArySubExpr("&&", transformFName(x._2))))
                    )).foldLeft(List[Opt[NArySubExpr]]())((a, b) => a ++ b)
            def clauseForHead(x: (Byte, String)): Expr = (if (x._1 == 0)
                UnaryOpExpr("!", transformFName(x._2))
            else
                transformFName(x._2)
                )
            val cnfClauses: List[Expr] = bdd.getBddAllSat.map(clause(_)).toList
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
     * Loads a serialized Set of SingleFeatureExpressions.
     */
    private def loadSerializedFeatureNames(filename: String): Set[SingleFeatureExpr] = try {
        val fr = new ObjectInputStream(new FileInputStream(filename)) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val sfe = fr.readObject().asInstanceOf[Set[String]]
        fr.close()
        sfe.map(FeatureExprFactory.createDefinedExternal(_))
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized FeatureSet: " + e.getMessage); null
    }

    /**
     * Takes the difference between a feature (ex: A&B) and a context (ex: A) and computes a CExpr from the difference
     * of feature and context (ex: generates CExpr for B).
     */
    private def featureToDiffedCExpr(feature: FeatureExpr, context: FeatureExpr): Expr = {
        val shortFeature = fExprDiff(context, feature)
        featureToCExpr(shortFeature)
    }

    /**
     * Computes the difference between a features (ex: A&B) and a context (ex: A).
     * fExprDiff(A&B, A) => B
     */
    private def fExprDiff(context: FeatureExpr, pc: FeatureExpr): FeatureExpr = {
        if (context.equivalentTo(trueF) || context.equivalentTo(pc) || !(pc.implies(context).isTautology(fm))) {
            pc
        } else {
            val result = pc.diff(context)
            result
        }
    }

    /**
     * Retrieves an abstract syntax tree for a given file.
     */
    def getAstFromFile(fileToAnalyse: File): TranslationUnit = {
        def parseFile(stream: InputStream, file: String, dir: String): TranslationUnit = {
            val ast: AST = new ParserMain(new CParser).parserMain(
                () => CLexerAdapter.lexStream(stream, file, Collections.singletonList(dir), null), new CTypeContext, SilentParserOptions)
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
     * Returns a new CTypeSystemFrontend for a given AST.
     */
    private def getTypeSystem(ast: TranslationUnit): CTypeSystemFrontend = {
        new CTypeSystemFrontend(ast)
    }

    /**
     * Creates an AST including an external int (requested by Alexander von Rhein), a function which indicates feature
     * selection status, a struct with all features and an init function for that struct.
     */
    def getOptionFile(ast: AST): TranslationUnit = {
        val features = getSingleFeatures(ast)
        val optionsAst = getInitialTranslationUnit(features)
        optionsAst
    }

    /**
     * Loads the currently serialized features from @serializedFeaturePath and updates it with the features found in
     * given ast.
     */
    def loadAndUpdateFeatures(ast: TranslationUnit) = {
        if (new File(serializedFeaturePath).exists) {
            val loadedFeatures = loadSerializedFeatureNames(serializedFeaturePath)
            features = getSingleFeatures(ast) ++ loadedFeatures
        } else {
            features = getSingleFeatures(ast)
        }
        serializeFeatureNames(features.map(_.feature.toString), serializedFeaturePath)
    }

    /**
     * Converts a set of FeatureExpressions into a struct declaration.
     */
    def getOptionStruct(defExSet: Set[SingleFeatureExpr]): Declaration = {
        val structDeclList = defExSet.map(x => {
            featureToStructDeclaration(x.feature)
        }).toList
        structDeclListToDeclaration(structDeclList)
    }

    /**
     * Converts the name of a feature to an AST element of the type StructDeclaration. Ex: "CONFIG_X64" -> int config_x64
     * @param featureName
     * @return
     */
    def featureToStructDeclaration(featureName: String): Opt[StructDeclaration] = {
        Opt(trueF, StructDeclaration(List(Opt(trueF, IntSpecifier())), List(Opt(trueF, StructDeclarator(AtomicNamedDeclarator(List(), Id(featureName.toLowerCase), List()), None, List())))))
    }

    /**
     * Converts a list of StructDeclarations into a Declaration. Ex: List(int config_x64) -> struct ifdefoptions {int config_x64;} id2i;
     * @param structDeclList
     * @return
     */
    def structDeclListToDeclaration(structDeclList: List[Opt[StructDeclaration]]): Declaration = {
        Declaration(List(Opt(trueF, StructOrUnionSpecifier(false, Some(Id(featureStructName)), Some(structDeclList), List(), List()))), List(Opt(trueF, InitDeclaratorI(AtomicNamedDeclarator(List(), Id(featureStructInitializedName), List()), List(), None))))
    }

    /**
     * Converts the name of a feature 'n' and a given target expression 'e' into an assignment in the form of id2i.'n' = 'e';
     * @param featureName
     * @param assignmentSource
     * @return
     */
    def featureToAssignment(featureName: String, assignmentSource: Expr): Opt[ExprStatement] = {
        Opt(trueF, ExprStatement(AssignExpr(PostfixExpr(Id(featureStructInitializedName), PointerPostfixSuffix(".", Id(featureName.toLowerCase))), "=", assignmentSource)))
    }

    /**
     * Takes a set of SingleFeatureExpr and a path to a feature configuration file and returns an init function which
     * assigns values to the struct members of the ifdeftoif config struct according to the feature selection states
     * from the given feature configuration file.
     * @param defExSet
     * @param featureConfigPath
     * @return
     */
    def getInitFunction(defExSet: Set[SingleFeatureExpr], featureConfigPath: String = ""): FunctionDef = {
        var exprStmts: List[Opt[ExprStatement]] = List()

        if (!featureConfigPath.isEmpty) {
            val featureConfigFile = new File(featureConfigPath)
            val trueAndFalseFeatures = getFeaturesFromConfiguration(featureConfigFile, fm, defExSet)

            val trueFeaturesInSet = trueAndFalseFeatures._1
            val falseFeaturesInSet = trueAndFalseFeatures._2
            val otherFeaturesInSet = trueAndFalseFeatures._3

            val trueExprs = trueFeaturesInSet.map(x => featureToAssignment(x.feature, Constant("1")))
            val falseExprs = falseFeaturesInSet.map(x => featureToAssignment(x.feature, Constant("0")))
            val otherExprs = otherFeaturesInSet.map(x => featureToAssignment(x.feature, defaultConfigurationParameter))
            exprStmts = trueExprs ++ otherExprs ++ falseExprs
        } else {
            exprStmts = defExSet.toList.map(x => featureToAssignment(x.feature, defaultConfigurationParameter))
        }
        FunctionDef(List(Opt(trueF, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(initFunctionName), List(Opt(trueF, DeclIdentifierList(List())))), List(), CompoundStatement(exprStmts))
    }

    /**
     * Returns the AST representation of functions and declarations used for model checking.
     * The first is an extern declaration: extern int __VERIFIER_nondet_int(void);
     * The second is a function:
     * int select_one() {
     * if (__VERIFIER_nondet_int())
     * return 1;
     * else
     * return 0;
     * }
     */
    def getFunctionsForModelChecking(): List[Opt[ExternalDef]] = {
        val externDeclaration = Declaration(List(Opt(trueF, ExternSpecifier()), Opt(trueF, IntSpecifier())), List(Opt(trueF, InitDeclaratorI(AtomicNamedDeclarator(List(), Id("__VERIFIER_NONDET_INT"), List(Opt(trueF, DeclParameterDeclList(List(Opt(trueF, PlainParameterDeclaration(List(Opt(trueF, VoidSpecifier())), List()))))))), List(), None))))
        val selectOneFunction = FunctionDef(List(Opt(trueF, IntSpecifier())), AtomicNamedDeclarator(List(), Id("select_one"), List(Opt(trueF, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(trueF, IfStatement(One(PostfixExpr(Id("__VERIFIER_NONDET_INT"), FunctionCall(ExprList(List())))), One(CompoundStatement(List(Opt(trueF, ReturnStatement(Some(Constant("1"))))))), List(), Some(One(CompoundStatement(List(Opt(trueF, ReturnStatement(Some(Constant("0")))))))))))))
        List(Opt(trueF, externDeclaration), Opt(trueF, selectOneFunction))
    }

    /**
     * Returns a function used to initialize the feature values of the ifdeftoif configuration struct with the value
     * 'select_one' from the function 'getFunctionsForModelChecking'.
     * @param defExSet
     * @return
     */
    def getModelCheckInitFunction(defExSet: Set[SingleFeatureExpr]): FunctionDef = {
        val cmpStmt = defExSet.map(x => {
            Opt(trueF, ExprStatement(AssignExpr(PostfixExpr(Id(featureStructInitializedName), PointerPostfixSuffix(".", Id(x.feature.toLowerCase))), "=", PostfixExpr(Id("select_one"), FunctionCall(ExprList(List()))))))
        }).toList
        FunctionDef(List(Opt(trueF, VoidSpecifier())), AtomicNamedDeclarator(List(), Id("initOptions"), List(Opt(trueF, DeclIdentifierList(List())))), List(), CompoundStatement(cmpStmt))
    }

    /**
     * Returns the initial TranslationUnit with the ifdeftoif configuration struct and a function which initializes
     * the features inside the ifdeftoif configuration struct with values.
     * @param defExSet
     * @param featureConfigPath
     * @return
     */
    def getInitialTranslationUnit(defExSet: Set[SingleFeatureExpr], featureConfigPath: String = ""): TranslationUnit = {
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
     * Creates an id2i_optionstruct.h file where the ifdeftoif feature configuration struct and the init function for
     * assigning selection states to features. The feature selection states are read from the given .config file path.
     * @param featureConfigPath
     */
    def writeExternIfdeftoIfStruct(featureConfigPath: String) = {
        val featureSet = loadSerializedFeatureNames(serializedFeaturePath)
        val structDeclaration = Opt(trueF, getOptionStruct(loadSerializedFeatureNames(serializedFeaturePath)))
        val externDeclaration = Opt(True, Declaration(List(Opt(True, ExternSpecifier()), Opt(True, StructOrUnionSpecifier(false, Some(Id(featureStructName)), None, List(), List()))), List(Opt(True, InitDeclaratorI(AtomicNamedDeclarator(List(), Id(featureStructInitializedName), List()), List(), None)))))
        val initFunction = Opt(trueF, getInitFunction(featureSet, featureConfigPath))

        PrettyPrinter.printF(TranslationUnit(List(structDeclaration, externDeclaration, initFunction)), externOptionStructPath)
    }

    /**
     * Filters a given product for feature expressions which are not True and returns a set including each single feature expression.
     */
    def getSingleFeatures(a: Any): Set[SingleFeatureExpr] = {
        var featureSet: Set[FeatureExpr] = Set()
        val r = manytd(query {
            case Opt(ft, _) =>
                featureSet += ft
            case Choice(ft, _, _) =>
                featureSet += ft
        })
        r(a).get
        featureSet.flatMap(x => x.collectDistinctFeatureObjects)
    }

    /**
     * Returns the very first #include statement used to the external ifdeftoif configuration file in string form.
     * This function looks for a serialized Set of feature names in 'serializedFeaturePath'
     * Ex: #include "TypeChef/ifdeftoif/id2i_optionstruct.h"
     * @param includePath
     * @return
     */
    def getIncludeStatement(includePath: String): String = {
        if (!includePath.isEmpty) {
            "#include \"" + includePath + "\"\n"
        } else {
            ""
        }
    }

    /**
     * Retrieves a list of tuples out of a choice node which include the corresponding FeatureExpr and AST node.
     * Also takes choices inside choices into account.
     */
    def conditionalToTuple[T <: Product](choice: Conditional[T], currentContext: FeatureExpr = trueF): List[(FeatureExpr, T)] = {
        val choiceList = choice.toList
        if (currentContext.equals(trueF)) {
            choiceList
        } else {
            //val result = choiceList.filter(x => x._1.implies(currentContext).isTautology)
            val result = choiceList.filterNot(x => x._1.and(currentContext).isContradiction(fm)).map(x => (x._1.and(currentContext), x._2))
            result
        }
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
    def conditionalToConditionalExpr(choice: Conditional[Expr], currentContext: FeatureExpr = trueF, transformExpr: Boolean = false): One[Expr] = {
        val conditionalTuple = conditionalToTuple(choice, currentContext)
        conditionalTuple match {
            case Nil =>
                choice match {
                    case o: One[Expr] =>
                        o
                    case c: Choice[Expr] =>
                        One(conditionalTuple.find(e => e._1.implies(currentContext).isTautology()).getOrElse((trueF, Id("")))._2)
                }
            case x :: Nil =>
                val currentExpr = replaceOptAndId(x._2, x._1)
                if (transformExpr) {
                    val features = computeFeaturesForDuplication(x._2, x._1)
                    One(convertToCondExpr(currentExpr, features, currentContext))
                } else {
                    One(currentExpr)
                }
            case x :: xs =>
                if (transformExpr) {
                    val innerExpr = replaceOptAndId(x._2, x._1)
                    val trasformedInnerExpr = convertToCondExpr(innerExpr, computeFeaturesForDuplication(innerExpr, x._1), currentContext)
                    // val newInnerExpr = ConditionalExpr(featureToCExpr(x._1.not), None, trasformedInnerExpr)
                    val newInnerExpr = trasformedInnerExpr
                    val resultExpr = xs.foldLeft(newInnerExpr)((expr, condTuple) => {
                        val newExpr = replaceOptAndId(condTuple._2, condTuple._1)
                        val transformedNewExpr = convertToCondExpr(newExpr, computeFeaturesForDuplication(newExpr, condTuple._1), currentContext)
                        ConditionalExpr(featureToCExpr(fExprDiff(currentContext, condTuple._1)), Some(transformedNewExpr), expr)
                    })
                    One(resultExpr)
                } else {
                    // val innerExpr = ConditionalExpr(featureToCExpr(x._1.not), None, replaceOptAndId(x._2, x._1))
                    val innerExpr = replaceOptAndId(x._2, x._1)
                    val resultExpr = xs.foldLeft(innerExpr)((expr, condTuple) => {
                        val newExpr = replaceOptAndId(condTuple._2, condTuple._1)
                        ConditionalExpr(featureToCExpr(fExprDiff(currentContext, condTuple._1)), Some(newExpr), expr)
                    })
                    One(resultExpr)
                }
        }
    }

    /**
     * This method fills the IdMap which is used to map a feature expression to a number. This number is used for
     * for renaming identifiers e.g. #ifdef A int a #endif -> int _1_a     feature A is mapped to number 1.
     */
    def fillIdMap(a: Any) {
        if (featureNumberMap.size == 0) {
            featureNumberMap += (trueF -> featureNumberMap.size)
        }

        if (new File(path ++ "featureMap.csv").exists) {
            val featureMap = scala.io.Source.fromFile(path ++ "featureMap.csv").mkString.split("\n")
            if (!featureMap.isEmpty) {
                featureMap.foreach(x => {
                    val tuple = x.split(",")
                    val feature = new FeatureExprParser().parse(tuple.head)
                    val number = tuple.tail.head.toInt
                    featureNumberMap += (feature -> number)
                })
            }
        }
    }

    private def updateIdMap(feat: FeatureExpr) = {
        if (!featureNumberMap.contains(feat)) {
            featureNumberMap += (feat -> featureNumberMap.size)
        }
    }

    /**
     * Creates a prefix for identifiers from the presence condition under which they occur.
     * Format is _x_ where x is an Integer which represents the presence condition.
     */
    def getPrefixFromIdMap(feat: FeatureExpr): String = {
        def getFromIdMap(feat: FeatureExpr): Int = {
            updateIdMap(feat)
            featureNumberMap.get(feat).get
        }
        "_" + getFromIdMap(feat) + "_"
    }

    /**
     * Creates all possible 2 power n combinations for a list of n raw (single) feature expressions. List(def(x64), def(x86))
     * becomes List(def(x64)&def(x86),!def(x64)&def(x86),def(x64)&!def(x86),!def(x64)&!def(x86).
     */
    def getFeatureCombinations(fList: List[FeatureExpr]): List[FeatureExpr] = {
        if (fList.size == 0) {
            List()
        } else {
            fList.tail.foldLeft(List(fList.head, fList.head.not()))((curRes, curFExpr) => {
                curRes.flatMap(x => List(x.and(curFExpr), x.and(curFExpr.not())))
            })
        }
    }

    /**
     * Creates all possible 2 power n combinations for a list of n raw (single) feature expressions. List(def(x64), def(x86))
     * becomes List(def(x64)&def(x86),!def(x64)&def(x86),def(x64)&!def(x86),!def(x64)&!def(x86).
     * Also filters out features which are not satisfiable according to the feature model and the given context.
     */
    def getFeatureCombinationsFiltered(fList: List[FeatureExpr], context: FeatureExpr): List[FeatureExpr] = {
        getFeatureCombinations(fList).filterNot(x => x.and(context).isSatisfiable() || x.isSatisfiable(fm))
    }
    /**
     * Retrieves the FeatureExpression which is mapped to the given number. Used for the second run of the
     * ifdeftoif transformation to retrieve the context of an already renamed identifier.
     */
    def getFeatureForId(id: Int): Option[FeatureExpr] = {
        if (featureNumberMap.size < id || id < 0) {
            None
        } else {
            val it = featureNumberMap.iterator
            while (it.hasNext) {
                val next = it.next()
                if (next._2.equals(id)) {
                    return Some(next._1)
                }
            }
            None
        }
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
            val idUsages = usedef.get(i).flatMap(x => defuse.get(x))
            idUsages.foreach(x => {
                putIntoIdsToBeReplaced(x, ft)
            })
        }
    }

    /**
     * Renames identifiers inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    def convertIds[T <: Product](t: T, ft: FeatureExpr): T = {
        if (ft.equivalentTo(trueF, fm))
            return t

        val r = alltd(rule {
            case init@InitDeclaratorI(decl@AtomicNamedDeclarator(a, i: Id, b), attr, inits) =>
                if (!isMainFunction(i.name)) {
                    addIdUsages(i, ft)
                    replaceId.put(i, ft)
                    updateIdMap(ft)
                    InitDeclaratorI(AtomicNamedDeclarator(a, renameIdentifier(i, ft), b), attr, inits)
                } else {
                    init
                }
            case init@InitDeclaratorI(nnd@NestedNamedDeclarator(l, decl@AtomicNamedDeclarator(a, i: Id, b), r, c), attr, inits) =>
                if (!isMainFunction(i.name)) {
                    addIdUsages(i, ft)
                    replaceId.put(i, ft)
                    updateIdMap(ft)
                    InitDeclaratorI(NestedNamedDeclarator(l, AtomicNamedDeclarator(a, renameIdentifier(i, ft), b), r, c), attr, inits)
                } else {
                    init
                }
        })

        r(t).getOrElse(t).asInstanceOf[T]
    }

    /**
     * Renames the first identifier inside a declaration by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    def convertId[T <: Product](current: T, feat: FeatureExpr): T = {
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
                        AtomicNamedDeclarator(pointers, renameIdentifier(i, ft), extensions).asInstanceOf[T]
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
     * Checks if given functionName describes the call to a 'main' C-function.
     * "_main" check for BusyBox files: in BusyBox ls_main is the main function of ls functionality, which is
     * statically linked into the BusyBox binary.
     */
    def isMainFunction(functionName: String): Boolean = {
        functionName.equals("main") || functionName.equals(currentFileName + "_main")
    }

    /**
     * Renames identifiers inside of StructDeclarations by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    def convertStructId[T <: Product](t: T, ft: FeatureExpr): T = {
        val r = oncetd(rule {
            case decl@AtomicNamedDeclarator(a, i: Id, b) =>
                if (!isMainFunction(i.name)) {
                    addIdUsages(i, ft)
                    replaceId.put(i, ft)
                    updateIdMap(ft)
                    AtomicNamedDeclarator(a, renameIdentifier(i, ft), b)
                } else {
                    decl
                }
        })

        if (ft.equivalentTo(trueF, fm))
            return t

        r(t).getOrElse(t).asInstanceOf[T]
    }

    /**
     * Renames Enumerators by adding the ifdeftoif prefix number for given FeatureExpr ft.
     */
    def convertEnumId(enu: Enumerator, ft: FeatureExpr): Enumerator = {
        if (ft.equivalentTo(trueF)) {
            enu
        } else {
            addIdUsages(enu.id, ft)
            updateIdMap(ft)
            Enumerator(renameIdentifier(enu.id, ft), enu.assignment)
        }
    }

    /**
     * Converts a given enumerator or initializer element. If they contain variability a new initializer/enumerator
     * element is returned where the variability is encoded in the form of a conditional expression.
     */
    // TODO: @fgarbe: The name of the function is not very specifc.
    def convertToCondExpr[T <: Product](current: T, variantFeatures: List[FeatureExpr], currentContext: FeatureExpr): T = {
        def condExprHelper(expr: Expr, features: List[FeatureExpr]): Expr = {
            val innerMostExpr = replaceOptAndId(expr, features.head)
            features.tail.foldLeft(innerMostExpr)((first, second) =>
                ConditionalExpr(featureToCExpr(fExprDiff(currentContext, second)), Some(replaceOptAndId(expr, second)), first))
        }
        variantFeatures match {
            case Nil =>
                current
            case x :: Nil =>
                replaceOptAndId(current, x)
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
     * Converts a given Choice[Expr] element to a ConditionalExpr. Example:
     * Choice(def(A),One(Id(a)),Choice(def(B),One(Id(b)),One(null))) -> (id2i_opt.a ? a : ((! id2i_opt.b) ?  : b))
     */
    def conditionalToCondExpr(current: Conditional[Expr], currentContext: FeatureExpr): Conditional[Expr] = {
        def conditionalToCondExprHelper(condExpr: Conditional[Expr], currentFeature: FeatureExpr = trueF): Expr = {
            condExpr match {
                case One(expr: Expr) =>
                    replaceOptAndId(expr, currentFeature)
                case Choice(feat, One(null), elseBranch) =>
                    ConditionalExpr(featureToCExpr(feat), None, conditionalToCondExprHelper(elseBranch, currentFeature.and(feat.not())))
                case Choice(feat, thenBranch, One(null)) =>
                    ConditionalExpr(featureToCExpr(feat.not), None, conditionalToCondExprHelper(thenBranch, currentFeature.and(feat)))
                case Choice(feat, thenBranch, elseBranch) =>
                    ConditionalExpr(featureToCExpr(feat), Some(conditionalToCondExprHelper(thenBranch, currentFeature.and(feat))), conditionalToCondExprHelper(elseBranch, currentFeature.and(feat.not())))
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
    def renameIdentifier(id: Id, context: FeatureExpr): Id = {
        var actualContext = context
        var idname = id.name
        if (context.equivalentTo(trueF)) {
            id
        } else {
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

    /**
     * Replaces given FeatureExpression recursively from given Element by True. Also removes Opt nodes which should not
     * occur in this given context. Also renames identifiers if they have a declaration annotated by given FeatureExpression.
     */
    def replaceOptAndId[S <: Product](current: S, feature: FeatureExpr): S = {
        def replaceHelp[T <: Any](t: T, feat: FeatureExpr): T = {
            val r = alltd(rule {
                case l: List[Opt[_]] =>
                    l.flatMap(x => {
                        x match {
                            case o: Opt[_] =>
                                if (o.feature.equivalentTo(feat, fm) || feat.implies(o.feature).isTautology(fm)) {

                                    // Feature in opt node is equal or less specific than the context, replace opt node feature with True
                                    List(Opt(trueF, replaceHelp(o.entry, feat)))
                                } else if (feat.and(o.feature).isSatisfiable(fm)) {

                                    // Feature in opt node is more specific and still satisfiable in the context, don't change opt node
                                    List(Opt(o.feature, replaceHelp(o.entry, feat)))
                                } else {

                                    // Feature in opt node is not satisfiable in the current context, remove opt node
                                    List()
                                }
                        }
                    })
                case i: Id =>
                    if (idsToBeReplaced.containsKey(i)) {
                        updateIdMap(feat)
                        val featureList = idsToBeReplaced.get(i)
                        val matchingId = featureList.find(x => feat.implies(x).isTautology(fm))
                        matchingId match {
                            case None =>
                                // TODO: this should not happen?
                                val lst = idsToBeReplaced.get(i)
                                renameIdentifier(i, feat)
                                i
                            case Some(x: FeatureExpr) =>
                                if (x.equivalentTo(trueF, fm)) {
                                    i
                                } else {
                                    renameIdentifier(i, x)
                                }
                            case k =>
                                Id("")
                        }
                    } else {
                        i
                    }
            })
            r(t).getOrElse(t).asInstanceOf[T]
        }
        if (feature.equivalentTo(trueF, fm)) {
            current
        } else {
            current match {
                case o@Opt(ft, entry) =>
                    if (ft.equivalentTo(trueF, fm) || ft.equivalentTo(feature, fm)) {
                        Opt(trueF, replaceHelp(entry, feature)).asInstanceOf[S]
                    } else {
                        Opt(ft, replaceHelp(entry, feature)).asInstanceOf[S]
                    }
                case _ =>
                    replaceHelp(current, feature).asInstanceOf[S]
            }
        }
    }

    /**
     * Calls the replaceOptAndId function first and then the transformRecursive function on given Product.
     */
    def replaceAndTransform[T <: Product](t: T, feat: FeatureExpr): T = {
        transformRecursive(replaceOptAndId(t, feat), feat)
    }

    /**
     * Starts the ifdeftoif transformation process on the given TranslationUnit and additional information like
     * the featureModel, declUseMap etc.
     */
    def ifdeftoif(source_ast: TranslationUnit, decluse: IdentityIdHashMap, usedecl: IdentityIdHashMap, featureModel: FeatureModel = FeatureExprLib.featureModelFactory.empty, outputStem: String = "unnamed", lexAndParseTime: Long = 0, writeStatistics: Boolean = true, newPath: String = "", typecheckResult: Boolean = true, useExternConfigStruct: Boolean = true): (Option[AST], Long, List[TypeChefError]) = {
        new File(path).mkdirs()
        val tb = java.lang.management.ManagementFactory.getThreadMXBean
        init(fm)

        // Set the feature model, declUseMap, useDeclMap
        fm = featureModel
        defuse = decluse
        usedef = usedecl

        fillIdMap(source_ast)
        loadAndUpdateFeatures(source_ast)

        val fileNameWithExt = outputStemToFileName(outputStem)
        currentFileName = getFileNameWithoutExtension(fileNameWithExt)

        val time = tb.getCurrentThreadCpuTime()
        var new_ast = transformRecursive(source_ast, trueF, true)

        // Check if a second run is required, can be the case if structs get used before they get defined
        if (!idsToBeReplacedSecondRun.isEmpty) {
            isFirstRun = false
            new_ast = transformRecursive(new_ast, trueF)
        }
        val transformTime = (tb.getCurrentThreadCpuTime() - time) / nstoms
        var result_ast: TranslationUnit = new_ast

        var typecheck_ast: TranslationUnit = TranslationUnit(List())
        if (useExternConfigStruct) {
            typecheck_ast = TranslationUnit(getInitialTranslationUnit(features).defs ++ new_ast.defs)
        } else {
            result_ast = TranslationUnit(getInitialTranslationUnit(features).defs ++ new_ast.defs)
            typecheck_ast = result_ast
        }
        exportRenamings()

        var ifdeftoif_file = ""
        if (newPath.equals("")) {
            ifdeftoif_file = outputStemToifdeftoif(outputStem)
        } else {
            ifdeftoif_file = newPath
        }
        PrettyPrinter.printF(result_ast, ifdeftoif_file, getIncludeStatement(externOptionStructPath))
        println("Printed ifdeftoif to file " + ifdeftoif_file)

        if (!typecheckResult) {
            println("Skipping typecheck of ifdeftoif result")
            (Some(result_ast), transformTime, List())
        } else {
            println("Typechecking result")
            val typeCheckSuccessful = checkAstSilent(typecheck_ast)

            val featureMap = featureNumberMap.-(trueF).map(x => x._1.toTextExpr + "," + x._2) mkString "\n"
            writeToFile(path ++ "featureMap.csv", featureMap)

            if (typeCheckSuccessful) {
                if (writeStatistics) {
                    exportStatistics(updateIfdeftoifStatistics(source_ast, typecheck_ast, fileNameWithExt, lexAndParseTime, transformTime, features.size, statisticsPath, topLevelStatisticsPath))
                }
                (Some(result_ast), transformTime, List())
            } else {

                // Overwrite file with #include statement with a file which possesses the ifdeftoif config struct
                PrettyPrinter.printF(typecheck_ast, ifdeftoif_file)
                val result_ast_with_position = getAstFromFile(new File(ifdeftoif_file))
                if (result_ast_with_position == null) {
                    val errorHeader = "-+ ParseErrors in " + fileNameWithExt + " +-\n"
                    if (!(new File(typeErrorPath).exists)) {
                        writeToFile(typeErrorPath, errorHeader + "\n\n")
                    } else {
                        appendToFile(typeErrorPath, errorHeader + "\n\n")
                    }
                    (None, 0, List())
                } else {
                    val errors = getAstErrors(result_ast_with_position)
                    val errorHeader = "-+ TypeErrors in " + fileNameWithExt + " +-\n"
                    val errorString = errors mkString "\n"
                    if (!(new File(typeErrorPath).exists)) {
                        writeToFile(typeErrorPath, errorHeader + errorString + "\n\n")
                    } else {
                        appendToFile(typeErrorPath, errorHeader + errorString + "\n\n")
                    }
                    (None, 0, errors)
                }
            }
        }
    }

    /**
     * Returns the filename of given absolute path (including file extension).
     * Ex: /home/user/main.c -> main.c
     */
    def outputStemToFileName(outputStem: String): String = {
        val lastSepIndex = outputStem.lastIndexOf(System.getProperty("file.separator"))
        if (lastSepIndex == -1) {
            outputStem
        } else {
            outputStem.substring(lastSepIndex + 1)
        }
    }

    /**
     * Returns the new absolute file path for the resulting transformation file.
     * @param outputStem
     * @return
     */
    def outputStemToifdeftoif(outputStem: String): String = {
        def outputStemToFileNameWithoutExtension(outputStem: String): String = {
            val indexOfLastFolderSep = outputStem.lastIndexOf(File.separatorChar)
            val lastPathElement = outputStem.substring(indexOfLastFolderSep);
            val lastSepIndex = indexOfLastFolderSep + lastPathElement.lastIndexOf(".")
            if (lastSepIndex == -1) {
                outputStem
            } else {
                outputStem.substring(0, lastSepIndex)
            }
        }

        if ((new File(outputStem)).getName.contains(".")) // if the filename has a extension, remove it
            outputStemToFileNameWithoutExtension(outputStem) + ifdeftoifFileSuffix
        else
            outputStem + ifdeftoifFileSuffix
    }

    def getFileNameWithoutExtension(file: File): String = {
        file.getName().replaceFirst("[.][^.]+$", "")
    }
    def getFileNameWithoutExtension(strg: String): String = {
        strg.replaceFirst("[.][^.]+$", "")
    }

    /**
     * Removes duplicate features by checking for boolean equality according to the featureModel.
     * List(A&&B, B&&A) -> List(A&&B)
     */
    def removeDuplicateFeatures(features: List[FeatureExpr], currentContext: FeatureExpr): List[FeatureExpr] = {
        features match {
            case Nil =>
                List()
            case x :: Nil =>
                List(x)
            case x :: xs =>
                if (xs.exists(y => y.equivalentTo(x, fm.and(currentContext)))) {
                    (removeDuplicateFeatures(xs, currentContext))
                } else {
                    x :: (removeDuplicateFeatures(xs, currentContext))
                }
        }
    }

    /**
     * Makes #ifdef to if transformation on given AST element. Returns new AST element and a statistics String.
     */
    def transformAst(source_ast: TranslationUnit, decluse: IdentityIdHashMap, usedecl: IdentityIdHashMap, parseTime: Long, featureModel: FeatureModel = FeatureExprLib.featureModelFactory.empty): (TranslationUnit, String) = {
        fm = featureModel
        init(fm)
        val tb = java.lang.management.ManagementFactory.getThreadMXBean

        fillIdMap(source_ast)
        features = getSingleFeatures(source_ast)
        defuse = decluse
        usedef = usedecl
        val time = tb.getCurrentThreadCpuTime()
        val result = transformRecursive(source_ast, trueF, true)
        val transformTime = (tb.getCurrentThreadCpuTime() - time) / nstoms

        val csvEntry = createCsvEntry(source_ast, result, "unnamed", parseTime, transformTime, features.size)
        (TranslationUnit(getInitialTranslationUnit(features).defs ++ result.asInstanceOf[TranslationUnit].defs), csvEntry)
    }

    /**
     * This is the core of the #ifdef to if transformation. This function is called recursively on all opt nodes inside the
     * given AST element. The general strategy is to look at opt nodes:
     * - statements which need to be duplicated or embedded inside if statements
     * - declarations which need to be duplicated/renamed
     */
    def transformRecursive[T <: Product](t: T, currentContext: FeatureExpr = trueF, isTopLevel: Boolean = false): T = {
        val r = alltd(rule {
            case l: List[Opt[_]] =>
                l.flatMap(x => {
                    x match {
                        case o@Opt(ft, entry) =>
                            if (x.entry.isInstanceOf[AST] && !x.entry.asInstanceOf[AST].range.getOrElse(None).equals(None)) {

                                /*
                                Exports the current code position in the source file. Can be used to find out
                                where the #ifdef to if progress stopped or gets stuck.
                                 */
                                writeToFile("ifdeftoif_progress.txt", x.entry.asInstanceOf[AST].range.get.toString() + " from " + o)
                            }
                            val features = computeFeaturesForDuplication(entry, o.feature.and(currentContext), isTopLevel)
                            if (exceedsThreshold(features)) {
                                List(o)
                            } else {
                                entry match {
                                    case InitDeclaratorI(decl, attributes, Some(init@Initializer(initElemLabel, expr))) =>
                                        if (!isTopLevel) {

                                            /*
                                            Local declarations with assignments can be transformed with conditional assignments.
                                            Ex: void main() { int a = id2i.x64 ? 0 : 1 }
                                            Not possible for assignments on global variables!
                                             */
                                            val exprFeatures = computeFeaturesForDuplication(expr, o.feature.and(currentContext))
                                            if (exceedsThreshold(exprFeatures)) {
                                                List(o)
                                            } else {
                                                val condExpr = convertToCondExpr(init, exprFeatures, currentContext)
                                                List(transformRecursive(Opt(ft, InitDeclaratorI(decl, attributes, Some(condExpr))), currentContext.and(ft)))
                                            }
                                        } else {
                                            List(transformRecursive(o, currentContext.and(ft)))
                                        }

                                    case DeclarationStatement(decl: Declaration) =>
                                        val result = handleDeclarations(Opt(ft, decl), currentContext, isTopLevel).map(x => Opt(trueF, DeclarationStatement(x.entry)))
                                        countDuplications(decl, result.size, isTopLevel)
                                        result
                                    case _: Declaration =>
                                        val result = handleDeclarations(o.asInstanceOf[Opt[Declaration]], currentContext, isTopLevel)
                                        countDuplications(o.entry, result.size, isTopLevel)
                                        result
                                    case i@Initializer(elem, expr) =>
                                        if (features.isEmpty) {
                                            List(replaceAndTransform(Opt(trueF, Initializer(elem, expr)), o.feature.and(currentContext)))
                                        } else {
                                            List(Opt(trueF, convertToCondExpr(i, features, currentContext)))
                                        }
                                    case e@Enumerator(_, None) =>
                                        if (!features.isEmpty) {
                                            countDuplications(o.entry, features.size, isTopLevel)
                                            features.map(x => Opt(trueF, transformRecursive(convertEnumId(replaceOptAndId(e, x), x), x)))
                                        } else if (ft.equals(trueF)) {
                                            List(transformRecursive(o, currentContext))
                                        } else {
                                            List(Opt(trueF, transformRecursive(convertEnumId(replaceOptAndId(e, o.feature), o.feature), o.feature.and(currentContext))))
                                        }
                                    case e@Enumerator(_, Some(expr)) =>
                                        val exprFeatures = computeFeaturesForDuplication(expr, o.feature.and(currentContext))
                                        if (!features.isEmpty) {
                                            countDuplications(o.entry, features.size, isTopLevel)
                                            features.map(x => Opt(trueF, transformRecursive(convertEnumId(replaceOptAndId(convertToCondExpr(e, exprFeatures, o.feature.and(currentContext)), x), x), x)))
                                        } else if (ft.equals(trueF)) {
                                            List(transformRecursive(Opt(trueF, convertToCondExpr(e, exprFeatures, o.feature.and(currentContext))), currentContext))
                                        } else {
                                            List(Opt(trueF, transformRecursive(convertEnumId(replaceOptAndId(convertToCondExpr(e, exprFeatures, o.feature.and(currentContext)), o.feature), o.feature), o.feature.and(currentContext))))
                                        }
                                    case sd@StructDeclaration(qual, decl) =>
                                        if (!features.isEmpty) {
                                            countDuplications(o.entry, features.size, isTopLevel)
                                            features.map(x => replaceAndTransform(Opt(trueF, StructDeclaration(qual, convertStructId(decl, x))), x))
                                        } else if (ft.equals(trueF)) {
                                            List(transformRecursive(o, currentContext))
                                        } else {
                                            List(replaceOptAndId(Opt(trueF, StructDeclaration(qual, convertStructId(decl, o.feature))), o.feature))
                                        }

                                    case _: FunctionDef | _: NestedFunctionDef =>
                                        val result = handleFunction(o)
                                        countDuplications(o.entry, result.size, isTopLevel)
                                        result


                                    case _: IfStatement | _: WhileStatement | _: SwitchStatement | _: DoStatement | _: ForStatement =>
                                        val result = handleStatement(o, ft.and(currentContext))
                                        countDuplications(o.entry, result.size, isTopLevel)
                                        result
                                    case r: ReturnStatement =>
                                        if (!features.isEmpty) {
                                            val result = features.map(x => Opt(trueF, statementToIf(replaceAndTransform(r, x), x, currentContext)))
                                            result
                                        } else {
                                            if (ft.equals(trueF)) {
                                                List(transformRecursive(o, currentContext))
                                            } else {
                                                List(Opt(trueF, statementToIf(replaceAndTransform(r, ft), ft, currentContext)))
                                            }
                                        }
                                    case g: GotoStatement =>
                                        if (!features.isEmpty) {
                                            val result = features.map(x => Opt(trueF, statementToIf(replaceOptAndId(g, x), ft, currentContext)))
                                            result
                                        } else if (ft.equals(trueF)) {
                                            List(transformRecursive(o, currentContext))
                                        } else {
                                            List(Opt(trueF, statementToIf(replaceOptAndId(g, ft), ft, currentContext)))
                                        }
                                    case e: ExprStatement =>
                                        val realFeature = currentContext.and(o.feature)
                                        val features = computeFeaturesForDuplication(e, realFeature)
                                        if (!features.isEmpty) {
                                            countDuplications(o.entry, features.size, isTopLevel)
                                            features.map(x => Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, x.and(realFeature)))), One(CompoundStatement(List(replaceAndTransform(Opt(trueF, e), x.and(realFeature))))), List(), None)))
                                        } else if (ft.equals(trueF)) {
                                            List(transformRecursive(o, currentContext))
                                        } else {
                                            val result = List(Opt(trueF, IfStatement(One(featureToCExpr(realFeature)), One(CompoundStatement(List(replaceAndTransform(Opt(trueF, e), realFeature)))), List(), None)))
                                            result
                                        }
                                    case label: LabelStatement =>
                                        val features = computeFeaturesForDuplication(label, ft.and(currentContext))
                                        if (!features.isEmpty) {
                                            val result = features.map(x => Opt(trueF, statementToIf(replaceOptAndId(label, x), ft, currentContext)))
                                            result
                                        } else if (ft.equals(trueF)) {
                                            List(o)
                                        } else {
                                            List(Opt(trueF, statementToIf(replaceOptAndId(label, ft), ft, currentContext)))
                                        }


                                    case typeless: TypelessDeclaration =>
                                        // TODOkA: Umwandlung
                                        List(o)
                                    case Pragma(stringLit) =>
                                        // TODO: Eventuell variabel lassen
                                        List(Opt(trueF, Pragma(StringLit(stringLit.name.map(x => Opt(trueF, x.entry))))))
                                    case es: EmptyStatement =>
                                        List()
                                    case ee: EmptyExternalDef =>
                                        List()
                                    case cs@CompoundStatement(innerStmts) =>
                                        if (o.feature.equals(trueF)) {
                                            List(Opt(trueF, replaceAndTransform(cs, o.feature.and(currentContext))))
                                        } else {
                                            List(Opt(trueF, IfStatement(One(featureToCExpr(o.feature)), One(replaceAndTransform(cs, o.feature)), List(), None)))
                                        }
                                    case k =>
                                        //println("Missing Opt: " + o + "\nFrom: " + k.asInstanceOf[AST].getPositionFrom + "\n")
                                        List(transformRecursive(o, currentContext.and(o.feature)))
                                }
                            }
                        case k => List(transformRecursive(k, currentContext))
                    }
                })
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

    /**
     * Computes the cartesian product of a list of lists of FeatureExpressions using the boolean 'and' operator.
     * Ex: List( List(a, b), List(c, d, e)) becomes List(a&c, a&d, a&e, b&c, b&d, b&e).
     */
    def computeCarthesianProduct(list: List[List[FeatureExpr]], context: FeatureExpr): List[FeatureExpr] = {
        def computeCarthesianProductHelper(listOfLists: List[List[FeatureExpr]], currentContext: FeatureExpr): List[FeatureExpr] = {
            val preparedList = listOfLists.map(x => x.filterNot(y => y.equivalentTo(FeatureExprFactory.False) || y.equivalentTo(trueF) || y.equivalentTo(currentContext))).filterNot(x => x.isEmpty)
            preparedList match {
                case Nil =>
                    List()
                case x :: Nil =>
                    x
                case x :: xs =>
                    xs.foldLeft(x)((first, second) => {
                        if (first.size > 1000) {
                            println("Currently computed " + first.size + " different features.")
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
        computeCarthesianProductHelper(list, context).filter(x => x.isSatisfiable() && x.isSatisfiable(fm))
    }

    /**
     * Checks whether given feature list contains exactly 1 False feature. Used to tag featuresForDuplications where
     * the number of features exceeds the variantThreshold.
     */
    def exceedsThreshold(lst: List[FeatureExpr]): Boolean = {
        lst.size == 1 && lst.head.equals(FeatureExprFactory.False)
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
            })
        }
    }

    /**
     * Returns a list of FeatureExpressions for the given AST Element a. This list contains FeatureExpressions that
     * require code duplications. Example: condition inside an IfStatement has a variable Identifier -> we have to create
     * two different IfStatements and the function returns these two distinct features.
     */
    def computeFeaturesForDuplication(a: Any, curCtx: FeatureExpr, isTopLevel: Boolean = false): List[FeatureExpr] = {
        def computationHelper(a: Any, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
            val featureList = getNextOptFeatures(a, currentContext, isTopLevel).filterNot(x => x.equivalentTo(currentContext, fm)) ++ List(FeatureExprFactory.False)
            val identFeatureList = getNextIdFeatures(a, currentContext, isTopLevel)
            if (featureList.size == 1 && identFeatureList.isEmpty) {
                List()
            } else {
                val featureBuffer: ListBuffer[List[FeatureExpr]] = ListBuffer()
                val currentFeatures: mutable.HashSet[FeatureExpr] = new mutable.HashSet
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
                    val result = computeCarthesianProduct(List(firstResult, identFeatureList.diff(firstResult)), currentContext)
                    result
                } else {
                    val featureBufferList = featureBuffer.toList
                    val potentialResultSize1 = featureBufferList.map(x => x.size).foldLeft(1)(_ * _)
                    val distinctSingleFeatures = featureBufferList.flatten.map(x => x.collectDistinctFeatureObjects).flatten.distinct
                    val potentialResultSize2 = Math.pow(distinctSingleFeatures.size, 2).toInt
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
                        }
                        List(FeatureExprFactory.False)
                    } else {
                        if (potentialResultSize2 < potentialResultSize1) {
                            val result = getFeatureCombinationsFiltered(distinctSingleFeatures, currentContext)
                            result
                        } else {
                            val optResult = computeCarthesianProduct(featureBufferList, currentContext)
                            val result = computeCarthesianProduct(List(optResult, identFeatureList.diff(optResult)), currentContext)
                            result
                        }
                    }
                }
            }
        }
        def handleLists(listOfLists: List[List[FeatureExpr]], currentContext: FeatureExpr): List[FeatureExpr] = {
            if (listOfLists.exists(x => exceedsThreshold(x))) {
                List(FeatureExprFactory.False)
            } else {
                listOfLists match {
                    case Nil => Nil
                    case x :: Nil => x
                    case k => computeCarthesianProduct(k, currentContext)
                }
            }
        }
        a match {
            case o: Opt[_] =>
                if (curCtx.implies(o.feature).isTautology(fm)) {
                    computeFeaturesForDuplication(o.entry, curCtx)
                } else {
                    val features = computeFeaturesForDuplication(o.entry, o.feature)
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
                val result = handleLists(featureLists, curCtx)
                result
            case is@IfStatement(One(statement), thenBranch, elif, els) =>
                computationHelper(statement, curCtx)
            case is@IfStatement(c: Choice[Product], thenBranch, elif, els) =>
                val choices = conditionalToTuple(c, curCtx)
                choices.flatMap(x => computationHelper(x._2, x._1)).distinct
            case ss@SwitchStatement(e, One(stmt: CompoundStatement)) =>
                computationHelper(ss.expr, curCtx)
            case es: ExprStatement =>
                computationHelper(es.expr, curCtx)
            case ds: DoStatement =>
                computationHelper(ds.expr, curCtx)
            case rs@ReturnStatement(Some(CompoundStatementExpr(CompoundStatement(stmts)))) =>
                computeCarthesianProduct(computeDistinctLists(stmts.map(x => computationHelper(x.entry, curCtx.and(x.feature)))), curCtx)
            case rs@ReturnStatement(Some(x)) =>
                computationHelper(x, curCtx)
            case gs: GotoStatement =>
                computationHelper(gs.target, curCtx)
            case fd: FunctionDef =>
                val featureLists = List(computationHelper(fd.specifiers, curCtx), computationHelper(fd.declarator, curCtx), computationHelper(fd.oldStyleParameters, curCtx))
                val result = handleLists(featureLists, curCtx)
                result
            case d@Declaration(declSpecs, init) =>
                // Handled by handleDeclarations
                val featureLists = List(computationHelper(declSpecs, curCtx), computationHelper(init, curCtx))
                val result = handleLists(featureLists, curCtx)
                result
            case nfd: NestedFunctionDef =>
                val featureLists = List(computationHelper(nfd.specifiers, curCtx), computationHelper(nfd.declarator, curCtx), computationHelper(nfd.parameters, curCtx))
                val result = handleLists(featureLists, curCtx)
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
        var result = computeFeaturesForDuplication(a, currentContext, isTopLevel)
        if (result.isEmpty) {
            result = List(currentContext)
        }
        result
    }

    /**
     * Takes a look at the CaseStatements and CompoundStatements inside a SwitchStatement in order to determine
     * the list of FeatureExpressions needed for duplication.
     */
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
        val caseFeatures = getFeatureCombinations(cmpStmt.innerStatements.map(x => {x.feature}).filter(x => !x.equivalentTo(trueF)).flatMap(x => x.collectDistinctFeatureObjects).distinct).filter(x => x.implies(currentContext).isTautology(fm))
        caseFeatures
    }

    /**
     * Takes a look at the CompoundStatements and CaseStatements AS WELL as the expression inside the CaseStatements
     * in a SwitchStatement in order to determine the list of FeatureExpressions needed for duplication purposes.
     */
    def computeTotalCaseFeatures(cmpStmt: CompoundStatement, currentContext: FeatureExpr = trueF): List[FeatureExpr] = {
        val caseFeatures = getFeatureCombinations(cmpStmt.innerStatements.flatMap(x => {
            x.entry match {
                case cs: CaseStatement =>
                    val features = computeFeaturesForDuplication(x, currentContext)
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
                case d@Opt(ft, entry: Initializer) =>
                    //println("Stopping at: " + d)
                    val realFeature = getFeatureForContext(ft, currentContext)
                    if (ft.equivalentTo(trueF) || ft.equivalentTo(FeatureExprFactory.False) || realFeature.equivalentTo(currentContext)) {
                        List()
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
        computeCarthesianProduct(listOfLists, currentContext).filter(z => z.isSatisfiable(fm) && !z.equivalentTo(trueF))
    }

    /**
     * Transforms given Statement e into an IfStatement with ifdeftoif condition according to given FeatureExpr ft.
     * Ex: 3 + 1; -> if (id2i.ft) { 3 + 1; }
     */
    def statementToIf(e: Statement, ft: FeatureExpr, currentContext: FeatureExpr): IfStatement = {
        IfStatement(One(featureToCExpr(fExprDiff(currentContext, ft))), One(CompoundStatement(List(Opt(trueF, replaceOptAndId(e, ft))))), List(), None)
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
    def handleStatement(opt: Opt[_], currentContext: FeatureExpr = trueF): List[Opt[_]] = {
        opt.entry match {
            case i: IfStatement =>
                handleIfStatementConditional(opt, currentContext)
            //handleIfStatement(opt, currentContext)
            case f: ForStatement =>
                handleForStatement(opt.asInstanceOf[Opt[Statement]], currentContext)
            case w: WhileStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext)
            case d: DoStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext)
            case s: SwitchStatement =>
                handleWSDStatements(opt.asInstanceOf[Opt[Statement]], currentContext)

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
    def handleIfStatementConditional(optIf: Opt[_], currentContext: FeatureExpr = trueF): List[Opt[_]] = {

        // 1. Step
        if (!optIf.feature.equivalentTo(trueF)) {
            optIf.entry match {
                case IfStatement(cond, thenBranch, elifs, elseBranch) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, optIf.feature))), One(CompoundStatement(handleIfStatementConditional(replaceOptAndId(optIf, optIf.feature), optIf.feature).asInstanceOf[List[Opt[Statement]]])), List(), None)))
                case _ =>
                    List()
            }
        } else {
            optIf.entry match {

                // 2. Step with conditionalExpressions
                case i@IfStatement(c: Conditional[Expr], thenBranch: Conditional[Statement], elif, els) =>
                    var newCond: Expr = null
                    val statementTuple = conditionalToTuple(thenBranch, currentContext)
                    var elseTuple = List((FeatureExprFactory.True, None.asInstanceOf[Option[Conditional[Statement]]]))
                    els match {
                        case None =>
                        case Some(One(stmt)) =>
                        case Some(c: Choice[Statement]) =>
                            elseTuple = conditionalToTuple(c, currentContext).map(x => (x._1, Some(One(x._2))))
                    }
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val elsFeatures = elseTuple.map(x => x._1)
                    val totalCarthProduct = computeCarthesianProduct(List(stmtFeatures, elsFeatures), currentContext)
                    totalCarthProduct match {
                        case Nil =>
                            newCond = conditionalToConditionalExpr(c, currentContext, true).value
                            val elsBranch = elseTuple.find(e => currentContext.implies(e._1).isTautology(fm)).get._2
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            List(Opt(trueF, IfStatement(One(newCond), replaceAndTransform(stmt, currentContext), elif.flatMap(y => handleIfStatementConditional(y, currentContext).asInstanceOf[List[Opt[ElifStatement]]]), transformRecursive(elsBranch, currentContext))))
                        case k =>
                            totalCarthProduct.flatMap(x => {

                                // Narrow down if condition to the current context
                                newCond = conditionalToConditionalExpr(c, x, true).value
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                val elsBranch = elseTuple.find(e => x.implies(e._1).isTautology(fm)).getOrElse((currentContext, None.asInstanceOf[Option[Conditional[Statement]]]))._2
                                List(Opt(trueF, IfStatement(One(newCond), replaceAndTransform(stmt, x), replaceOptAndId(elif.flatMap(y => handleIfStatementConditional(y, x).asInstanceOf[List[Opt[ElifStatement]]]), x), replaceAndTransform(elsBranch, x))))
                            })

                    }

                // 4. Step: ElifStatements
                case elif@ElifStatement(One(e: Expr), thenBranch: One[Statement]) =>
                    val exprFeatures = computeFeaturesForDuplication(e, currentContext)
                    val newCond = convertToCondExpr(e, exprFeatures, currentContext)
                    List(Opt(trueF, ElifStatement(One(newCond), transformRecursive(thenBranch, currentContext))))
                case elif@ElifStatement(c: Conditional[Expr], thenBranch) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    val statementTuple = conditionalToTuple(thenBranch, currentContext)
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(condFeatures, stmtFeatures), currentContext)
                    carthProduct match {
                        case Nil =>
                            val cond = conditionalTuple.find(y => currentContext.implies(y._1).isTautology(fm)).get._2
                            val exprFeatures = computeNextRelevantFeaturesUnempty(cond, currentContext)
                            val newCond = convertToCondExpr(cond, exprFeatures, currentContext)
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            List(Opt(trueF, ElifStatement(One(replaceOptAndId(newCond, currentContext)), replaceAndTransform(stmt, currentContext))))
                        case k =>
                            carthProduct.map(x => {
                                val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                                val exprFeatures = computeNextRelevantFeaturesUnempty(cond, x)
                                val newCond = convertToCondExpr(cond, exprFeatures, currentContext)
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                Opt(trueF, ElifStatement(One(replaceOptAndId(newCond, x)), replaceAndTransform(stmt, x)))
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
    def handleIfStatement(optIf: Opt[_], currentContext: FeatureExpr = trueF): List[Opt[_]] = {

        // 1. Step
        if (!optIf.feature.equivalentTo(trueF)) {
            optIf.entry match {
                case IfStatement(cond, thenBranch, elifs, elseBranch) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, optIf.feature))), One(CompoundStatement(handleIfStatement(replaceOptAndId(optIf, optIf.feature), optIf.feature).asInstanceOf[List[Opt[Statement]]])), List(), None)))
                case _ =>
                    List()
            }
        } else {
            optIf.entry match {

                // 3. Step
                case i@IfStatement(One(expr), One(stmt), elif, els@None) =>
                    val features = computeFeaturesForDuplication(expr, currentContext)
                    if (features.isEmpty) {
                        List(Opt(trueF, IfStatement(One(replaceOptAndId(expr, currentContext)), One(transformRecursive(stmt, currentContext)), elif.flatMap(x => handleIfStatement(replaceOptAndId(x, currentContext), currentContext)).asInstanceOf[List[Opt[ElifStatement]]], transformRecursive(replaceOptAndId(els, currentContext), currentContext))))
                    } else {
                        features.flatMap(x => List(Opt(trueF,
                            IfStatement(
                                One(NAryExpr(featureToCExpr(fExprDiff(currentContext, x)), List(Opt(trueF, NArySubExpr("&&", replaceOptAndId(expr, x)))))),
                                transformRecursive(replaceOptAndId(One(convertStatementToCompound(stmt)), x), x),
                                elif.flatMap(y => handleIfStatement(replaceOptAndId(y, x), x))
                                    .asInstanceOf[List[Opt[ElifStatement]]],
                                transformRecursive(replaceOptAndId(els, x), x)))))
                    }

                // alternative 3. Step with elseBranch
                case i@IfStatement(One(expr), One(stmt), elif, els@Some(One(elseStmt))) =>
                    val features = computeFeaturesForDuplication(expr, currentContext)
                    if (features.isEmpty) {
                        List(Opt(trueF, IfStatement(One(replaceOptAndId(expr, currentContext)), One(transformRecursive(stmt, currentContext)), elif.flatMap(x => handleIfStatement(replaceOptAndId(x, currentContext), currentContext)).asInstanceOf[List[Opt[ElifStatement]]], transformRecursive(replaceOptAndId(els, currentContext), currentContext))))
                    } else {
                        features.flatMap(x => List(Opt(trueF,
                            IfStatement(
                                One(NAryExpr(featureToCExpr(fExprDiff(currentContext, x)),
                                    List(Opt(trueF, NArySubExpr("&&", replaceOptAndId(expr, x)))))),
                                transformRecursive(replaceOptAndId(One(convertStatementToCompound(stmt)), x), x),
                                elif.flatMap(y => handleIfStatement(replaceOptAndId(y, x), x)).asInstanceOf[List[Opt[ElifStatement]]],
                                transformRecursive(replaceOptAndId(els, x), x)))))
                    }

                // 2. Step
                case IfStatement(c: Conditional[Expr], thenBranch: Conditional[Statement], elif, els) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    val statementTuple = conditionalToTuple(thenBranch, currentContext)
                    var elseTuple = List((FeatureExprFactory.True, None.asInstanceOf[Option[Conditional[Statement]]]))
                    els match {
                        case None =>
                        case Some(One(stmt)) =>
                        case Some(c: Choice[Statement]) =>
                            elseTuple = conditionalToTuple(c, currentContext).map(x => (x._1, Some(One(x._2))))
                    }
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val elsFeatures = elseTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(condFeatures, stmtFeatures, elsFeatures), currentContext)
                    carthProduct.flatMap(x => {
                        val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                        val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                        val elsBranch = elseTuple.find(e => x.implies(e._1).isTautology(fm)).getOrElse((currentContext, None.asInstanceOf[Option[Conditional[Statement]]]))._2
                        handleIfStatement(Opt(trueF, IfStatement(One(NAryExpr(featureToCExpr(fExprDiff(currentContext, x)), List(Opt(trueF, NArySubExpr("&&", replaceOptAndId(cond, x)))))), replaceAndTransform(stmt, x), replaceOptAndId(elif.flatMap(y => handleIfStatement(y, x).asInstanceOf[List[Opt[ElifStatement]]]), x), replaceAndTransform(elsBranch, x))), x)
                    })

                // 4. Step
                case ElifStatement(c: Conditional[Expr], thenBranch) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    val statementTuple = conditionalToTuple(thenBranch, currentContext)
                    val condFeatures = conditionalTuple.map(x => x._1)
                    val stmtFeatures = statementTuple.map(x => x._1)
                    val carthProduct = computeCarthesianProduct(List(stmtFeatures, condFeatures), currentContext)

                    carthProduct match {
                        case Nil =>
                            val cond = conditionalTuple.find(y => currentContext.implies(y._1).isTautology(fm)).get._2
                            val exprFeatures = computeNextRelevantFeaturesUnempty(cond, currentContext)
                            val stmt = One(statementTuple.find(z => currentContext.implies(z._1).isTautology(fm)).get._2)
                            exprFeatures.map(x => Opt(trueF, ElifStatement(One(replaceOptAndId(cond, x)), replaceAndTransform(stmt, x))))
                        case k =>
                            carthProduct.flatMap(x => {
                                val cond = conditionalTuple.find(y => x.implies(y._1).isTautology(fm)).get._2
                                val exprFeatures = computeNextRelevantFeaturesUnempty(cond, x)
                                val stmt = One(statementTuple.find(z => x.implies(z._1).isTautology(fm)).get._2)
                                exprFeatures.map(y => Opt(trueF, ElifStatement(One(replaceOptAndId(cond, y)), replaceAndTransform(stmt, y))))
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
    def handleForStatement(opt: Opt[Statement], currentContext: FeatureExpr = trueF): List[Opt[Statement]] = {

        // 1. Step
        if (!opt.feature.equivalentTo(trueF)) {
            opt.entry match {
                case ForStatement(expr1, expr2, expr3, cond) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleForStatement(replaceOptAndId(opt, opt.feature), opt.feature.and(currentContext)))), List(), None)))
                case _ =>
                    List()
            }
        } else {
            opt.entry match {

                // 2. Step
                case ForStatement(expr1, expr2, expr3, c: Choice[Statement]) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, x._1))), One(CompoundStatement(handleForStatement(Opt(trueF, ForStatement(expr1, expr2, expr3, One(x._2))), x._1))), List(), None)))

                // 3. Step
                case ForStatement(expr1, expr2, expr3, One(stmt: Statement)) =>
                    val features1 = computeFeaturesForDuplication(expr1.getOrElse(EmptyStatement()), currentContext)
                    val newExpr1 = convertToCondExpr(expr1, features1, currentContext)
                    val features2 = computeFeaturesForDuplication(expr2.getOrElse(EmptyStatement()), currentContext)
                    val newExpr2 = convertToCondExpr(expr2, features2, currentContext)
                    val features3 = computeFeaturesForDuplication(expr3.getOrElse(EmptyStatement()), currentContext)
                    val newExpr3 = convertToCondExpr(expr3, features3, currentContext)
                    List(Opt(trueF, ForStatement(newExpr1, newExpr2, newExpr3, One(transformRecursive(replaceOptAndId(stmt, currentContext), currentContext)))))
            }
        }
    }

    /**
     * Handles while / switch / do statements (they have a similar structure).
     */
    def handleWSDStatements(opt: Opt[Statement], currentContext: FeatureExpr = trueF): List[Opt[Statement]] = {
        // 1. Step
        if (!opt.feature.equivalentTo(trueF)) {
            opt.entry match {
                case WhileStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature), opt.feature))), List(), None)))
                case SwitchStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature), opt.feature))), List(), None)))
                case DoStatement(expr, cond) =>
                    List(Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, opt.feature))), One(CompoundStatement(handleWSDStatements(replaceOptAndId(opt, opt.feature), opt.feature))), List(), None)))
                case _ =>
                    List()
            }
        } else {
            opt.entry match {

                // 3. Step
                case WhileStatement(expr, One(stmt: Statement)) =>
                    val features = computeFeaturesForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, features, currentContext)
                    List(Opt(trueF, WhileStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext), currentContext)))))
                case SwitchStatement(expr, One(stmt: Statement)) =>
                    val exprFeatures = computeFeaturesForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, exprFeatures, currentContext)
                    // val caseFeatures = computeCaseFeatures(stmt.asInstanceOf[CompoundStatement], currentContext)
                    val caseFeatures = computeTotalCaseFeatures(stmt.asInstanceOf[CompoundStatement], currentContext)
                    if (caseFeatures.isEmpty) {
                        List(Opt(trueF, SwitchStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext))))))
                    } else {
                        caseFeatures.map(x => Opt(trueF, IfStatement(One(featureToCExpr(fExprDiff(currentContext, x))), One(CompoundStatement(List(Opt(trueF, SwitchStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, x), x))))))), List(), None)))
                    }
                case DoStatement(expr, One(stmt: Statement)) =>
                    val features = computeFeaturesForDuplication(expr, currentContext)
                    val newExpr = convertToCondExpr(expr, features, currentContext)
                    List(Opt(trueF, DoStatement(newExpr, One(transformRecursive(replaceOptAndId(stmt, currentContext), currentContext)))))

                // 2. Step
                case WhileStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(featureToCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                WhileStatement(
                                    expr,
                                    One(x._2))), x._1))),
                            List(),
                            None)))
                case SwitchStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(featureToCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                SwitchStatement(
                                    expr,
                                    One(x._2))), x._1))),
                            List(),
                            None)))
                case DoStatement(expr, c: Conditional[Statement]) =>
                    val conditionalTuple = conditionalToTuple(c, currentContext)
                    conditionalTuple.map(x => Opt(trueF,
                        IfStatement(
                            One(featureToCExpr(fExprDiff(currentContext, x._1))),
                            One(CompoundStatement(handleWSDStatements(Opt(trueF,
                                DoStatement(
                                    expr,
                                    One(x._2))), x._1))),
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
    def handleDeclarations(optDeclaration: Opt[Declaration], currentContext: FeatureExpr = trueF, isTopLevel: Boolean = false): List[Opt[Declaration]] = {
        optDeclaration.entry match {
            case Declaration(declSpecs, init) =>
                val declarationFeature = optDeclaration.feature
                val newDeclSpecs = declSpecs.map(x => if (optDeclaration.feature.equivalentTo(currentContext) && currentContext.implies(x.feature).isTautology(fm)) x
                else {
                    val relevantFeature = x.feature.and(declarationFeature)
                    x match {
                        case o@Opt(ft, EnumSpecifier(Some(i: Id), Some(enums))) =>
                            val newEnums = Some(enums.map(x => Opt(trueF, convertEnumId(x.entry, relevantFeature))))
                            if (defuse.containsKey(i)) {
                                addIdUsages(i, declarationFeature)
                                Opt(ft, EnumSpecifier(Some(renameIdentifier(i, declarationFeature)), newEnums))
                            } else {
                                Opt(ft, EnumSpecifier(Some(i), newEnums))
                            }
                        case Opt(ft, EnumSpecifier(None, Some(enums))) =>
                            val newEnums = Some(enums.map(x => Opt(trueF, convertEnumId(x.entry, relevantFeature))))
                            Opt(ft, EnumSpecifier(None, newEnums))
                        case o@Opt(ft, EnumSpecifier(Some(i: Id), k)) =>
                            if (defuse.containsKey(i)) {
                                addIdUsages(i, relevantFeature)
                                Opt(ft, EnumSpecifier(Some(renameIdentifier(i, relevantFeature)), k))
                            } else {
                                o
                            }
                        case o@Opt(ft, StructOrUnionSpecifier(a, Some(i: Id), b, c, d)) =>
                            if (init.isEmpty || defuse.containsKey(i)) {
                                addIdUsages(i, relevantFeature)
                                Opt(ft, StructOrUnionSpecifier(a, Some(renameIdentifier(i, relevantFeature)), b, c, d))
                            } else {
                                o
                            }

                        case k =>
                            k
                    }
                })

                val tmpDecl = Declaration(newDeclSpecs, init)
                val features = computeFeaturesForDuplication(tmpDecl, declarationFeature.and(currentContext), isTopLevel)
                if ((features.isEmpty) && isTopLevel && !currentContext.and(optDeclaration.feature).equivalentTo(trueF, fm)) {
                    if (isTopLevel && !currentContext.and(optDeclaration.feature).equivalentTo(trueF, fm)) {
                        if (declSpecs.exists(x => x.entry.isInstanceOf[TypedefSpecifier])) {
                            incOptionalTypedefs
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[StructOrUnionSpecifier])) {
                            incOptionalStructUnions
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[EnumSpecifier])) {
                            incOptionalEnums
                        } else {
                            // Function forward declarations
                            if (init.exists(x => x.entry.declarator.extensions.exists(y => y.entry.isInstanceOf[DeclIdentifierList] || y.entry.isInstanceOf[DeclParameterDeclList]))) {
                                incOptionalForwardFunctions()
                            } else {
                                incOptionalVariables
                            }
                        }
                    }
                }
                if (exceedsThreshold(features)) {
                    List(Opt(trueF, tmpDecl))
                } else if (!features.isEmpty) {
                    val result = features.map(x => Opt(trueF, transformRecursive(convertId(replaceOptAndId(tmpDecl, x), x), x)))
                    result
                } else {
                    val tst = replaceOptAndId(tmpDecl, declarationFeature)
                    val tst2 = convertId(tst, declarationFeature)
                    val result = List(Opt(trueF, transformRecursive(tst2)))
                    result
                }
        }
    }

    /**
     * Temporary function, needs improvement and verification. Ignore for now.
     */
    def handleDeclarations_new(optDeclaration: Opt[Declaration], currentContext: FeatureExpr = trueF): List[Opt[Declaration]] = {
        def convertSpecifiers(declSpecs: List[Opt[Specifier]], feat: FeatureExpr = trueF): List[Opt[Specifier]] = {
            if (!feat.equivalentTo(trueF)) {
                declSpecs.map(x => x match {
                    case o@Opt(ft, EnumSpecifier(Some(i: Id), k)) =>
                        if (defuse.containsKey(i)) {
                            addIdUsages(i, feat)
                            Opt(ft, EnumSpecifier(Some(renameIdentifier(i, feat)), k))
                        } else {
                            o
                        }
                    case o@Opt(ft, StructOrUnionSpecifier(a, Some(i: Id), b, c, d)) =>
                        if (defuse.containsKey(i)) {
                            addIdUsages(i, feat)
                            Opt(ft, StructOrUnionSpecifier(a, Some(renameIdentifier(i, feat)), b, c, d))
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
            newOptDecl = replaceOptAndId(Opt(trueF, optDeclaration.entry), optDeclaration.feature)
            context = optDeclaration.feature
        } else {
            context = trueF
        }

        // 2. Step
        val features = computeFeaturesForDuplication(newOptDecl.entry, context)
        val specs = convertSpecifiers(newOptDecl.entry.declSpecs, context)
        val inits = newOptDecl.entry.init
        if (!features.isEmpty) {
            features.map(x => replaceOptAndId(Opt(trueF, transformRecursive(Declaration(convertSpecifiers(specs, x), convertIds(inits, x)), x)), x))
        } else {
            List(replaceOptAndId(Opt(trueF, transformRecursive(Declaration(convertSpecifiers(specs, context), convertIds(inits, context)), context)), context))
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
            if (!optFunction.feature.equivalentTo(trueF)) {
                optFunction.entry match {
                    case fd@FunctionDef(spec, decl, par, stmt) =>
                        handleFunctionRec(Opt(trueF, replaceOptAndId(fd, optFunction.feature)), optFunction.feature)
                    case nfd@NestedFunctionDef(isAuto, spec, decl, par, stmt) =>
                        handleFunctionRec(Opt(trueF, replaceOptAndId(nfd, optFunction.feature)), optFunction.feature)
                }
            } else {
                // 2. Step
                optFunction.entry match {
                    case fd@FunctionDef(spec, decl, par, stmt) =>
                        val features = computeFeaturesForDuplication(fd, currentContext).filterNot(FeatureExprFactory.False.equals(_))
                        if (features.isEmpty) {
                            List(Opt(trueF, FunctionDef(replaceOptAndId(spec, currentContext), replaceOptAndId(convertStructId(decl, currentContext), currentContext), replaceOptAndId(par, currentContext), transformRecursive(replaceOptAndId(stmt, currentContext), currentContext))))
                        } else {
                            features.map(x => Opt(trueF, FunctionDef(replaceOptAndId(spec, x), replaceOptAndId(convertStructId(decl, x), x), replaceOptAndId(par, x), transformRecursive(replaceOptAndId(stmt, x), x))))
                        }
                    case nfd@NestedFunctionDef(isAuto, spec, decl, par, stmt) =>
                        val features = computeFeaturesForDuplication(nfd, currentContext)
                        if (features.isEmpty) {
                            List(Opt(trueF, NestedFunctionDef(isAuto, replaceOptAndId(spec, currentContext), replaceOptAndId(convertStructId(decl, currentContext), currentContext), replaceOptAndId(par, currentContext), transformRecursive(replaceOptAndId(stmt, currentContext), currentContext))))
                        } else {
                            features.map(x => Opt(trueF, NestedFunctionDef(isAuto, replaceOptAndId(spec, x), replaceOptAndId(convertStructId(decl, x), x), replaceOptAndId(par, x), transformRecursive(replaceOptAndId(stmt, x), x))))
                        }
                }
            }
        }
        handleFunctionRec(newFunction, context)
    }

    /**
     * Returns a set of enabled, disabled and not occoring features from a configuration file.
     */
    def getFeaturesFromConfiguration(@SuppressWarnings(Array("unchecked")) file: File, fm: FeatureModel = FeatureExprFactory.empty, features: Set[SingleFeatureExpr] = Set()): (List[SingleFeatureExpr], List[SingleFeatureExpr], List[SingleFeatureExpr]) = {
        val correctFeatureModelIncompatibility = false
        var ignoredFeatures = 0
        var changedAssignment = 0
        var totalFeatures = 0
        var fileEx: FeatureExpr = FeatureExprFactory.True
        var trueFeatures: Set[SingleFeatureExpr] = Set()
        var falseFeatures: Set[SingleFeatureExpr] = Set()

        val enabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*)=y")
        val disabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*)=n")
        for (line <- Source.fromFile(file).getLines().filterNot(_.startsWith("#")).filterNot(_.isEmpty)) {
            totalFeatures += 1
            var matcher = enabledPattern.matcher(line)
            if (matcher.matches()) {
                val name = matcher.group(1)
                val feature = FeatureExprFactory.createDefinedExternal(name)
                var fileExTmp = fileEx.and(feature)
                if (correctFeatureModelIncompatibility) {
                    val isSat = fileExTmp.isSatisfiable(fm)
                    println(name + " " + (if (isSat) "sat" else "!sat"))
                    if (!isSat) {
                        fileExTmp = fileEx.andNot(feature)
                        println("disabling feature " + feature)
                        //fileExTmp = fileEx; println("ignoring Feature " +feature)
                        falseFeatures += feature
                        changedAssignment += 1
                    } else {
                        trueFeatures += feature
                    }
                } else {
                    trueFeatures += feature
                }
                fileEx = fileExTmp
            } else {
                matcher = disabledPattern.matcher(line)
                if (matcher.matches()) {
                    val name = matcher.group(1)
                    val feature = FeatureExprFactory.createDefinedExternal(name)
                    var fileExTmp = fileEx.andNot(feature)
                    if (correctFeatureModelIncompatibility) {
                        val isSat = fileEx.isSatisfiable(fm)
                        println("! " + name + " " + (if (isSat) "sat" else "!sat"))
                        if (!isSat) {
                            fileExTmp = fileEx.and(feature)
                            println("SETTING " + name + "=y")
                            trueFeatures += feature
                            changedAssignment += 1
                        } else {
                            falseFeatures += feature
                        }
                    } else {
                        falseFeatures += feature
                    }
                    fileEx = fileExTmp
                } else {
                    ignoredFeatures += 1
                    //println("ignoring line: " + line)
                }
            }
            //println(line)
        }

        if (features.isEmpty) {
            (trueFeatures.toList, falseFeatures.toList, List())
        } else {
            (features.filter(trueFeatures.contains).toList, features.filter(falseFeatures.contains).toList, features.filterNot((trueFeatures ++ falseFeatures).contains).toList)
        }
    }

    /**
     * Creates a single boolean expression for given .config file.
     */
    def getConfigFormula(@SuppressWarnings(Array("unchecked")) file: File, fm: FeatureModel = FeatureExprFactory.empty, features: Set[SingleFeatureExpr] = Set()): FeatureExpr = {
        val featuresFromConfig = getFeaturesFromConfiguration(file, fm, features)
        val trueFeatures = featuresFromConfig._1
        val falseFeatures = featuresFromConfig._2.map(x => x.not())
        var otherFeatures: List[FeatureExpr] = featuresFromConfig._3
        if (!defaultFeatureSelection) {
            otherFeatures = otherFeatures.map(x => x.not())
        }
        (trueFeatures ++ falseFeatures ++ otherFeatures).foldLeft(trueF)((first, second) => first.and(second))
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
     * Checks if the given ast contains any opt or choice nodes which contain variability in the form of #ifdefs.
     * @param ast
     * @return
     */
    def hasVariableNodes(ast: AST): Boolean = {
        val r = manytd(query {
            case Opt(ft, _) =>
                if (!ft.equals(trueF)) {
                    return true
                }
            case Choice(ft, _, _) =>
                if (!ft.equals(trueF)) {
                    return true
                }
        })
        r(ast)
        false
    }
}