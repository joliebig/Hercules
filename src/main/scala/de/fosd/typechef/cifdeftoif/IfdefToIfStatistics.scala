package de.fosd.typechef.crewrite

import java.io._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExprFactory, FeatureExpr}
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c.Enumerator
import de.fosd.typechef.parser.c.EnumSpecifier
import de.fosd.typechef.parser.c.StructOrUnionSpecifier
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.IfStatement
import de.fosd.typechef.parser.c.DeclParameterDeclList
import de.fosd.typechef.parser.c.Declaration
import de.fosd.typechef.parser.c.DeclIdentifierList
import de.fosd.typechef.parser.c.StructDeclaration
import de.fosd.typechef.parser.c.ElifStatement
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.TypedefSpecifier
import scala.reflect.ClassTag

trait IfdefToIfStatisticsInterface {

    def init(fm: FeatureModel) {}

    def countDuplications[T <: Any](originalElement: T, growth: Int, isTopLevel: Boolean) = {}

    def incOptionalTypedefs() {}

    def incOptionalStructUnions() {}

    def incOptionalEnums() {}

    def incOptionalForwardFunctions() {}

    def incOptionalVariables() {}

    def incRenamings() {}

    def incRenamingUsages(usages: Int) {}

    def exportStatistics(tuple: (List[(String, String)], List[(String, String)])) = {}

    def createCsvDuplicationString(ast: TranslationUnit, fileName: String): (String, String) = {
        ("", "")
    }

    def createCsvEntry(source_ast: AST, new_ast: AST, fileName: String, lexAndParseTime: Long, transformTime: Long, noOfFeatures: Int): String = { "" }

    def updateIfdeftoifStatistics(source_ast: TranslationUnit, new_ast: TranslationUnit, fileName: String, lexAndParseTime: Long, transformTime: Long, noOfFeatures: Int, statisticsPath: String, topLevelStatisticsPath: String): (List[(String, String)], List[(String, String)]) = { (List(), List()) }
}

trait IfdefToIfStatistics extends IfdefToIfStatisticsInterface with IOUtilities {
    val trueF2 = FeatureExprFactory.True
    var fm2: FeatureModel = FeatureExprFactory.empty

    // Declarations
    var noOfStructUnionDuplications = 0
    var noOfOptionalStructUnions = 0
    var noOfStructUnionsToDuplicate = 0

    var noOfTypedefDuplications = 0
    var noOfOptionalTypedefs = 0
    var noOfTypedefsToDuplicate = 0

    var noOfEnumDuplications = 0
    var noOfOptionalEnums = 0
    var noOfEnumsToDuplicate = 0

    var noOfVariableDuplications = 0
    var noOfOptionalVariables = 0
    var noOfVariablesToDuplicate = 0

    var noOfFunctionDuplications = 0
    var noOfOptionalFunctions = 0
    var noOfFunctionsToDuplicate = 0

    var noOfForwardFunctionDuplications = 0
    var noOfOptionalForwardFunctions = 0
    var noOfForwardFunctionsToDuplicate = 0

    var noOfEnumeratorDuplications = 0
    var noOfOptionalEnumerators = 0
    var noOfEnumeratorsToDuplicate = 0

    // Statements
    var noOfIfStatementDuplications = 0
    var noOfWhileStatementDuplications = 0
    var noOfSwitchStatementDuplications = 0
    var noOfForStatementDuplications = 0
    var noOfDoStatementDuplications = 0
    var noOfReturnStatementDuplications = 0
    var noOfExprStatementDuplications = 0

    // Techniques
    var noOfRenamings = 0
    var noOfRenamingUsages = 0


    /**
     * Returns the header string for the csv file which includes various statistics of the ifdef to if transformation
     * process.
     * @return
     */
    def getCSVHeader: String = {
        "File name,Number of features,Number of AST nodes before,Number of AST nodes after,AST node difference,Declarations before,Annotated declarations,Annotated declaration ratio,Declarations afterwards,Declaration growth,Functions,Annotated functions,Annotated function ratio,Functions afterwards,Function growth,If/Elif statements before,If/Elif statements afterwards,If/Elif statement growth,Renamed identifier declarations,Renamed identifier usages,Parsing time,Ifdeftoif time\n"
    }

    /**
     * Creates a comma separated string from a list of strings.
     * @param input
     * @return
     */
    private def createCommaSeparatedString(input: List[Any]): String = {
        input.map(x => x.toString) mkString ","
    }

    /**
     * Counts the number of variable declarations (delcarations, enumerators, structdeclarations) in given AST element ast.
     */
    private def countNumberOfVariableDeclarations(ast: AST): Long = {
        def countNumberHelper(a: Any, currentContext: FeatureExpr = trueF2): Long = {
            val i = 0
            a match {
                case l: List[_] => l.map(x => countNumberHelper(x, currentContext)).sum
                case _: FeatureExpr => 0
                case o@Opt(ft, entry: AST) =>
                    if ((ft.implies(currentContext).isTautology(fm2) && !ft.equivalentTo(currentContext)) && (entry.isInstanceOf[Declaration] || entry.isInstanceOf[Enumerator] || entry.isInstanceOf[StructDeclaration])) {
                        1 + entry.productIterator.toList.map(x => countNumberHelper(x, ft)).sum
                    } else {
                        entry.productIterator.toList.map(x => countNumberHelper(x, ft)).sum
                    }
                case p: Product =>
                    p.productIterator.toList.map(x => countNumberHelper(x, currentContext)).sum
                case _ =>
                    0
            }
        }
        countNumberHelper(ast)
    }

    def computeDifference(before: Int, after: Int): Double = {
        ((after - before) / (before.toDouble))
    }

    def computeDifference(before: Long, after: Long): Double = {
        ((after - before) / (before.toDouble))
    }

    private def countNumberOfVariableElements[T <: Any](ast: AST)(implicit m: ClassTag[T]): Long = {
        var res = 0
        var currentContext = trueF2
        val variable = manytd(query {
            case l: List[_] =>
            case o@Opt(ft, entry: AST) =>
                if ((ft.implies(currentContext).isTautology(fm2) && !ft.equivalentTo(currentContext)) && m.runtimeClass.isInstance(entry)) {
                    res = res + 1
                }
                currentContext = ft
            case _ =>
        })

        variable(ast)
        res
    }

    /**
     * Counts the number of astNodes, declarations, functions, if/elif statements in given AST element ast.
     */
    def getNumberOfElements(ast: AST): (Long, Long, Long, Long) = {
        var astNodes: Long = 0
        var declarations: Long = 0
        var functions: Long = 0
        var ifElifStatements: Long = 0

        val variable = manytd(query {
            case l: List[_] =>
            case p: Product =>
                astNodes = astNodes + 1
                if (p.isInstanceOf[Declaration] || p.isInstanceOf[Enumerator] || p.isInstanceOf[StructDeclaration]) {
                    declarations = declarations + 1
                }
                if (p.isInstanceOf[IfStatement] || p.isInstanceOf[ElifStatement]) {
                    ifElifStatements = ifElifStatements + 1
                }
                if (p.isInstanceOf[FunctionDef]) {
                    functions = functions + 1
                }
            case _ =>
        })

        variable(ast)

        val result = (astNodes, declarations, functions, ifElifStatements)
        result
    }

    override def init(featureModel: FeatureModel) = {
        fm2 = featureModel
    }

    override def updateIfdeftoifStatistics(source_ast: TranslationUnit, new_ast: TranslationUnit, fileName: String, lexAndParseTime: Long, transformTime: Long, noOfFeatures: Int, statisticsPath: String, topLevelStatisticsPath: String): (List[(String, String)], List[(String, String)]) = {
        var toWrite: List[(String, String)] = List()
        var toAppend: List[(String, String)] = List()
        if (!(new File(statisticsPath).exists)) {
            toWrite = (statisticsPath, getCSVHeader) :: toWrite
            //writeToFile(statisticsPath, getCSVHeader)
        }

        val csvEntry = createCsvEntry(source_ast, source_ast, fileName, lexAndParseTime, transformTime, noOfFeatures)
        toAppend = (statisticsPath, csvEntry) :: toAppend
        //appendToFile(statisticsPath, csvEntry)

        val csvDuplications = createCsvDuplicationString(source_ast, fileName)
        if (!(new File(topLevelStatisticsPath).exists)) {
            toWrite = (topLevelStatisticsPath, csvDuplications._1) :: toWrite
        }
        toAppend = (topLevelStatisticsPath, csvDuplications._2) :: toAppend
        (toWrite, toAppend)
    }

    /**
     * Counts number of duplications of given AST element type 'T' in given AST.
     */
    override def countDuplications[T <: Any](originalElement: T, growth: Int, isTopLevel: Boolean) = {
        if (isTopLevel) {
            // val growth = newNumber - 1
            if (growth > 1) {
                originalElement match {
                    case d@Declaration(declSpecs, inits) =>
                        if (declSpecs.exists(x => x.entry.isInstanceOf[TypedefSpecifier])) {
                            noOfTypedefDuplications = noOfTypedefDuplications + growth
                            noOfTypedefsToDuplicate = noOfTypedefsToDuplicate + 1
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[StructOrUnionSpecifier])) {
                            noOfStructUnionDuplications = noOfStructUnionDuplications + growth
                            noOfStructUnionsToDuplicate = noOfStructUnionsToDuplicate + 1
                        } else if (declSpecs.exists(x => x.entry.isInstanceOf[EnumSpecifier])) {
                            noOfEnumDuplications = noOfEnumDuplications + growth
                            noOfEnumsToDuplicate = noOfEnumsToDuplicate + 1
                        } else {
                            // Function forward declarations
                            if (inits.exists(x => x.entry.declarator.extensions.exists(y => y.entry.isInstanceOf[DeclIdentifierList] || y.entry.isInstanceOf[DeclParameterDeclList]))) {
                                noOfForwardFunctionDuplications = noOfForwardFunctionDuplications + growth
                                noOfForwardFunctionsToDuplicate = noOfForwardFunctionsToDuplicate + 1
                            } else {
                                noOfVariableDuplications = noOfVariableDuplications + growth
                                noOfVariablesToDuplicate = noOfVariablesToDuplicate + 1
                            }
                        }
                    case f@FunctionDef(_, _, _, _) =>
                        noOfFunctionDuplications = noOfFunctionDuplications + growth
                        noOfFunctionsToDuplicate = noOfFunctionsToDuplicate + 1
                    /*case e@Enumerator(_, _) =>
                        noOfEnumeratorDuplications = noOfEnumeratorDuplications + growth
                        noOfEnumeratorsToDuplicate = noOfEnumeratorsToDuplicate + 1
                    case ss@SwitchStatement(_, _) =>
                        noOfSwitchStatementDuplications = noOfSwitchStatementDuplications + growth
                    case is@IfStatement(_, _, _, _) =>
                        noOfIfStatementDuplications = noOfIfStatementDuplications + growth
                    case ws@WhileStatement(_, _) =>
                        noOfWhileStatementDuplications = noOfWhileStatementDuplications + growth
                    case rs@ReturnStatement(_) =>
                        noOfReturnStatementDuplications = noOfReturnStatementDuplications + growth
                    case ds@DoStatement(_, _) =>
                        noOfDoStatementDuplications = noOfDoStatementDuplications + growth
                    case fs@ForStatement(_, _, _, _) =>
                        noOfForStatementDuplications = noOfForStatementDuplications + growth
                    case es@ExprStatement(_) =>
                        noOfExprStatementDuplications = noOfExprStatementDuplications + growth*/

                    case _ =>
                }
            }
        }
    }

    /**
     * Creates a string with all statistical data from the ifdef to if configuration.
     */
    override def createCsvEntry(source_ast: AST, new_ast: AST, fileName: String, lexAndParseTime: Long, transformTime: Long, noOfFeatures: Int): String = {
        val oldNumbers = getNumberOfElements(source_ast)
        val newNumbers = getNumberOfElements(new_ast)
        val numberOfAstElements = oldNumbers._1
        val newNumberOfAstElements = newNumbers._1
        val astGrowth = computeDifference(numberOfAstElements, newNumberOfAstElements)

        val numberOfDecls = oldNumbers._2
        val numberOfVariableDecls = countNumberOfVariableDeclarations(source_ast)
        val numberOfFunctions = oldNumbers._3
        val numberOfVariableFunctions = countNumberOfVariableElements[FunctionDef](source_ast)
        val numberOfIfsAndElifs = oldNumbers._4

        val newNumberOfDecls = newNumbers._2
        val newNumberOfFunctions = newNumbers._3
        val newNumberOfIfsAndElifs = newNumbers._4

        val variableToTotalDecls = numberOfVariableDecls / numberOfDecls.toDouble
        val declarationGrowth = computeDifference(numberOfDecls, newNumberOfDecls)

        val variableToTotalFunctions = numberOfVariableFunctions / numberOfFunctions.toDouble
        val functionGrowth = computeDifference(numberOfFunctions, newNumberOfFunctions)

        val ifAndElifGrowth = computeDifference(numberOfIfsAndElifs, newNumberOfIfsAndElifs)

        createCommaSeparatedString(List(fileName, noOfFeatures, numberOfAstElements, newNumberOfAstElements, astGrowth, numberOfDecls, numberOfVariableDecls, variableToTotalDecls, newNumberOfDecls, declarationGrowth, numberOfFunctions, numberOfVariableFunctions, variableToTotalFunctions, newNumberOfFunctions, functionGrowth, numberOfIfsAndElifs, newNumberOfIfsAndElifs, ifAndElifGrowth, noOfRenamings, noOfRenamingUsages, lexAndParseTime, transformTime)) ++ "\n"
    }

    override def incOptionalTypedefs() {
        noOfOptionalTypedefs = noOfOptionalTypedefs + 1
    }

    override def incOptionalStructUnions() {
        noOfOptionalStructUnions = noOfOptionalStructUnions + 1
    }

    override def incOptionalEnums() {
        noOfOptionalEnums = noOfOptionalEnums + 1
    }

    override def incOptionalForwardFunctions() {
        noOfOptionalForwardFunctions = noOfOptionalForwardFunctions + 1
    }

    override def incOptionalVariables() {
        noOfOptionalVariables = noOfOptionalVariables + 1
    }

    override def incRenamings() {
        noOfRenamings = noOfRenamings + 1
    }

    override def incRenamingUsages(usages: Int) {
        noOfRenamingUsages = noOfRenamingUsages + usages
    }

    /**
     * Takes two String tuples t1, t2 and writes the content of t1._2 into the file at path t1._1 and appends
     * the content of t2._2 into the file at path t2._1
     */
    override def exportStatistics(tuple: (List[(String, String)], List[(String, String)])) = {
        tuple._1.foreach(x => writeToFile(x._1, x._2))
        tuple._2.foreach(x => appendToFile(x._1, x._2))
    }

    /**
     * Creates a string with all duplication data from the ifdef to if configuration.
     */
    override def createCsvDuplicationString(ast: TranslationUnit, fileName: String): (String, String) = {
        val headers = List("FileName", "Typedefs", "Optional Typedefs", "Typedefs to duplicate", "Typedef duplications",
            "StructUnions", "Optional StructUnions", "StructUnions to duplicate", "StructUnion duplications",
            "Enums", "Optional Enums", "Enums to duplicate", "Enum duplications",
            "Functions", "Optional Functions", "Functions to duplicate", "Function duplications",
            "Function forward declarations", "Optional function forward declarations", "Function forward declarations to duplicate", "Function forward declaration duplications",
            "Variables", "Optional Variables", "Variables to duplicate", "Variable Duplications")
        val elements = getNumberOfTopLevelDeclarationElements(ast)
        val numbers = fileName :: List(elements._1, noOfOptionalTypedefs, noOfTypedefsToDuplicate, noOfTypedefDuplications,
            elements._2, noOfOptionalStructUnions, noOfStructUnionsToDuplicate, noOfStructUnionDuplications,
            elements._3, noOfOptionalEnums, noOfEnumsToDuplicate, noOfEnumDuplications,
            elements._4, noOfOptionalFunctions, noOfFunctionsToDuplicate, noOfFunctionDuplications,
            elements._5, noOfOptionalForwardFunctions, noOfForwardFunctionsToDuplicate, noOfForwardFunctionDuplications,
            elements._6, noOfOptionalVariables, noOfVariablesToDuplicate, noOfVariableDuplications).map(x => x.toString)
        (createCommaSeparatedString(headers) ++ "\n", createCommaSeparatedString(numbers) ++ "\n")
    }

    /**
     * Counts the number of different declarations at the very top level in the AST: typefdef, structOrUnions, enums,
     * functions, foward function declarations, variables.
     */
    private def getNumberOfTopLevelDeclarationElements(ast: TranslationUnit): (Long, Long, Long, Long, Long, Long) = {
        var typedefs: Long = 0
        var structOrUnions: Long = 0
        var enums: Long = 0
        var functions: Long = 0
        var forwardFunctions: Long = 0
        var variables: Long = 0

        ast.defs.foreach(x => x.entry match {
            case d@Declaration(declSpecs, inits) =>
                if (declSpecs.exists(x => x.entry.isInstanceOf[TypedefSpecifier])) {
                    typedefs = typedefs + 1
                } else if (declSpecs.exists(x => x.entry.isInstanceOf[StructOrUnionSpecifier])) {
                    structOrUnions = structOrUnions + 1
                } else if (declSpecs.exists(x => x.entry.isInstanceOf[EnumSpecifier])) {
                    enums = enums + 1
                } else {
                    // Function forward declaration
                    if (inits.exists(x => x.entry.declarator.extensions.exists(y => y.entry.isInstanceOf[DeclIdentifierList] || y.entry.isInstanceOf[DeclParameterDeclList]))) {
                        forwardFunctions = forwardFunctions + 1
                    } else {
                        variables = variables + 1
                    }
                }
            case f@FunctionDef(_, _, _, _) =>
                functions = functions + 1
            case _ =>
        })
        val result = (typedefs, structOrUnions, enums, functions, forwardFunctions, variables)
        result
    }
}