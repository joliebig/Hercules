package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
import org.apache.logging.log4j.LogManager
import org.kiama.rewriting.Rewriter._

import scala.collection.JavaConversions._

trait IfdefToIfPerformanceInterface {

    def insertPerfFunctCalls(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        cmpstmt
    }

    def insertPerfMainFunctCalls(cmpstmt: CompoundStatement): CompoundStatement = {
        cmpstmt
    }

    def removePerformanceBeforeFunction(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        stmts
    }

    def removePerformanceAfterFunction(stmts: List[Opt[Statement]]): (List[Opt[Statement]], Int) = {
        (stmts, 0)
    }

    def combinePerformancePair(firstStmts: CompoundStatement, secondStmts: CompoundStatement): CompoundStatement = {
        CompoundStatement(firstStmts.innerStatements ++ secondStmts.innerStatements)
    }

    def printPerformanceCounter(path: String) = {
        // Nothing
    }

    def insertPerformanceCounter(suffix: PostfixSuffix): PostfixSuffix = {
        suffix
    }

    def insertPerformanceCounter(cmpStmt: CompoundStatement): CompoundStatement = {
        cmpStmt
    }

    def updatePerformancePrependString(prependString: String, createIncludeDirective: (String) => String, path: String, fileName: String): String = {
        ""
    }

    def fixBreakAndContinues[T <: Product](t: T, ifdefDepth: Int = 0, forDoWhileDepth: Int = 0, switchIfdefDepth: Int = 0, lastStmtWasSwitch: Boolean = false): T = {
        t
    }

    def correctPerformanceFeaturePrefix(newPrefix: String) = {
        // Nothing
    }
}

trait IfdefToIfPerformance extends IfdefToIfPerformanceInterface with IOUtilities {
    private lazy val logger2 = LogManager.getLogger(this.getClass.getName)
    val trueF3 = FeatureExprFactory.True
    val functionBeforeName = "id2iperf_time_before"
    val functionBeforeCounterName = "id2iperf_time_before_counter"
    val functionAfterName = "id2iperf_time_after"
    val functionStartName = "id2iperf_time_start"
    val functionEndName = "id2iperf_time_end"
    val returnMacroName = "id2iperf_return"
    private val featureStructInitializedName2 = "id2i"
    private val performanceIncludeFileName = "perf_measuring.c"
    private val noIncludeFileName = "noincludes.c"
    private val includeFileName = "includes.c"
    private val performanceCmpStmtContextMap: java.util.LinkedHashMap[CompoundStatement, String] = new java.util.LinkedHashMap()
    private val returnMacro = "#define id2iperf_return(expr, stmts) __typeof__(expr) ____RetTmp = expr; stmts; return ____RetTmp;\n"
    private var featurePrefix2 = "f_"
    private var performanceCounter = 0
    private var insertPerformanceCounter = true

    override def correctPerformanceFeaturePrefix(newPrefix: String): Unit = {
        featurePrefix2 = newPrefix
    }

    override def fixBreakAndContinues[T <: Product](t: T, ifdefDepth: Int = 0, forDoWhileIfdefDepth: Int = 0, switchIfdefDepth: Int = 0, lastStmtWasSwitch: Boolean = false): T = {
        val transformation = alltd(rule {
            case Opt(ft, i@IfStatement(cond, _, _, _)) =>
                if (isIfdeftoifCondition2(cond)) {
                    Opt(ft, fixBreakAndContinues(i, ifdefDepth + 1, forDoWhileIfdefDepth, switchIfdefDepth, lastStmtWasSwitch))
                } else {
                    Opt(ft, fixBreakAndContinues(i, ifdefDepth, forDoWhileIfdefDepth, switchIfdefDepth, lastStmtWasSwitch))
                }
            case Opt(ft, s: ForStatement) =>
                Opt(ft, fixBreakAndContinues(s, ifdefDepth, ifdefDepth, switchIfdefDepth, false))
            case Opt(ft, s: DoStatement) =>
                Opt(ft, fixBreakAndContinues(s, ifdefDepth, ifdefDepth, switchIfdefDepth, false))
            case Opt(ft, s: WhileStatement) =>
                Opt(ft, fixBreakAndContinues(s, ifdefDepth, ifdefDepth, switchIfdefDepth, false))
            case Opt(ft, s: SwitchStatement) =>
                Opt(ft, fixBreakAndContinues(s, ifdefDepth, forDoWhileIfdefDepth, ifdefDepth, true))
            case CompoundStatement(innerStmts) =>
                CompoundStatement(innerStmts.flatMap {
                    case Opt(ft, ReturnStatement(None)) if ifdefDepth > 0 =>
                        var result: List[Opt[Statement]] = List()
                        val afterStmt = Opt(trueF3, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant("0"))))))))
                        for (counter <- 0 until ifdefDepth - forDoWhileIfdefDepth) {
                            result = afterStmt :: result
                        }
                        result ++ List(Opt(ft, ReturnStatement(None)))
                    case Opt(ft, ReturnStatement(Some(expr))) if ifdefDepth > 0 =>
                        var result: List[Opt[Statement]] = List()
                        val afterStmt = Opt(trueF3, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant("0"))))))))
                        val returnMacroCall = Opt(ft, ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, expr), Opt(trueF3, Id(functionAfterName + "(" + "0" + ")"))))))))
                        for (counter <- 1 until ifdefDepth - forDoWhileIfdefDepth) {
                            result = afterStmt :: result
                        }
                        result ++ List(returnMacroCall)
                    case Opt(ft, ContinueStatement()) if ifdefDepth > 0 =>
                        var result: List[Opt[Statement]] = List()
                        val afterStmt = Opt(trueF3, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant("0"))))))))
                        for (counter <- 0 until ifdefDepth - forDoWhileIfdefDepth) {
                            result = afterStmt :: result
                        }
                        result ++ List(Opt(ft, ContinueStatement()))
                    case Opt(ft, BreakStatement()) if ifdefDepth > 0 =>
                        var result: List[Opt[Statement]] = List()
                        val afterStmt = Opt(trueF3, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant("0"))))))))
                        var limit = 0
                        if (lastStmtWasSwitch) {
                            limit = switchIfdefDepth
                        } else {
                            limit = forDoWhileIfdefDepth
                        }
                        for (counter <- 1 until ifdefDepth - limit) {
                            result = afterStmt :: result
                        }
                        result ++ List(Opt(ft, BreakStatement()))
                    case k =>
                        List(fixBreakAndContinues(k, ifdefDepth, forDoWhileIfdefDepth, switchIfdefDepth, lastStmtWasSwitch))
                })
        })
        transformation(t).getOrElse(t).asInstanceOf[T]
    }

    override def updatePerformancePrependString(prependString: String, createIncludeDirective: (String) => String, path: String, fileName: String): String = {
        var result = returnMacro
        if (fileName.startsWith("sqlite")) {
            result += createIncludeDirective(path ++ noIncludeFileName)
        } else {
            result += createIncludeDirective(path ++ includeFileName)
        }
        result += createIncludeDirective(path ++ performanceIncludeFileName)
        result
    }

    override def printPerformanceCounter(path: String) = {
        var resultString = ""
        if (insertPerformanceCounter) {
            //println("Number of performance measuring nodes: " + performanceCounter)
            resultString += "Number of performance measuring nodes: " + performanceCounter.toString() + "\n"
            var currentIndex = 0
            performanceCmpStmtContextMap.foreach(x => {
                //println("Node " + currentIndex.toString + ":\t" + x._2 + " -> " + x._1)
                resultString += "Node " + currentIndex.toString + ":\t" + x._2 + " -> " + x._1 + "\n"
                currentIndex += 1
            })
        }
        writeToFile(path ++ "_nodes.txt", resultString)
    }

    override def insertPerformanceCounter(suffix: PostfixSuffix): PostfixSuffix = {
        if (insertPerformanceCounter) {
            suffix match {
                case FunctionCall(ExprList(List(opt1@Opt(_, StringLit(_))))) =>
                    performanceCounter += 1
                    return FunctionCall(ExprList(List(opt1, Opt(trueF3, Constant(performanceCounter.toString)))))
                case _ =>
            }
        }
        suffix
    }

    override def insertPerformanceCounter(cmpStmt: CompoundStatement): CompoundStatement = {
        if (insertPerformanceCounter) {
            cmpStmt.innerStatements.head match {
                case Opt(ft, ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(opt1@Opt(_, StringLit(_)))))))) =>
                    val result = CompoundStatement(Opt(ft, ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(opt1, Opt(trueF3, Constant(performanceCounter.toString)))))))) :: cmpStmt.innerStatements.tail)
                    performanceCmpStmtContextMap.put(result, getContext(cmpStmt))
                    performanceCounter += 1
                    return result
                case _ =>
            }
            return cmpStmt
        } else {
            return cmpStmt
        }
    }

    private def getContext(cmpStmt: CompoundStatement): String = {
        cmpStmt.innerStatements.head match {
            case Opt(ft, ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(entries))))) =>
                entries match {
                    case Opt(_, StringLit(List(Opt(_, stringLiteral)))) :: xs =>
                        return stringLiteral.replaceFirst("\"(.*?)\"", "$1")
                    case _ =>
                        logger2.error("Could not find context for statement: " + CompoundStatement)
                        return ""
                }
            case _ =>
        }
        logger2.error("Could not find context for statement: " + CompoundStatement)
        return ""
    }

    override def insertPerfFunctCalls(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        if (context == trueF3 || cmpstmt.innerStatements.isEmpty) {
            // Don't insert anything
            return cmpstmt
        }
        var result: CompoundStatement = cmpstmt
        val numberOfStatements = getNumberOfStatements(cmpstmt)
        var functionName = functionBeforeName
        if (insertPerformanceCounter) {
            functionName = functionBeforeCounterName
        }
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ contextToReadableString(context) ++ "\"")))))))))
        val last = cmpstmt.innerStatements.last
        val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))

        def alterStatementHelper[T <: Product](t: T, continueExitsContext: Boolean = true): T = {
            val transformation = alltd(rule {
                case i@IfStatement(cond, One(CompoundStatement(List(Opt(ft, ContinueStatement())))), List(), None) =>
                    if (continueExitsContext) {
                        IfStatement(cond, One(CompoundStatement(List(Opt(trueF3, afterStmt), Opt(ft, ContinueStatement())))), List(), None)
                    } else {
                        i
                    }
                case Opt(ft, s: ForStatement) =>
                    Opt(ft, alterStatementHelper(s, false))
                case Opt(ft, s: DoStatement) =>
                    Opt(ft, alterStatementHelper(s, false))
                case Opt(ft, s: WhileStatement) =>
                    Opt(ft, alterStatementHelper(s, false))
                case CompoundStatement(innerStmts) =>
                    CompoundStatement(innerStmts.flatMap {
                        case Opt(ft, ReturnStatement(None)) =>
                            List(Opt(ft, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))), Opt(ft, ReturnStatement(None)))
                        case Opt(ft, ReturnStatement(Some(expr))) =>
                            List(Opt(ft, ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, expr), Opt(trueF3, Id(functionAfterName + "(" + numberOfStatements.toString + ")")))))))))
                        case k =>
                            List(alterStatementHelper(k, continueExitsContext))
                    })
            })
            transformation(t).getOrElse(t).asInstanceOf[T]
        }
        val r = manytd(rule {
            case i@IfStatement(cond, One(CompoundStatement(List(Opt(ft, ContinueStatement())))), List(), None) =>
                IfStatement(cond, One(CompoundStatement(List(Opt(trueF3, afterStmt), Opt(ft, ContinueStatement())))), List(), None)
            case CompoundStatement(innerStmts) =>
                CompoundStatement(innerStmts.flatMap {
                    case Opt(ft, ReturnStatement(None)) =>
                        List(Opt(ft, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))), Opt(ft, ReturnStatement(None)))
                    case Opt(ft, ReturnStatement(Some(expr))) =>
                        List(Opt(ft, ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, expr), Opt(trueF3, Id(functionAfterName + "(" + numberOfStatements.toString + ")")))))))))
                    case k =>
                        List(k)
                })
        })
        if (last.entry.isInstanceOf[ReturnStatement]) {
            result = CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ r(cmpstmt).getOrElse(cmpstmt).asInstanceOf[CompoundStatement].innerStatements)
        } else {
            //val newCompound = r(cmpstmt).getOrElse(cmpstmt).asInstanceOf[CompoundStatement]
            //val newCompound = alterStatementHelper(cmpstmt)
            val newCompound = cmpstmt
            if (last.entry.isInstanceOf[BreakStatement]) {
                result = CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ newCompound.innerStatements.take(newCompound.innerStatements.size - 1) ++ List(Opt(trueF3, afterStmt)) ++ List(last))
            } else {
                result = CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ newCompound.innerStatements ++ List(Opt(trueF3, afterStmt)))
            }
        }
        return result
        /*if (last.entry.isInstanceOf[ReturnStatement]) {
            val currentReturn = last.entry.asInstanceOf[ReturnStatement]
            if (currentReturn.expr.isDefined) {
                val fctCall = ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, last.entry.asInstanceOf[ReturnStatement].expr.get), Opt(trueF3, Id(functionAfterName + "(" + numberOfStatements.toString + ")")))))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, fctCall)))
            } else {
                //val fctCall = ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, Constant("0")), Opt(trueF3, Id(functionAfterName + "(" + numberOfStatements.toString + ")")))))))
                //return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, fctCall)))

                val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, afterStmt)) ++ List(last))
            }
        } else {
            val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))
            return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements ++ List(Opt(trueF3, afterStmt)))
        }*/
    }

    override def insertPerfMainFunctCalls(cmpstmt: CompoundStatement): CompoundStatement = {
        if (cmpstmt.innerStatements.isEmpty) {
            // Don't insert anything
            return cmpstmt
        }
        val startStmt = ExprStatement(PostfixExpr(Id(functionStartName), FunctionCall(ExprList(List()))))
        val last = cmpstmt.innerStatements.last
        if (last.entry.isInstanceOf[ReturnStatement]) {
            val currentReturn = last.entry.asInstanceOf[ReturnStatement]
            if (currentReturn.expr.isDefined) {
                val fctCall = ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, last.entry.asInstanceOf[ReturnStatement].expr.get), Opt(trueF3, Id(functionEndName + "()")))))))
                return CompoundStatement(List(Opt(trueF3, startStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, fctCall)))
            } else {
                val endStmt = ExprStatement(PostfixExpr(Id(functionEndName), FunctionCall(ExprList(List()))))
                return CompoundStatement(List(Opt(trueF3, startStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, endStmt)) ++ List(last))
            }
        } else {
            val endStmt = ExprStatement(PostfixExpr(Id(functionEndName), FunctionCall(ExprList(List()))))
            return CompoundStatement(List(Opt(trueF3, startStmt)) ++ cmpstmt.innerStatements ++ List(Opt(trueF3, endStmt)))
        }
    }

    override def combinePerformancePair(firstStmts: CompoundStatement, secondStmts: CompoundStatement): CompoundStatement = {
        val tmpTuple = removePerformanceAfterFunction(firstStmts.innerStatements)
        val result = CompoundStatement(tmpTuple._1 ++ updatePerformanceAfterFunction(removePerformanceBeforeFunction(secondStmts.innerStatements), tmpTuple._2))
        return result
    }

    override def removePerformanceBeforeFunction(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        val first = stmts.head
        first match {
            case Opt(trueF3, ExprStatement(PostfixExpr(Id(functionBeforeName), fc: FunctionCall))) =>
                return stmts.drop(1)
            case _ =>
                return stmts
        }
        return stmts
    }

    override def removePerformanceAfterFunction(stmts: List[Opt[Statement]]): (List[Opt[Statement]], Int) = {
        val last = stmts.last
        last match {
            case Opt(_, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(_, Constant(numberOfStatements)))))))) =>
                return (stmts.take(stmts.size - 1), numberOfStatements.toInt)
            case _ =>
                return (stmts, 0)
        }
        return (stmts, 0)
    }

    private def updatePerformanceAfterFunction(stmts: List[Opt[Statement]], numberOfStmtsToAdd: Int): List[Opt[Statement]] = {
        val last = stmts.last
        last match {
            case Opt(f1, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(f2, Constant(numberOfStatements)))))))) =>
                return stmts.updated(stmts.size - 1, Opt(f1, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(f2, Constant((numberOfStatements.toInt + numberOfStmtsToAdd).toString())))))))))
            case _ =>
                return stmts
        }
        return stmts
    }

    private def contextToReadableString(context: FeatureExpr): String = {
        val regexPattern = "(defined|definedEx)\\(([a-zA-Z_0-9]+)\\)".r
        return regexPattern replaceAllIn(context.toTextExpr, "$2")
    }

    /**
      * Checks if given Conditional[Expr] is a condition generated from the ifdeftoif transformation process (see
      * function toCExpr).
      */
    private def isIfdeftoifCondition2(cExpr: Conditional[Expr]): Boolean = {
        var result = false
        cExpr match {
            case One(expr@NAryExpr(PostfixExpr(Id(name), PointerPostfixSuffix(".", i: Id)), _)) =>
                if (name.equals(featureStructInitializedName2))
                    result = true
            case One(expr@PostfixExpr(Id(name), PointerPostfixSuffix(".", i: Id))) =>
                if (name.equals(featureStructInitializedName2))
                    result = true
            case One(expr@UnaryOpExpr("!", NAryExpr(PostfixExpr(Id(name), PointerPostfixSuffix(".", i: Id)), _))) =>
                if (name.equals(featureStructInitializedName2))
                    result = true
            case One(expr@UnaryOpExpr("!", PostfixExpr(Id(name), PointerPostfixSuffix(".", i: Id)))) =>
                if (name.equals(featureStructInitializedName2))
                    result = true
            case One(expr@NAryExpr(exprs, others)) =>
                result = isIfdeftoifCondition2(exprs)
            case One(id: Id) =>
                result = id.name.startsWith(featurePrefix2)
            case One(UnaryOpExpr("!", id: Id)) =>
                result = id.name.startsWith(featurePrefix2)
            case _ =>
        }
        return result
    }

    /**
      * Checks if given Conditional[Expr] is a condition generated from the ifdeftoif transformation process (see
      * function toCExpr).
      */
    private def isIfdeftoifCondition2(cExpr: Expr): Boolean = {
        cExpr match {
            case NAryExpr(expr, others) =>
                return isIfdeftoifCondition2(expr)
            case id: Id =>
                return id.name.startsWith(featurePrefix2)
            case UnaryOpExpr("!", id: Id) =>
                return id.name.startsWith(featurePrefix2)
            case _ =>
                return false
        }
    }

    private def getNumberOfStatements(stmt: Statement): Int = {
        stmt match {
            case CompoundStatement(innerStmts) =>
                return innerStmts.filter(x => x.entry.isInstanceOf[CompoundStatement]).map(y => getNumberOfStatements(y.entry)).sum + innerStmts.filter(x => !x.entry.isInstanceOf[CompoundStatement]).map(x => getNumberOfStatements(x.entry)).sum
            case IfStatement(cond, thens, elifs, elses) =>
                if (isIfdeftoifCondition2(cond)) {
                    return 0
                } else {
                    // TODO statement counting inside ifs?
                    return 1
                }
            case s: Statement =>
                return 1
        }
    }
}