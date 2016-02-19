package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
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

    def printPerformanceCounter() = {
        // Nothing
    }

    def insertPerformanceCounter(suffix: PostfixSuffix): PostfixSuffix = {
        suffix
    }

    def insertPerformanceCounter(cmpStmt: CompoundStatement): CompoundStatement = {
        cmpStmt
    }

    def updatePerformancePrependString(prependString: String, createIncludeDirective: (String) => String, path: String): String = {
        ""
    }
}

trait IfdefToIfPerformance extends IfdefToIfPerformanceInterface with IOUtilities {
    val trueF3 = FeatureExprFactory.True
    val functionBeforeName = "id2iperf_time_before"
    val functionAfterName = "id2iperf_time_after"
    val functionStartName = "id2iperf_time_start"
    val functionEndName = "id2iperf_time_end"
    val returnMacroName = "id2iperf_return"

    private val featureStructInitializedName2 = "id2i"
    private val performanceIncludeFileName = "perf_measuring.c"
    private val performanceIncludeCounterFileName = "perf_measuring_counter.c"
    private val performanceCmpStmtContextMap: java.util.LinkedHashMap[CompoundStatement, FeatureExpr] = new java.util.LinkedHashMap()
    private val returnMacro = "#define id2iperf_return(expr, stmts) __typeof__(expr) ____RetTmp = expr; stmts; return ____RetTmp;\n"
    private var featurePrefix2 = "f_"
    private var performanceCounter = 0
    private var insertPerformanceCounter = true

    override def updatePerformancePrependString(prependString: String, createIncludeDirective: (String) => String, path: String): String = {
        if (insertPerformanceCounter) {
            returnMacro ++ createIncludeDirective(path ++ performanceIncludeCounterFileName)
        } else {
            returnMacro ++ createIncludeDirective(path ++ performanceIncludeFileName)
        }
    }

    override def printPerformanceCounter() = {
        if (insertPerformanceCounter) {
            println("Number of performance measuring nodes: " + performanceCounter)
            var currentIndex = 0
            performanceCmpStmtContextMap.foreach(x => {
                println("Node " + currentIndex.toString + ":\t" + contextToReadableString(x._2) + " -> " + x._1)
                currentIndex += 1
            })
        }
    }

    private def contextToReadableString(context: FeatureExpr): String = {
        val regexPattern = "(defined|definedEx)\\(([a-zA-Z_0-9]+)\\)".r
        return regexPattern replaceAllIn(context.toTextExpr, "$2")
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
                    val context = performanceCmpStmtContextMap.get(cmpStmt)
                    val result = CompoundStatement(Opt(ft, ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(opt1, Opt(trueF3, Constant(performanceCounter.toString)))))))) :: cmpStmt.innerStatements.tail)
                    performanceCmpStmtContextMap.put(result, context)
                    performanceCmpStmtContextMap.remove(cmpStmt)
                    performanceCounter += 1
                    return result
                case _ =>
            }
            return cmpStmt
        } else {
            return cmpStmt
        }
    }

    override def insertPerfFunctCalls(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        val result = insertPerfFunctCallsInner(cmpstmt, context)
        performanceCmpStmtContextMap.put(result, context)
        return result
    }

    def insertPerfFunctCallsInner(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        if (context == trueF3 || cmpstmt.innerStatements.isEmpty) {
            // Don't insert anything
            return cmpstmt
        }
        val numberOfStatements = getNumberOfStatements(cmpstmt)
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ contextToReadableString(context) ++ "\"")))))))))
        val last = cmpstmt.innerStatements.last

        val r = manytd(rule {
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
        if (!last.entry.isInstanceOf[ReturnStatement]) {
            val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))
            return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ r(cmpstmt).getOrElse(cmpstmt).asInstanceOf[CompoundStatement].innerStatements ++ List(Opt(trueF3, afterStmt)))
        } else {
            return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ r(cmpstmt).getOrElse(cmpstmt).asInstanceOf[CompoundStatement].innerStatements)
        }
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
        val context = performanceCmpStmtContextMap.get(firstStmts)
        performanceCmpStmtContextMap.put(result, context)
        performanceCmpStmtContextMap.remove(firstStmts)
        performanceCmpStmtContextMap.remove(secondStmts)
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