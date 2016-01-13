package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._

trait IfdefToIfPerformanceInterface {

    def insertPerfFunctCalls(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        stmts
    }

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

    def combinePerformancePair(firstStmts: List[Opt[Statement]], secondStmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        firstStmts ++ secondStmts
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
    private var featurePrefix2 = "f_"

    override def insertPerfFunctCalls(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        return insertPerfFunctCalls(CompoundStatement(stmts), context).innerStatements
    }

    override def insertPerfFunctCalls(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        if (context == trueF3 || cmpstmt.innerStatements.isEmpty) {
            // Don't insert anything
            return cmpstmt
        }
        val numberOfStatements = getNumberOfStatements(cmpstmt)
        val regexPattern = "(defined|definedEx)\\(([a-zA-Z_0-9]+)\\)".r
        val featureString = regexPattern replaceAllIn(context.toTextExpr, "$2")
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ featureString ++ "\"")))))))))
        val last = cmpstmt.innerStatements.last
        if (last.entry.isInstanceOf[ReturnStatement]) {
            val currentReturn = last.entry.asInstanceOf[ReturnStatement]
            if (currentReturn.expr.isDefined) {
                val fctCall = ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, last.entry.asInstanceOf[ReturnStatement].expr.get), Opt(trueF3, Id(functionAfterName + "(" + numberOfStatements.toString + ")")))))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, fctCall)))
            } else {
                val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, afterStmt)) ++ List(last))
            }
        } else {
            val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List(Opt(trueF3, Constant(numberOfStatements.toString)))))))
            return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements ++ List(Opt(trueF3, afterStmt)))
        }
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

    override def combinePerformancePair(firstStmts: List[Opt[Statement]], secondStmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        val tmpTuple = removePerformanceAfterFunction(firstStmts)
        tmpTuple._1 ++ updatePerformanceAfterFunction(removePerformanceBeforeFunction(secondStmts), tmpTuple._2)
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
            case One(NAryExpr(PostfixExpr(Id(featureStructInitializedName2), PointerPostfixSuffix(".", i: Id)), _)) =>
                result = true
            case One(PostfixExpr(Id(featureStructInitializedName2), PointerPostfixSuffix(".", i: Id))) =>
                result = true
            case One(UnaryOpExpr("!", NAryExpr(PostfixExpr(Id(featureStructInitializedName2), PointerPostfixSuffix(".", i: Id)), _))) =>
                result = true
            case One(UnaryOpExpr("!", PostfixExpr(Id(featureStructInitializedName2), PointerPostfixSuffix(".", i: Id)))) =>
                result = true
            case One(NAryExpr(expr, others)) =>
                result = isIfdeftoifCondition2(expr)
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