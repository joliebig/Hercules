package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.Opt
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

    def removePerformanceFunctions(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        stmts
    }

    def removePerformanceBeforeFunction(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        stmts
    }

    def removePerformanceAfterFunction(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        stmts
    }
}

trait IfdefToIfPerformance extends IfdefToIfPerformanceInterface with IOUtilities {
    val trueF3 = FeatureExprFactory.True
    val functionBeforeName = "id2iperf_time_before"
    val functionAfterName = "id2iperf_time_after"
    val functionStartName = "id2iperf_time_start"
    val functionEndName = "id2iperf_time_end"
    val returnMacroName = "id2iperf_return"

    override def insertPerfFunctCalls(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        return insertPerfFunctCalls(CompoundStatement(stmts), context).innerStatements
    }

    override def insertPerfFunctCalls(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        if (context == trueF3 || cmpstmt.innerStatements.isEmpty) {
            // Don't insert anything
            return cmpstmt
        }
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ context.toTextExpr ++ "\"")))))))))
        val last = cmpstmt.innerStatements.last
        if (last.entry.isInstanceOf[ReturnStatement]) {
            val currentReturn = last.entry.asInstanceOf[ReturnStatement]
            if (currentReturn.expr.isDefined) {
                val fctCall = ExprStatement(PostfixExpr(Id(returnMacroName), FunctionCall(ExprList(List(Opt(trueF3, last.entry.asInstanceOf[ReturnStatement].expr.get), Opt(trueF3, Id(functionAfterName + "()")))))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, fctCall)))
            } else {
                val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))
                return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements.take(cmpstmt.innerStatements.size - 1) ++ List(Opt(trueF3, afterStmt)) ++ List(last))
            }
        } else {
            val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))
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

    override def removePerformanceFunctions(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        removePerformanceAfterFunction(removePerformanceBeforeFunction(stmts))
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

    override def removePerformanceAfterFunction(stmts: List[Opt[Statement]]): List[Opt[Statement]] = {
        val last = stmts.last
        last match {
            case Opt(trueF3, ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))) =>
                return stmts.take(stmts.size - 1)
            case _ =>
                return stmts
        }
        return stmts
    }
}