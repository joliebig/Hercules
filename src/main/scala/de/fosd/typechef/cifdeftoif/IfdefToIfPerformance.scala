package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._

trait IfdefToIfPerformanceInterface {

    def insertIntoIf(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        stmts
    }

    def insertIntoCmpStmt(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        cmpstmt
    }

    def insertIntoMain(cmpstmt: CompoundStatement): CompoundStatement = {
        cmpstmt
    }

}

trait IfdefToIfPerformance extends IfdefToIfPerformanceInterface with IOUtilities {
    val trueF3 = FeatureExprFactory.True
    val functionBeforeName = "id2iperf_time_before"
    val functionAfterName = "id2iperf_time_after"
    val functionStartName = "id2iperf_time_start"
    val functionEndName = "id2iperf_time_end"

    override def insertIntoIf(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        if (context == trueF3) {
            // Don't insert anything
            return stmts
        }
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ context.toTextExpr ++ "\"")))))))))
        val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))
        return List(Opt(trueF3, beforeStmt)) ++ stmts ++ List(Opt(trueF3, afterStmt))
    }

    override def insertIntoCmpStmt(cmpstmt: CompoundStatement, context: FeatureExpr): CompoundStatement = {
        if (context == trueF3) {
            // Don't insert anything
            return cmpstmt
        }
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ context.toTextExpr ++ "\"")))))))))
        val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))
        return CompoundStatement(List(Opt(trueF3, beforeStmt)) ++ cmpstmt.innerStatements ++ List(Opt(trueF3, afterStmt)))
    }

    override def insertIntoMain(cmpstmt: CompoundStatement): CompoundStatement = {
        val startStmt = ExprStatement(PostfixExpr(Id(functionStartName), FunctionCall(ExprList(List()))))
        val endStmt = ExprStatement(PostfixExpr(Id(functionEndName), FunctionCall(ExprList(List()))))
        return CompoundStatement(List(Opt(trueF3, startStmt)) ++ cmpstmt.innerStatements ++ List(Opt(trueF3, endStmt)))
    }

}