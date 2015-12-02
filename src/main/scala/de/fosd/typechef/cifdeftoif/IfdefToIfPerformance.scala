package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c.{IfStatement, _}

trait IfdefToIfPerformanceInterface {

    def insertIntoIf(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        stmts
    }

    def insertPerfIntoIf(ifstmt: IfStatement, context: FeatureExpr): IfStatement = {
        ifstmt
    }

}

trait IfdefToIfPerformance extends IfdefToIfPerformanceInterface with IOUtilities {
    val trueF3 = FeatureExprFactory.True
    val functionBeforeName = "id2iperf_time_before"
    val functionAfterName = "id2iperf_time_after"
    val functionStartName = "id2iperf_time_start"
    val functionEndName = "id2iperf_time_end"

    override def insertIntoIf(stmts: List[Opt[Statement]], context: FeatureExpr): List[Opt[Statement]] = {
        val beforeStmt = ExprStatement(PostfixExpr(Id(functionBeforeName), FunctionCall(ExprList(List(Opt(trueF3, StringLit(List(Opt(trueF3, "\"" ++ context.toTextExpr ++ "\"")))))))))
        val afterStmt = ExprStatement(PostfixExpr(Id(functionAfterName), FunctionCall(ExprList(List()))))
        return List(Opt(trueF3, beforeStmt)) ++ stmts ++ List(Opt(trueF3, afterStmt))
    }

    override def insertPerfIntoIf(ifstmt: IfStatement, context: FeatureExpr): IfStatement = {
        ifstmt match {
            case IfStatement(One(expr), thenBranch, elifs, elseBranch) =>

        }
        ifstmt
    }


}