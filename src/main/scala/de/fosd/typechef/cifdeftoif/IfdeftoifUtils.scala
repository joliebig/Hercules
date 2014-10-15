package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, SingleFeatureExpr}
import org.kiama.rewriting.Rewriter._


/**
 * Created by rhein on 6/23/14.
 */
object IfdeftoifUtils {
    /**
     * Returns a set of all configuration options in a.
     */
    def getSingleFeatures(a: Any): Set[SingleFeatureExpr] = {
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
    def getSingleFeaturesFromList(lst: List[FeatureExpr]): Set[SingleFeatureExpr] = {
        var featureSet: Set[FeatureExpr] = lst.toSet
        featureSet.flatMap(x => x.collectDistinctFeatureObjects)
    }
}
