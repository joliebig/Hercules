package de.fosd.typechef.cifdeftoif

import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, Options}
import java.util
import de.fosd.typechef.options.Options.OptionGroup
import gnu.getopt.{Getopt, LongOpt}

class IfdefToIfOptions extends FrontendOptionsWithConfigFiles {
    private final val F_IFDEFTOIF: Char = Options.genOptionId
    private final val F_IFDEFTOIFSTATISTICS: Char = Options.genOptionId
    private final val F_IFDEFTOIFNOCHECK: Char = Options.genOptionId
    private final val F_FEATURECONFIG: Char = Options.genOptionId
    private final val F_DECLUSE: Char = Options.genOptionId

    var ifdeftoif: Boolean = false
    var ifdeftoifstatistics: Boolean = false
    var ifdeftoifnocheck: Boolean = false
    var decluse: Boolean = false
    private var featureConfigFile: String = ""
    private var includeStructFile: String = ""

    protected override def getOptionGroups() = {
        val groups = new util.ArrayList[OptionGroup](super.getOptionGroups())

        groups.add(
            new Options.OptionGroup("#ifdef to if options", 1,
                new Options.Option("ifdeftoif", LongOpt.NO_ARGUMENT, F_IFDEFTOIF, "file",
                    "Make #ifdef to if transformation."),
                new Options.Option("ifdeftoifstatistics", LongOpt.NO_ARGUMENT, F_IFDEFTOIFSTATISTICS, "file",
                    "Make #ifdef to if transformation."),
                new Options.Option("ifdeftoifnocheck", LongOpt.NO_ARGUMENT, F_IFDEFTOIFNOCHECK, "file",
                    "Do not typecheck the result of #ifdef to if transformation."),
                new Options.Option("featureConfig", LongOpt.REQUIRED_ARGUMENT, F_FEATURECONFIG, null,
                    "Make #ifdef to if transformation."),
                new Options.Option("decluse", LongOpt.NO_ARGUMENT, F_DECLUSE, null,
                    "Test the declaration use map.")
            ))

        groups
    }

    protected override def interpretOption(c: Int, g: Getopt): Boolean = {
        if (c == F_IFDEFTOIFSTATISTICS) {
            parse = true
            typecheck = true
            ifdeftoif = true
            ifdeftoifstatistics = true
            if (g.getOptarg == null)
                includeStructFile = ""
            else {
                includeStructFile = g.getOptarg
            }
        } else if (c == F_IFDEFTOIF) {
            parse = true
            typecheck = true
            ifdeftoif = true
            if (g.getOptarg == null)
                includeStructFile = ""
            else {
                includeStructFile = g.getOptarg
            }
        } else if (c == F_FEATURECONFIG) {
            checkFileExists(g.getOptarg)
            featureConfigFile = g.getOptarg
            featureConfig = true
        } else if (c == F_IFDEFTOIFNOCHECK) {
            parse = true
            typecheck = true
            ifdeftoif = true
            ifdeftoifnocheck = true
        } else if (c == F_DECLUSE) {
            parse = true
            typecheck = true
            decluse = true
        } else {
            return super.interpretOption(c, g)
        }

        true
    }

    def getFeatureConfigFilename: String = featureConfigFile
    def getincludeStructFilename: String = includeStructFile

}
