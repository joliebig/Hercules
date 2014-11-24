package de.fosd.typechef.cifdeftoif

import java.util

import de.fosd.typechef.options.Options.OptionGroup
import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, Options}
import gnu.getopt.{Getopt, LongOpt}

class IfdefToIfOptions extends FrontendOptionsWithConfigFiles {
    private final val F_IFDEFTOIF: Char = Options.genOptionId
    private final val F_IFDEFTOIFSTATISTICS: Char = Options.genOptionId
    private final val F_IFDEFTOIFNOCHECK: Char = Options.genOptionId
    private final val F_ASSUME_NO_SWITCH_FALLTHROUGH: Char = Options.genOptionId
    private final val F_FEATURECONFIG: Char = Options.genOptionId
    private final val F_DECLUSE: Char = Options.genOptionId
    private final val F_MD: Char = Options.genOptionId // dependency output option of gcc

    var ifdeftoif: Boolean = false
    var ifdeftoifstatistics: Boolean = false
    var ifdeftoifnocheck: Boolean = false
    var assume_no_switch_fallthrough: Boolean = false
    var decluse: Boolean = false
    var featureConfig: Boolean = false

    private var featureConfigFile: String = ""
    private var md: String = ""

    def getFeatureConfigFilename: String = featureConfigFile

    def getMDoption: String = md

    protected override def getOptionGroups() = {
        val groups = new util.ArrayList[OptionGroup](super.getOptionGroups())

        groups.add(
            new Options.OptionGroup("#ifdef to if options", 1,
                new Options.Option("ifdeftoif", LongOpt.NO_ARGUMENT, F_IFDEFTOIF, "file",
                    "Make #ifdef to if transformation."),
                new Options.Option("ifdeftoifstatistics", LongOpt.NO_ARGUMENT, F_IFDEFTOIFSTATISTICS, "file",
                    "Save statistics for #ifdef to if transformation."),
                new Options.Option("ifdeftoifnocheck", LongOpt.NO_ARGUMENT, F_IFDEFTOIFNOCHECK, "file",
                    "Do not typecheck the result of #ifdef to if transformation."),
                new Options.Option("assumeNoSwitchFallThroughs", LongOpt.NO_ARGUMENT, F_ASSUME_NO_SWITCH_FALLTHROUGH, "file",
                    "Assume that each path through a case in a switch case statement ends with a break (no fall through to next case)."),
                new Options.Option("featureConfig", LongOpt.REQUIRED_ARGUMENT, F_FEATURECONFIG, null,
                    "Save file for load-time feature configuration at given filename."),
                new Options.Option("decluse", LongOpt.NO_ARGUMENT, F_DECLUSE, null,
                    "Test the declaration use map."),
                new Options.Option("MD", LongOpt.REQUIRED_ARGUMENT, F_MD, "file",
                    "Export dependency list.")
            ))

        groups
    }

    protected override def interpretOption(c: Int, g: Getopt): Boolean = {
        if (c == F_IFDEFTOIFSTATISTICS) {
            parse = true
            typecheck = true
            ifdeftoif = true
            ifdeftoifstatistics = true
        } else if (c == F_IFDEFTOIF) {
            parse = true
            typecheck = true
            ifdeftoif = true
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
        } else if (c == F_MD) {
            md = g.getOptarg
        } else if (c == F_ASSUME_NO_SWITCH_FALLTHROUGH) {
            assume_no_switch_fallthrough = true
        } else {
            return super.interpretOption(c, g)
        }

        true
    }

}
