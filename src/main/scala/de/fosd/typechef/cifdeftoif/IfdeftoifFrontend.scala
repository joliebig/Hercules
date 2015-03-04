package de.fosd.typechef.cifdeftoif


import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.lexer.LexerFrontend
import de.fosd.typechef.options._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.{CPP_replacement_methods, ErrorXML}

import scala.io.Source


object IfdeftoifFrontend extends App with Logging with EnforceTreeHelper {

    private var opt: IfdefToIfOptions = new IfdefToIfOptions()
    private var i: IfdefToIf = new IfdefToIf

    override def main(args: Array[String]): Unit = {
        opt = new IfdefToIfOptions()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException =>
                    if (!opt.isPrintVersion && !opt.featureConfig) throw o
            }
        }

        catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }
        // Needs to be initialized after options are processed.
        // Otherwise FeatureExprFactory will still be the default (sat) and fields in IfdefToIf are initialized with sat expressions even if bdds are used later.
        i = new IfdefToIf

        if (!opt.getFiles().isEmpty()) {
            processFile(opt)
            if (!opt.featureConfig) {
                val configPath = opt.getFeatureConfigFilename
                i.writeExternIfdeftoIfStruct(configPath)
                println("Created extern struct file, all features are initialized with default value '" + i.defaultValue + "' at: " + configPath)
            }
        }

        if (opt.featureConfig) {
            val configPath = opt.getFeatureConfigFilename
            i.writeExternIfdeftoIfStruct(configPath)
            println("Created extern struct file from configuration at: " + configPath)
        }
    }

    private def processFile(opt: IfdefToIfOptions) {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val stopWatch = new StopWatch()
        stopWatch.start("loadFM")

        val fullFM = opt.getFullFeatureModel().and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        val parseFM = opt.getSmallFeatureModel().and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(parseFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        opt.setFullFeatureModel(fullFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        var ast: TranslationUnit = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedTUnitFilename).exists()) {
            println("loading AST.")
            try {
                ast = loadSerializedAST(opt.getSerializedTUnitFilename)
                ast = prepareAST[TranslationUnit](ast)
            } catch {
                case e: Throwable => println(e.toString); e.printStackTrace(); ast = null
            }
            if (ast == null)
                println("... failed reading AST\n")
        }
        var ast_replaced: TranslationUnit = null
        val replacementDefintionsFile = new File("./ifdeftoif_replacements_parts/PreparedReplacementParts.txt")

        stopWatch.start("lexing")
        //no parsing if read serialized ast
        val in = if (ast == null) lex(opt) else null

        if (opt.ifdeftoifstatistics) {
            i = new IfdefToIf with IfdefToIfStatistics
        }

        if (opt.parse) {
            stopWatch.start("parsing")

            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(parseFM))
                ast = parserMain.parserMain(in, opt, fullFM)
                ast = prepareAST[TranslationUnit](ast)
                if (opt.ifdeftoif && !opt.reuseAST) {
                    // preprocessing: replace situations with too much local variability (e.g. different string in each variant) with prepared replacements
                    if (replacementDefintionsFile.exists()) {
                        val (newAst, usedVariables) = PreparedIfdeftoifParts.replaceInAST(ast, replacementDefintionsFile)
                        ast_replaced = i.prepareASTforIfdef(newAst)
                        i.loadAndUpdateFeatures(usedVariables)
                    } else {
                        println("Did not find file with replacement definitions: " + replacementDefintionsFile.getPath)
                    }
                    ast = i.prepareASTforIfdef(ast)
                }

                if (ast != null && opt.serializeAST) {
                    stopWatch.start("serialize")
                    serializeAST(ast, opt.getSerializedTUnitFilename)
                }

            }

            if (ast != null) {


                // I need this code for building serialized AST-parts that I can use in PreparedIfdeftoifParts to replace problematic AST elements.
                // step 1. copy problematic source code in file, load it with TC, trim and copy AST code (line 1) in val s
                // (trimming (search object) means to remove the TranslationUnit and the next List Object )
                // step 2. fix problematic source code in file, load it with TC, trim and copy AST code (line 1) in val r
                // (trimming (replace object) means to remove the TranslationUnit and optionally the List if it has only one item )
                // step 3. run TC again, copy 4 following lines to File "ifdeftoif_replacements_parts/PreparedReplacementParts.txt"
                /*
                println("// AST:  " + ast)
                println()
                val s = Opt(FeatureExprFactory.True,Declaration(List(Opt(FeatureExprFactory.True,EnumSpecifier(None,Some(List(Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_COLLAPSE"),Some(Constant("0x00010000")))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_TRIM"),Some(Constant("0x00020000")))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_GREEDY"),Some(Constant("0x00040000")))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_MIN_DIE"),Some(Constant("0x00100000")))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_KEEP_COPY"),Some(NAryExpr(Constant("0x00200000"),List(Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_CROND_D"),NArySubExpr("*",Constant("1"))), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_CROND_D").not(),NArySubExpr("*",Constant("0")))))))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_EOL_COMMENTS"),Some(Constant("0x00400000")))), Opt(FeatureExprFactory.True,Enumerator(Id("PARSE_NORMAL"),Some(NAryExpr(Id("PARSE_COLLAPSE"),List(Opt(FeatureExprFactory.True,NArySubExpr("|",Id("PARSE_TRIM"))), Opt(FeatureExprFactory.True,NArySubExpr("|",Id("PARSE_GREEDY"))), Opt(FeatureExprFactory.True,NArySubExpr("|",Id("PARSE_EOL_COMMENTS"))))))))))))),List()))
                val r = List(Opt(FeatureExprFactory.True,Declaration(List(Opt(FeatureExprFactory.True,EnumSpecifier(None,Some(List(Opt(FeatureExprFactory.True,Enumerator(Id("LOGMODE_NONE"),Some(Constant("0")))), Opt(FeatureExprFactory.True,Enumerator(Id("LOGMODE_STDIO"),Some(NAryExpr(Constant("1"),List(Opt(FeatureExprFactory.True,NArySubExpr("<<",Constant("0"))))))))))))),List())), Opt(FeatureExprFactory.True,Declaration(List(Opt(FeatureExprFactory.True,IntSpecifier())),List(Opt(FeatureExprFactory.True,InitDeclaratorI(AtomicNamedDeclarator(List(),Id("LOGMODE_SYSLOG"),List()),List(),None))))), Opt(FeatureExprFactory.True,Declaration(List(Opt(FeatureExprFactory.True,IntSpecifier())),List(Opt(FeatureExprFactory.True,InitDeclaratorI(AtomicNamedDeclarator(List(),Id("LOGMODE_BOTH"),List()),List(),None))))), Opt(FeatureExprFactory.True,FunctionDef(List(Opt(FeatureExprFactory.True,VoidSpecifier())),AtomicNamedDeclarator(List(),Id("prepared_init_logmodeEnum"),List(Opt(FeatureExprFactory.True,DeclIdentifierList(List())))),List(),CompoundStatement(List(Opt(FeatureExprFactory.True,ExprStatement(AssignExpr(Id("LOGMODE_SYSLOG"),"=",ConditionalExpr(PostfixExpr(Id("id2i"),PointerPostfixSuffix(".",Id("config_feature_syslog"))),Some(NAryExpr(NAryExpr(Constant("1"),List(Opt(FeatureExprFactory.True,NArySubExpr("<<",Constant("1"))))),List(Opt(FeatureExprFactory.True,NArySubExpr("*",Constant("1")))))),NAryExpr(NAryExpr(Constant("1"),List(Opt(FeatureExprFactory.True,NArySubExpr("<<",Constant("1"))))),List(Opt(FeatureExprFactory.True,NArySubExpr("*",Constant("0"))))))))), Opt(FeatureExprFactory.True,ExprStatement(AssignExpr(Id("LOGMODE_BOTH"),"=",NAryExpr(Id("LOGMODE_SYSLOG"),List(Opt(FeatureExprFactory.True,NArySubExpr("+",Id("LOGMODE_STDIO")))))))))))))
                println("// search for: " + s)
                println("searchObj: " + PreparedIfdeftoifParts.serializeObject(s))
                println("// replace with: " + r)
                println("replaceObj: " + PreparedIfdeftoifParts.serializeObject(r))
                */

                // some dataflow analyses require typing information
                var ts = new CTypeSystemFrontend(ast, fullFM, opt) with CTypeCache with CDeclUse

                if (opt.typecheck || opt.writeInterface) {
                    //PrCDeclUseoductGeneration.typecheckProducts(fm,fullFM,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fullFM)

                    stopWatch.start("typechecking")
                    println("type checking")
                    val typeCheckStatus = ts.checkASTSilent
                    ts.errors.map(errorXML.renderTypeError)
                    if (opt.decluse) {
                        if (typeCheckStatus) {
                            val fw = new FileWriter(i.basename(opt.getOutputStem()) + ".decluse")
                            fw.write(ts.checkDefuse(ast, ts.getDeclUseMap, ts.getUseDeclMap, fullFM)._1)
                            fw.close()
                            println(ast)
                            println(ts.checkDefuse(ast, ts.getDeclUseMap, ts.getUseDeclMap, fullFM)._1)
                            println(ts.getDeclUseMap)
                        } else {
                            println("generating the declaration-usage map unsuccessful because of type errors in source file")
                        }
                    }
                    if (opt.ifdeftoif) {
                        if (typeCheckStatus) {
                            //ProductGeneration.typecheckProducts(fm,fullFM,ast,opt,
                            //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                            //ProductGeneration.estimateNumberOfVariants(ast, fullFM)
                            //val includeStructFilename = opt.getincludeStructFilename()
                            if (replacementDefintionsFile.exists() && !opt.reuseAST) {
                                ts = new CTypeSystemFrontend(ast_replaced, fullFM, opt) with CTypeCache with CDeclUse
                                ts.checkASTSilent
                                ast = ast_replaced
                            }
                            i.setSimpleSwitchTransformation(opt.simple_switch_transformation)
                            stopWatch.start("ifdeftoif")
                            println("ifdeftoif started")
                            i.setParseFM(parseFM)
                            val defUseMap = ts.getDeclUseMap
                            val useDefMap = ts.getUseDeclMap
                            val fileName = i.basename(opt.getOutputStem())
                            val checkIfdefToIfResult = !opt.ifdeftoifnocheck
                            val tuple = i.ifdeftoif(ast, defUseMap, useDefMap, fullFM, opt.getOutputStem(), stopWatch.get("lexing") + stopWatch.get("parsing"), opt.ifdeftoifstatistics, "", typecheckResult = checkIfdefToIfResult, true)
                            tuple._1 match {
                                case None =>
                                    println("!! Transformation of " ++ fileName ++ " unsuccessful because of type errors in transformation result !!")
                                /*
                                tuple._3.map(errorXML.renderTypeError(_))             y
                                 */
                                case Some(x) =>
                                    if (!opt.getOutputStem().isEmpty()) {
                                        println("++Transformed: " ++ fileName ++ "++\t\t --in " + tuple._2 ++ " ms--")
                                    }
                            }
                            if (new File("../ifdeftoif/partialConfiguration.config").exists()) {
                                val defaultConfigExpr: Expr = PostfixExpr(Id("__VERIFIER_NONDET_INT"), FunctionCall(ExprList(List())))
                                // next line is ast for  "extern int __VERIFIER_NONDET_INT();"
                                //val prefixEx = Declaration(List(Opt(FeatureExprFactory.True,ExternSpecifier()), Opt(FeatureExprFactory.True,IntSpecifier())),List(Opt(FeatureExprFactory.True,InitDeclaratorI(AtomicNamedDeclarator(List(),Id("__VERIFIER_NONDET_INT"),List(Opt(FeatureExprFactory.True,DeclIdentifierList(List())))),List(),None))))
                                // next line is ast for  "int __VERIFIER_NONDET_INT() {return 1;}"
                                val prefixEx = FunctionDef(List(Opt(FeatureExprFactory.True, IntSpecifier())), AtomicNamedDeclarator(List(), Id("__VERIFIER_NONDET_INT"), List(Opt(FeatureExprFactory.True, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(FeatureExprFactory.True, ReturnStatement(Some(Constant("1")))))))
                                val prefixStr = PrettyPrinter.print(prefixEx)
                                i.writeExternIfdeftoIfStruct("../ifdeftoif/partialConfiguration.config", defaultConfigExpr, prefixStr)
                            } else {
                                i.writeExternIfdeftoIfStruct("../ifdeftoif/partialConfiguration.config")
                            }
                            if (opt.getMDoption != null && !opt.getMDoption.isEmpty) {
                                println("MD option: " + opt.getMDoption)
                                val dfilepath: String = CPP_replacement_methods.writeDependencyFile(ast, opt.getOutputStem, fileName, opt.getMDoption)
                                println("written dependency file (.d) to " + dfilepath)
                            } else {
                                val gccOptFile = new File("../ifdeftoif/lastGCCoptions.txt") // extract MD option value from GCC options
                                if (gccOptFile.exists()) {
                                    var mdOption: String = ""
                                    val iter = Source.fromFile(gccOptFile).iter
                                    while (mdOption.isEmpty && !iter.isEmpty) {
                                        iter.dropWhile(!_.equals('-'))
                                        if (iter.hasNext && iter.next() == ('M'))
                                            if (iter.hasNext && iter.next() == ('D')) {
                                                for (x: Char <- iter.takeWhile(!_.equals('-')))
                                                    mdOption += x
                                            }
                                    }
                                    mdOption = mdOption.trim
                                    if (!mdOption.isEmpty) {
                                        if (mdOption.contains(" "))
                                            mdOption = mdOption.substring(0, mdOption.indexOf(" "))
                                        val caseStudyRoot = "/local/ifdeftoif/TypeChef-BusyboxAnalysis_CPP_replacement/busybox-1.18.5/"
                                        val dfilepath: String = CPP_replacement_methods.writeDependencyFile(ast, opt.getOutputStem, fileName, caseStudyRoot + mdOption)
                                        println("written dependency file (.d) to " + dfilepath)
                                    }
                                }
                            }
                        } else {
                            println("#ifdef to if transformation unsuccessful because of type errors in source file")
                        }
                    }
                    ts.errors.map(errorXML.renderTypeError(_))
                }
            }
        }
        stopWatch.start("done")
        errorXML.write()
        if (opt.recordTiming)
            println(stopWatch)
    }

    private def lex(opt: IfdefToIfOptions): TokenReader[CToken, CTypeContext] =
        CLexerAdapter.prepareTokens(new LexerFrontend().run(opt, opt.parse))

    private def serializeAST(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    private def writeInterface(ast: AST, fm: FeatureModel, opt: IfdefToIfOptions) {
        val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm, opt) with CTypeCache with CDeclUse
        ts.checkAST()

        val interface = {
            if (opt.getUseDefaultPC) ts.getInferredInterface().and(opt.getFilePresenceCondition)
            else ts.getInferredInterface()
        }
        ts.writeInterface(interface, new File(opt.getInterfaceFilename))
        if (opt.writeDebugInterface)
            ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
    }

    private def loadSerializedAST(filename: String): TranslationUnit = try {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = {
                /*println(desc);*/ super.resolveClass(desc)
            }
        }
        val ast = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()
        ast
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); null
    }

    private class StopWatch {
        var lastStart: Long = 0
        var currentPeriod: String = "none"
        var currentPeriodId: Int = 0
        var times: Map[(Int, String), Long] = Map()

        def start(period: String) {
            val now = System.currentTimeMillis()
            val lastTime = now - lastStart
            times = times + ((genId(), currentPeriod) -> lastTime)
            lastStart = now
            currentPeriod = period
        }

        private def genId(): Int = {
            currentPeriodId += 1;
            currentPeriodId
        }

        def get(period: String): Long = times.filter(v => v._1._2 == period).headOption.map(_._2).getOrElse(0)

        override def toString = {
            var res = "timing "
            val switems = times.toList.filterNot(x => x._1._2 == "none" || x._1._2 == "done").sortBy(_._1._1)

            if (switems.size > 0) {
                res = res + "("
                res = res + switems.map(_._1._2).reduce(_ + ", " + _)
                res = res + ")\n"
                res = res + switems.map(_._2.toString).reduce(_ + ";" + _)
            }
            res
        }

        private def measure(checkpoint: String) {
            times = times + ((genId(), checkpoint) -> System.currentTimeMillis())
        }
    }
}
