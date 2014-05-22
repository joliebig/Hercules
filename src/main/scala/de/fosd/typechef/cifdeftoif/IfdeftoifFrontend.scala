package de.fosd.typechef.cifdeftoif


import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options._
import de.fosd.typechef.{ErrorXML, lexer}
import java.io._
import de.fosd.typechef.parser.TokenReader
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.crewrite._


object IfdeftoifFrontend extends App with Logging with EnforceTreeHelper {

    private var opt: FrontendOptions = new FrontendOptions()

    private class StopWatch {
        var lastStart: Long = 0
        var currentPeriod: String = "none"
        var currentPeriodId: Int = 0
        var times: Map[(Int, String), Long] = Map()

        private def genId(): Int = { currentPeriodId += 1; currentPeriodId }

        private def measure(checkpoint: String) {
            times = times + ((genId(), checkpoint) -> System.currentTimeMillis())
        }

        def start(period: String) {
            val now = System.currentTimeMillis()
            val lastTime = now - lastStart
            times = times + ((genId(), currentPeriod) -> lastTime)
            lastStart = now
            currentPeriod = period
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
    }

    override def main(args: Array[String]): Unit = {
        opt = new FrontendOptionsWithConfigFiles()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isPrintVersion || !opt.featureConfig) throw o
            }

            if (opt.featureConfig) {
                val i = new IfdefToIf
                val configPath = opt.getFeatureConfigFilename()
                i.writeExternIfdeftoIfStruct(configPath)
                println("Created extern struct file from configuration at: " + configPath)
                return
            }
        }

        catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }

        processFile(opt)
    }

    private def processFile(opt: FrontendOptions) {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val stopWatch = new StopWatch()
        stopWatch.start("loadFM")

        // @fgarbe: did merge with christian's master - we are using the full fm instead of the small fm.
        val fm = opt.getFullFeatureModel().and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions
        if (!opt.getFilePresenceCondition.isSatisfiable(fm)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        var ast: TranslationUnit = null

        stopWatch.start("lexing")
        //no parsing if read serialized ast
        val in = if (ast == null) lex(opt) else null


        if (opt.parse) {
            stopWatch.start("parsing")

            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(fm))
                ast = parserMain.parserMain(in, opt, fm)
                ast = prepareAST[TranslationUnit](ast)

                if (ast != null && opt.serializeAST) {
                    stopWatch.start("serialize")
                    serializeAST(ast, opt.getSerializedTUnitFilename)
                }

            }

            if (ast != null) {
                val fm_ts = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)

                // some dataflow analyses require typing information
                val ts = new CTypeSystemFrontend(ast, fm_ts, opt) with CTypeCache with CDeclUse

                if (opt.typecheck || opt.writeInterface) {
                    //PrCDeclUseoductGeneration.typecheckProducts(fm,fm_ts,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fm_ts)

                    stopWatch.start("typechecking")
                    println("type checking")
                    val typeCheckStatus = ts.checkAST()
                    ts.errors.map(errorXML.renderTypeError)
                    if (opt.decluse) {
                        if (typeCheckStatus) {
                            val i = new IfdefToIf
                            val fw = new FileWriter(i.outputStemToFileName(opt.getOutputStem()) + ".decluse")
                            fw.write(ts.checkDefuse(ast, ts.getDeclUseMap, ts.getUseDeclMap, fm_ts)._1)
                            fw.close()
                            println(ast)
                            println(ts.checkDefuse(ast, ts.getDeclUseMap, ts.getUseDeclMap, fm_ts)._1)
                            println(ts.getDeclUseMap)
                        } else {
                            println("generating the declaration-usage map unsuccessful because of type errors in source file")
                        }
                    }
                    if (opt.ifdeftoif) {
                        if (typeCheckStatus) {
                            //ProductGeneration.typecheckProducts(fm,fm_ts,ast,opt,
                            //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                            //ProductGeneration.estimateNumberOfVariants(ast, fm_ts)
                            //val includeStructFilename = opt.getincludeStructFilename()
                            stopWatch.start("ifdeftoif")
                            println("ifdeftoif started")
                            var i: IfdefToIf = null
                            if (opt.ifdeftoifstatistics) {
                                i = new IfdefToIf with IfdefToIfStatistics
                            } else {
                                i = new IfdefToIf
                            }
                            val defUseMap = ts.getDeclUseMap
                            val useDefMap = ts.getUseDeclMap
                            val fileName = i.outputStemToFileName(opt.getOutputStem())
                            val checkIfdefToIfResult = !opt.ifdeftoifnocheck
                            val tuple = i.ifdeftoif(ast, defUseMap, useDefMap, fm, opt.getOutputStem(), stopWatch.get("lexing") + stopWatch.get("parsing"), opt.ifdeftoifstatistics, "", typecheckResult = checkIfdefToIfResult, true)
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
                        } else {
                            println("#ifdef to if transformation unsuccessful because of type errors in source file")
                        }
                    }
                    ts.errors.map(errorXML.renderTypeError(_))
                }
                if (opt.writeInterface) {
                    stopWatch.start("interfaces")
                    val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

                    stopWatch.start("writeInterfaces")
                    ts.writeInterface(interface, new File(opt.getInterfaceFilename))
                    if (opt.writeDebugInterface)
                        ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
                }
                if (opt.dumpcfg) {
                    stopWatch.start("dumpCFG")

                    val cf = new CInterAnalysisFrontend(ast, fm_ts)
                    val writer = new CFGCSVWriter(new FileWriter(new File(opt.getCCFGFilename)))
                    val dotwriter = new DotGraph(new FileWriter(new File(opt.getCCFGDotFilename)))
                    cf.writeCFG(opt.getFile, new ComposedWriter(List(dotwriter, writer)))
                }

                if (opt.staticanalyses) {
                    val sa = new CIntraAnalysisFrontend(ast, ts.asInstanceOf[CTypeSystemFrontend with CTypeCache with CDeclUse], fm_ts)
                    if (opt.warning_double_free) {
                        stopWatch.start("doublefree")
                        sa.doubleFree()
                    }
                    if (opt.warning_uninitialized_memory) {
                        stopWatch.start("uninitializedmemory")
                        sa.uninitializedMemory()
                    }
                    if (opt.warning_case_termination) {
                        stopWatch.start("casetermination")
                        sa.caseTermination()
                    }
                    if (opt.warning_xfree) {
                        stopWatch.start("xfree")
                        sa.xfree()
                    }
                    if (opt.warning_dangling_switch_code) {
                        stopWatch.start("danglingswitchcode")
                        sa.danglingSwitchCode()
                    }
                    if (opt.warning_cfg_in_non_void_func) {
                        stopWatch.start("cfginnonvoidfunc")
                        sa.cfgInNonVoidFunc()
                    }
                    if (opt.warning_stdlib_func_return) {
                        stopWatch.start("checkstdlibfuncreturn")
                        sa.stdLibFuncReturn()
                    }
                    if (opt.warning_dead_store) {
                        stopWatch.start("deadstore")
                        sa.deadStore()
                    }
                }

            }

        }
        stopWatch.start("done")
        errorXML.write()
        if (opt.recordTiming)
            println(stopWatch)

    }

    private def writeInterface(ast: AST, fm: FeatureModel, opt: FrontendOptions) {
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

    def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.LexerFrontend().run(opt, opt.parse)
        val in = CLexerAdapter.prepareTokens(tokens)
        in
    }

    private def serializeAST(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    private def loadSerializedAST(filename: String): TranslationUnit = try {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()
        ast
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); null
    }
}
