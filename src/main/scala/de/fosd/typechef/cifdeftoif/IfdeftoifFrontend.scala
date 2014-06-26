package de.fosd.typechef.cifdeftoif


import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.ErrorXML
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.lexer.LexerFrontend
import de.fosd.typechef.options._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}


object IfdeftoifFrontend extends App with Logging with EnforceTreeHelper {

    private var opt: IfdefToIfOptions = new IfdefToIfOptions()
    private var i: IfdefToIf = new IfdefToIf

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
        opt = new IfdefToIfOptions()
        i = new IfdefToIf
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
                if (opt.ifdeftoif) {
                    ast = i.prepareASTforIfdef(ast)
                }

                if (ast != null && opt.serializeAST) {
                    stopWatch.start("serialize")
                    serializeAST(ast, opt.getSerializedTUnitFilename)
                }

            }

            if (ast != null) {
                // some dataflow analyses require typing information
                val ts = new CTypeSystemFrontend(ast, fullFM, opt) with CTypeCache with CDeclUse

                if (opt.typecheck || opt.writeInterface) {
                    //PrCDeclUseoductGeneration.typecheckProducts(fm,fullFM,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fullFM)

                    stopWatch.start("typechecking")
                    println("type checking")
                    val typeCheckStatus = ts.checkAST()
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

    private def lex(opt: IfdefToIfOptions): TokenReader[CToken, CTypeContext] =
        CLexerAdapter.prepareTokens(new LexerFrontend().run(opt, opt.parse))

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
