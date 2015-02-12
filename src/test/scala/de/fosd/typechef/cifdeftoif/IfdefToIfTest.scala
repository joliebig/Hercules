package de.fosd.typechef.cifdeftoif

import java.io._

import de.fosd.typechef.conditional.{Choice, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureModel, SingleFeatureExpr}
import de.fosd.typechef.lexer.FeatureExprLib
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import org.junit.{Ignore, Test}

import scala.io.Source
import scala.sys.process._

class IfdefToIfTest extends ConditionalNavigation with ASTNavigation with CDeclUse with CTypeSystem with TestHelper with EnforceTreeHelper {

    de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

    val makeAnalysis = true
    val writeFilesIntoIfdeftoifFolder = true
    val checkForExistingFiles = true
    val typeCheckResult = true

    val filesToAnalysePerRun = 15
    val i = new IfdefToIf with IfdefToIfStatistics
    val path = new File("..").getCanonicalPath() ++ "/ifdeftoif/"
    val singleFilePath = new File("..").getCanonicalPath() ++ "/single_files/"
    val busyBoxPath = "../TypeChef-BusyboxAnalysis/busybox-1.18.5/"
    val busyBoxFmPath = "../Typechef-BusyboxAnalysis/"
    val linuxPath = "../TypeChef-LinuxAnalysis"
    val ifdeftoifTestPath = new File(".").getCanonicalPath() ++ "/src/test/resources/ifdeftoif_testfiles/"
    val True = FeatureExprFactory.True
    var filesTransformed = 0
    /* val tb = java.lang.management.ManagementFactory.getThreadMXBean
  val time = tb.getCurrentThreadCpuTime // Type long; beware in nanoseconds */

    def appendToFile(fileName: String, textData: String) = {
        using(new FileWriter(fileName, true)) {
            fileWriter => using(new PrintWriter(fileWriter)) {
                printWriter => printWriter.print(textData)
            }
        }
    }

    /**
     * Used for reading/writing to database, files, etc.
     * Code From the book "Beginning Scala"
     * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
     */
    def using[A <: {def close() : Unit}, B](param: A)(f: A => B): B =
        try {
            f(param)
        } finally {
            param.close()
        }

    def featureNameToFExprSet(featureNames: List[String]): Set[SingleFeatureExpr] = {
        featureNames.map(x => FeatureExprFactory.createDefinedExternal(x.toUpperCase)).toSet
    }

    def testMultipleFileSemantics(file: File, inputsAndResults: List[Tuple2[Int, Int]], enabledFeatures: Set[SingleFeatureExpr] = Set(), writeAst: Boolean = false, featureModel: FeatureModel = FeatureExprFactory.empty): Boolean = {
        new File(singleFilePath).mkdirs()
        val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
        val analyseString = "++Analyse: " + file.getName + "++"
        print(analyseString)
        for (i <- (analyseString.size / 4) until 15) {
            print("\t")
        }
        val startParsingAndTypeChecking = System.currentTimeMillis()
        val ast = i.prepareASTforIfdef(prepareAST(i.getAstFromFile(file)))
        if (ast == null) assert(false, "Could not parse input file " + file.toPath)
        val source_ast = prepareAST(ast)
        val ts = getTypeSystem(source_ast)
        //val env = createASTEnv(source_ast)
        ts.typecheckTranslationUnit(source_ast)
        val defUseMap = ts.getDeclUseMap
        val useDefMap = ts.getUseDeclMap
        val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
        print("--Parsed--")

        if (!i.checkAstSilent(source_ast)) {
            println("Please fix the type errors above in order to start the ifdeftoif transformation process!")
            return null.asInstanceOf[Boolean]
        }

        val startTransformation = System.currentTimeMillis()
        val new_ast = i.transformAst(source_ast, defUseMap, useDefMap, timeToParseAndTypeCheck, enabledFeatures)
        val timeToTransform = System.currentTimeMillis() - startTransformation
        print("\t--Transformed--\n")

        // println("\n" + PrettyPrinter.print(new_ast._1) + "\n\n")

        val startPrettyPrinting = System.currentTimeMillis()
        val ifdeftoif_File = new File(singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
        PrettyPrinter.printD(new_ast._1, ifdeftoif_File.getAbsolutePath)
        val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
        //print("\t--Printed--\n")
        if (writeAst) {
            writeToTextFile(fileNameWithoutExtension ++ "_ast.txt", source_ast.toString())
        }

        if (makeAnalysis) {
            PrettyPrinter.printD(source_ast, singleFilePath ++ fileNameWithoutExtension ++ ".src")
            writeToTextFile(singleFilePath ++ fileNameWithoutExtension ++ ".csv", i.getCSVHeader + new_ast._2)


            // new_ast._1 is the generated ast
            // can we compile (gcc) and execute?
            // compile
            var proc: Process = sys.process.stringToProcess("gcc " + ifdeftoif_File + "  -o ifdeftoif_test.o").run
            if (proc.exitValue() != 0) {
                // blocks until command is finished
                println("gcc failed")
                (-1)
            }

            var testSuccessful = true
            // execute binary
            for (tuple <- inputsAndResults) {
                proc = sys.process.stringToProcess("./ifdeftoif_test.o " + tuple._1).run
                val id2iExecExitVal = proc.exitValue()

                // check if binary execution return value was the expected one
                if (id2iExecExitVal != tuple._2) {
                    println("Using input value (" + tuple._1 + ") the result value (" + id2iExecExitVal + ") did not match expected (" + tuple._2 + ")")
                    testSuccessful = false
                }
            }
            proc = sys.process.stringToProcess("./ifdeftoif_test.o").run
            val id2iExecExitVal = proc.exitValue()
            // cleanup (remove binary)
            proc = sys.process.stringToProcess("rm -f ifdeftoif_test.o").run
            if (proc.exitValue() < 0) {
                println("Could not remove generated binary " + ifdeftoif_File)
            }
            return testSuccessful
        } else {
            null.asInstanceOf[Boolean]
        }
    }

    @Test
    def basic_gcc_test() {
        val file = new File(ifdeftoifTestPath + "basic_gcc_test.c")
        println(i.getAstFromFile(file))
        assert(testFileSemantics(file, 8), true)
    }

    def testFileSemantics(file: File, expectedResultValue: Int, enabledFeatures: Set[SingleFeatureExpr] = Set(), writeAst: Boolean = false, featureModel: FeatureModel = FeatureExprFactory.empty): Boolean = {
        new File(singleFilePath).mkdirs()
        val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
        val analyseString = "++Analyse: " + file.getName + "++"
        print(analyseString)
        for (i <- (analyseString.size / 4) until 15) {
            print("\t")
        }
        val startParsingAndTypeChecking = System.currentTimeMillis()
        val ast = i.prepareASTforIfdef(prepareAST(i.getAstFromFile(file)))
        if (ast == null) assert(false, "Could not parse input file " + file.toPath)
        val source_ast = prepareAST(ast)
        val ts = getTypeSystem(source_ast)
        //val env = createASTEnv(source_ast)
        ts.typecheckTranslationUnit(source_ast)
        val defUseMap = ts.getDeclUseMap
        val useDefMap = ts.getUseDeclMap
        val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
        print("--Parsed--")

        if (!i.checkAstSilent(source_ast)) {
            println("Please fix the type errors above in order to start the ifdeftoif transformation process!")
            return null.asInstanceOf[Boolean]
        }

        val startTransformation = System.currentTimeMillis()
        val new_ast = i.transformAst(source_ast, defUseMap, useDefMap, timeToParseAndTypeCheck, enabledFeatures)
        val timeToTransform = System.currentTimeMillis() - startTransformation
        print("\t--Transformed--\n")

        // println("\n" + PrettyPrinter.print(new_ast._1) + "\n\n")

        val startPrettyPrinting = System.currentTimeMillis()
        val ifdeftoif_File = new File(singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
        PrettyPrinter.printD(new_ast._1, ifdeftoif_File.getAbsolutePath)
        val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
        //print("\t--Printed--\n")
        if (writeAst) {
            writeToTextFile(fileNameWithoutExtension ++ "_ast.txt", source_ast.toString())
        }

        if (makeAnalysis) {
            PrettyPrinter.printD(source_ast, singleFilePath ++ fileNameWithoutExtension ++ ".src")
            writeToTextFile(singleFilePath ++ fileNameWithoutExtension ++ ".csv", i.getCSVHeader + new_ast._2)


            // new_ast._1 is the generated ast
            // can we compile (gcc) and execute?
            // compile
            var proc: Process = sys.process.stringToProcess("gcc " + ifdeftoif_File + "  -o ifdeftoif_test.o").run
            if (proc.exitValue() != 0) {
                // blocks until command is finished
                println("gcc failed")
                (-1)
            }
            // execute binary
            proc = sys.process.stringToProcess("./ifdeftoif_test.o").run
            val id2iExecExitVal = proc.exitValue()
            // cleanup (remove binary)
            proc = sys.process.stringToProcess("rm -f ifdeftoif_test.o").run
            if (proc.exitValue() < 0) {
                println("Could not remove generated binary " + ifdeftoif_File)
            }
            // check if binary execution return value was the expected one
            if (id2iExecExitVal != expectedResultValue) {
                println("Result value (" + id2iExecExitVal + ") did not match expected (" + expectedResultValue + ")")
                (id2iExecExitVal)
            }
            return id2iExecExitVal == expectedResultValue
        } else {
            null.asInstanceOf[Boolean]
        }
    }

    @Test
    def switch_case_default() {
        val file = new File(ifdeftoifTestPath + "switch_case_default.c")
        assert(testFileSemanticsComplete(file, List(0, 1, 2, 3, 4)))
    }

    def testFileSemanticsComplete(file: File, inputs: List[Int], featureModel: FeatureModel = FeatureExprFactory.empty): Boolean = {
        new File(singleFilePath).mkdirs()
        val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
        val analyseString = "++Transforming: " + file.getName + "++"
        print(analyseString)
        for (i <- (analyseString.size / 4) until 15) {
            print("\t")
        }
        val startParsingAndTypeChecking = System.currentTimeMillis()
        val ast = i.getAstFromFile(file)
        if (ast == null) assert(false, "Could not parse input file " + file.toPath)
        val source_ast = i.prepareASTforIfdef(prepareAST(ast))
        val ts = getTypeSystem(source_ast)
        //val env = createASTEnv(source_ast)
        ts.typecheckTranslationUnit(source_ast)
        val defUseMap = ts.getDeclUseMap
        val useDefMap = ts.getUseDeclMap
        val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
        print("--Parsed--")

        if (!i.checkAstSilent(source_ast)) {
            println("Please fix the type errors above in order to start the ifdeftoif transformation process!")
            return null.asInstanceOf[Boolean]
        }

        val startTransformation = System.currentTimeMillis()
        val new_ast = i.testAst(source_ast, defUseMap, useDefMap, timeToParseAndTypeCheck)
        //println(PrettyPrinter.print(source_ast) + "\n\n" + ast)
        val testTriple = new_ast._2.map(x => (x, x.map(y => {
            y match {
                case (fExpr, true) =>
                    "-D " + fExpr.feature
                case (fExpr, false) =>
                    "-U " + fExpr.feature
            }
        }).mkString(" "), x.map(y => {
            y match {
                case (fExpr, true) =>
                    fExpr
                case (fExpr, false) =>
                    fExpr.not()
            }
        }).foldLeft(FeatureExprFactory.True)((fst, snd) => fst.and(snd))))

        val timeToTransform = System.currentTimeMillis() - startTransformation
        print("\t--Transformed--\n")

        // println("\n" + PrettyPrinter.print(new_ast._1) + "\n\n")

        val startPrettyPrinting = System.currentTimeMillis()
        val ifdeftoif_File = new File(singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
        i.printWithInclude(new_ast._1, ifdeftoif_File.getAbsolutePath)
        val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
        //print("\t--Printed--\n")


        var testSuccessful = true

        for (param <- testTriple) {
            // new_ast._1 is the generated ast
            // can we compile (gcc) and execute?
            // compile
            val assignments = param._1
            val gccOpts = param._2
            val configuration = param._3

            // IFDEFTOIF BINARY
            // generate ifdeftoif feature assignment struct
            i.writeExternIfdeftoIfStructT(assignments)
            var proc = sys.process.stringToProcess("gcc " + ifdeftoif_File + "  -o ifdeftoif_test.o").run
            if (proc.exitValue() != 0) {
                // blocks until command is finished
                println("gcc failed on ifdef")
                (-1)
            }

            // ORIGINAL BINARY
            proc = sys.process.stringToProcess("gcc " + file + " " + gccOpts + " -o test.o").run
            if (proc.exitValue() != 0) {
                // blocks until command is finished
                println("gcc failed on original")
                (-1)
            }

            for (input <- inputs) {
                // execute ifdeftoif binary
                proc = sys.process.stringToProcess("./ifdeftoif_test.o " + input).run
                val id2iExecExitVal = proc.exitValue()

                // execute original binary
                proc = sys.process.stringToProcess("./test.o " + input).run
                val originalExitVal = proc.exitValue()

                // check if ifdef binary execution return value equals original binary execution return value
                if (id2iExecExitVal != originalExitVal) {
                    println("Using configuration \t-| " + configuration + " |-   \ttest fails for input value: " + input + ". Original returns: (" + originalExitVal + "), ifdeftoif returns (" + id2iExecExitVal + ")")
                    testSuccessful = false
                } else {
                    println("Using configuration \t-| " + configuration + " |-   \ttest successful for input value: " + input)
                }
            }
            println("")
        }

        var proc = sys.process.stringToProcess("rm -f ifdeftoif_test.o").run
        if (proc.exitValue() < 0) {
            println("Could not remove generated binary " + ifdeftoif_File)
        }
        proc = sys.process.stringToProcess("rm -f test.o").run
        if (proc.exitValue() < 0) {
            println("Could not remove generated binary " + file)
        }
        return testSuccessful
    }

    @Test
    def switch_case() {
        val file = new File(ifdeftoifTestPath + "switch_case.c")
        assert(testFileSemanticsComplete(file, List(0, 1, 2, 3)))
    }

    @Test
    def switch_case_2() {
        val file = new File(ifdeftoifTestPath + "switch_case_2.c")
        assert(testFileSemanticsComplete(file, List(0, 1, 2, 3, 4)))
    }

    @Test
    def switch_case_default_2() {
        val file = new File(ifdeftoifTestPath + "switch_case_default_2.c")
        assert(testFileSemanticsComplete(file, List(0, 1, 2, 3, 4, 5)))
    }

    @Test
    def switch_case_default_3() {
        val file = new File(ifdeftoifTestPath + "switch_case_default_3.c")
        assert(testFileSemanticsComplete(file, List(0, 1, 2, 3)))
    }

    @Test
    def variable_condition_test_1() {
        val file = new File(ifdeftoifTestPath + "variable_condition_1.c")
        assert(testFileSemanticsComplete(file, List((0))))
    }

    @Test
    def variable_condition_test_2() {
        val file = new File(ifdeftoifTestPath + "variable_condition_2.c")
        assert(testFileSemanticsComplete(file, List((0))))
    }

    @Test
    def variable_condition_test_3() {
        val file = new File(ifdeftoifTestPath + "variable_condition_3.c")
        assert(testFileSemanticsComplete(file, List((0))))
    }

    @Test
    def liftingExpr() {
        val fa = FeatureExprFactory.createDefinedExternal("a")
        val s = parseExpr(
            """
            a
            #ifdef B
            +b
            #endif
            """.stripMargin)
        val o = Opt(fa, s)
        val env = CASTEnv.createASTEnv(o)
        val r = i.liftVariability(s, env)
        println(r)
    }

    @Ignore
    def liftingDecl() {
        val fa = FeatureExprFactory.createDefinedExternal("a")
        val s = parseDecl(
            """
            int i = 1
            #ifdef B
            +b
            #endif
            ;
            """.stripMargin)
        val o = Opt(fa, s)
        val env = CASTEnv.createASTEnv(o)
        val r = i.liftVariability(s, env)
        println(r)
    }

    @Ignore
    def liftingStmt() {
        val fa = FeatureExprFactory.createDefinedExternal("a")
        val s = parseStmt(
            """
            i = 1
            #ifdef B
            +b
            #endif
            ;
            """.stripMargin)
        val o = Opt(fa, s)
        val env = CASTEnv.createASTEnv(o)
        val r = i.liftVariability(s, env)
        println(r)
    }

    @Ignore
    def liftingStmtChoice() {
        val fa = FeatureExprFactory.createDefinedExternal("a")
        val s1 = parseStmt(
            """
            i = 1
            #ifdef B
            +b
            #endif
            ;
            """.stripMargin)
        val s2 = parseStmt(";")
        val c = Choice(fa, One(s1), One(s2))
        val env = CASTEnv.createASTEnv(c)
        val r = i.liftVariability(s1, env)
        println(r)
    }

    @Ignore
    def array_test {
        val source_ast = getAST( """
        static const unsigned opt_flags[] = {
        	LIST_SHORT | STYLE_COLUMNS, /* C */
        	DISP_HIDDEN | DISP_DOT,     /* a */
        	DISP_NOLIST,                /* d */
        	LIST_INO,                   /* i */
        	LIST_LONG | STYLE_LONG,     /* l - remember LS_DISP_HR in mask! */
        	LIST_SHORT | STYLE_SINGLE,  /* 1 */
        	0,                          /* g (don't show owner) - handled via OPT_g */
        	LIST_ID_NUMERIC,            /* n */
        	LIST_BLOCKS,                /* s */
        	DISP_ROWS,                  /* x */
        	0,                          /* Q (quote filename) - handled via OPT_Q */
        	DISP_HIDDEN,                /* A */

        #if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
        1
        #endif
        #if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
        0
        #endif
         * LIST_CONTEXT, /* k (ignored if !SELINUX) */
        #if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
        	TIME_CHANGE | (
        #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
        1
        #endif
        #if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
        0
        #endif
         * SORT_CTIME),   /* c */
        	LIST_FULLTIME,              /* e */

        #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
        1
        #endif
        #if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
        0
        #endif
         * SORT_MTIME,   /* t */
        	TIME_ACCESS | (
        #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
        1
        #endif
        #if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
        0
        #endif
         * SORT_ATIME),   /* u */
        #endif
        #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
        	SORT_SIZE,                  /* S */
        	SORT_EXT,                   /* X */
        	SORT_REVERSE,               /* r */
        	SORT_VERSION,               /* v */
        #endif
        #if definedEx(CONFIG_FEATURE_LS_FILETYPES)
        	LIST_FILETYPE | LIST_EXEC,  /* F */
        	LIST_FILETYPE,              /* p */
        #endif
        #if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
        	FOLLOW_LINKS,               /* L */
        #endif
        #if definedEx(CONFIG_FEATURE_LS_RECURSIVE)
        	DISP_RECURSIVE,             /* R */
        #endif
        #if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
        	LS_DISP_HR,                 /* h */
        #endif
        #if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
        	LIST_MODEBITS|LIST_NLINKS|LIST_CONTEXT|LIST_SIZE|LIST_DATE_TIME, /* K */
        #endif
        #if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
        	LIST_MODEBITS|LIST_ID_NAME|LIST_CONTEXT, /* Z */
        #endif
        	(1U<<31)
        	/* options after Z are not processed through opt_flags:
        	 * T, w - ignored
        	 */
        };
                                 """);
        println(source_ast)
        println(testAst(source_ast))
    }

    def testAst(source_ast: TranslationUnit): String = {
        typecheckTranslationUnit(source_ast)
        val defUseMap = getDeclUseMap
        val useDefMap = getUseDeclMap

        val optionsAst = i.generateIfdefOptionsTUnit(source_ast)
        val newAst = i.transformAst(prepareAST(source_ast), defUseMap, useDefMap, 0)._1
        ("+++New Code+++\n" + PrettyPrinter.print(newAst))
    }

    @Ignore
    def array_test2 {
        val source_ast = getAST( """
static const char * const azCompileOpt[] = {
#if definedEx(SQLITE_CHECK_PAGES)
  "CHECK_PAGES",
#endif
#if definedEx(SQLITE_COVERAGE_TEST)
  "COVERAGE_TEST",
#endif
#if definedEx(SQLITE_ENABLE_CEROD)
  "ENABLE_CEROD",
#endif
#if definedEx(SQLITE_ENABLE_COLUMN_METADATA)
  "ENABLE_COLUMN_METADATA",
#endif
#if definedEx(SQLITE_ENABLE_EXPENSIVE_ASSERT)
  "ENABLE_EXPENSIVE_ASSERT",
#endif
#if (definedEx(SQLITE_ENABLE_FTS4) || definedEx(SQLITE_ENABLE_FTS3))
  "ENABLE_FTS3",
#endif
#if definedEx(SQLITE_ENABLE_FTS4)
  "ENABLE_FTS4",
#endif
#if definedEx(SQLITE_ENABLE_IOTRACE)
  "ENABLE_IOTRACE",
#endif
#if definedEx(SQLITE_ENABLE_LOCKING_STYLE)
  "ENABLE_LOCKING_STYLE=" "SQLITE_ENABLE_LOCKING_STYLE",
#endif
#if definedEx(SQLITE_ENABLE_MEMORY_MANAGEMENT)
  "ENABLE_MEMORY_MANAGEMENT",
#endif
#if definedEx(SQLITE_ENABLE_MEMSYS3)
  "ENABLE_MEMSYS3",
#endif
#if definedEx(SQLITE_ENABLE_MEMSYS5)
  "ENABLE_MEMSYS5",
#endif
#if definedEx(SQLITE_ENABLE_OVERSIZE_CELL_CHECK)
  "ENABLE_OVERSIZE_CELL_CHECK",
#endif
#if definedEx(SQLITE_ENABLE_UPDATE_DELETE_LIMIT)
  "ENABLE_UPDATE_DELETE_LIMIT",
#endif
#if definedEx(SQLITE_HAS_CODEC)
  "HAS_CODEC",
#endif
#if definedEx(SQLITE_HAVE_ISNAN)
  "HAVE_ISNAN",
#endif
#if definedEx(SQLITE_HOMEGROWN_RECURSIVE_MUTEX)
  "HOMEGROWN_RECURSIVE_MUTEX",
#endif
  "MAX_MMAP_SIZE=" "0",
#if definedEx(SQLITE_NO_SYNC)
  "NO_SYNC",
#endif
#if definedEx(SQLITE_OMIT_ANALYZE)
  "OMIT_ANALYZE",
#endif
#if definedEx(SQLITE_OMIT_ATTACH)
  "OMIT_ATTACH",
#endif
#if definedEx(SQLITE_OMIT_AUTOVACUUM)
  "OMIT_AUTOVACUUM",
#endif
#if definedEx(SQLITE_OMIT_BLOB_LITERAL)
  "OMIT_BLOB_LITERAL",
#endif
#if definedEx(SQLITE_OMIT_BUILTIN_TEST)
  "OMIT_BUILTIN_TEST",
#endif
#if definedEx(SQLITE_OMIT_CAST)
  "OMIT_CAST",
#endif
#if definedEx(SQLITE_OMIT_DECLTYPE)
  "OMIT_DECLTYPE",
#endif
#if definedEx(SQLITE_OMIT_DEPRECATED)
  "OMIT_DEPRECATED",
#endif
#if definedEx(SQLITE_OMIT_EXPLAIN)
  "OMIT_EXPLAIN",
#endif
#if definedEx(SQLITE_OMIT_FOREIGN_KEY)
  "OMIT_FOREIGN_KEY",
#endif
#if definedEx(SQLITE_OMIT_LOAD_EXTENSION)
  "OMIT_LOAD_EXTENSION",
#endif
#if definedEx(SQLITE_OMIT_OR_OPTIMIZATION)
  "OMIT_OR_OPTIMIZATION",
#endif
#if definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  "OMIT_PAGER_PRAGMAS",
#endif
#if definedEx(SQLITE_OMIT_PRAGMA)
  "OMIT_PRAGMA",
#endif
#if definedEx(SQLITE_OMIT_SUBQUERY)
  "OMIT_SUBQUERY",
#endif
#if definedEx(SQLITE_OMIT_VACUUM)
  "OMIT_VACUUM",
#endif
#if definedEx(SQLITE_OMIT_VIEW)
  "OMIT_VIEW",
#endif
#if definedEx(SQLITE_RTREE_INT_ONLY)
  "RTREE_INT_ONLY",
#endif
#if definedEx(SQLITE_SMALL_STACK)
  "SMALL_STACK",
#endif
  "THREADSAFE=" "1",
};
                                 """);
        println(source_ast)
        println(testAst(source_ast))
    }

    @Test
    def test_alex_1() {
        val file = new File(ifdeftoifTestPath + "1.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_2() {
        val file = new File(ifdeftoifTestPath + "2.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    def testFile(file: File, writeAst: Boolean = false, featureModel: FeatureModel = FeatureExprFactory.empty): (Int, TranslationUnit) = {
        new File(singleFilePath).mkdirs()
        val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
        val analyseString = "++Analyse: " + file.getName + "++"
        print(analyseString)
        for (i <- (analyseString.size / 4) until 15) {
            print("\t")
        }
        val startParsingAndTypeChecking = System.currentTimeMillis()
        val source_ast = i.prepareASTforIfdef(prepareAST(i.getAstFromFile(file)))
        if (source_ast == null) assert(false, "Could not parse input file " + file.toPath)
        val ts = getTypeSystem(source_ast)
        //val env = createASTEnv(source_ast)
        ts.typecheckTranslationUnit(source_ast)
        val defUseMap = ts.getDeclUseMap
        val useDefMap = ts.getUseDeclMap
        val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
        print("--Parsed--")

        if (!i.checkAstSilent(source_ast)) {
            println("Please fix the type errors above in order to start the ifdeftoif transformation process!")
            return (0, TranslationUnit(List()))
        }

        val startTransformation = System.currentTimeMillis()
        val new_ast = i.transformAst(source_ast, defUseMap, useDefMap, timeToParseAndTypeCheck)
        val timeToTransform = System.currentTimeMillis() - startTransformation
        print("\t--Transformed--")
        //println("\n" + PrettyPrinter.print(new_ast._1))

        val startPrettyPrinting = System.currentTimeMillis()
        PrettyPrinter.printD(new_ast._1, singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
        val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
        print("\t--Printed--\n")
        if (writeAst) {
            writeToTextFile(fileNameWithoutExtension ++ "_ast.txt", source_ast.toString())
        }

        if (makeAnalysis) {
            //if (!(new File(singleFilePath ++ fileNameWithoutExtension ++ ".src")).exists) {
            PrettyPrinter.printD(source_ast, singleFilePath ++ fileNameWithoutExtension ++ ".src")
            //}
            /*val linesOfCodeBefore = Source.fromFile(new File(singleFilePath ++ fileNameWithoutExtension ++ ".src")).getLines().size
            val linesOfCodeAfter = Source.fromFile(new File(singleFilePath ++ fileNameWithoutExtension ++ ".ifdeftoif")).getLines().size
            val codeDifference = computeDifference(linesOfCodeBefore, linesOfCodeAfter)
            val csvBeginning = file.getName() + "," + linesOfCodeBefore + "," + linesOfCodeAfter + "," + codeDifference + ","*/


            //val csvEntry = i.createCsvEntry(source_ast, new_ast._1, fileNameWithoutExtension, timeToParseAndTypeCheck, timeToTransform)
            writeToTextFile(singleFilePath ++ fileNameWithoutExtension ++ ".csv", i.getCSVHeader + new_ast._2)
            val resultFile = new File(singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
            val result_ast = i.getAstFromFile(resultFile)

            // new_ast._1 is the generated ast
            // result_ast is the ast parsed from the generated file
            // 1. is the generated ast ok?
            val wellTypedAST = i.checkAst(new_ast._1)
            if (wellTypedAST) {
                println("\t--TypeCheck: " + true + "--\n")
            } else {
                println("\t--TypeCheck: " + false + "--\n")
            }
            assert(wellTypedAST, "generated AST is not well typed")

            // 2. is the generated file well typed?
            println(PrettyPrinter.print(result_ast))
            val wellTypedFile = i.checkAst(result_ast)
            assert(wellTypedFile, "generated file is not well typed or could not be parsed")
            // 3. does it still contain #if statements?
            val containsIfdef = i.hasVariableNodes(result_ast)
            val fileContent = Source.fromFile(resultFile).getLines().mkString("\n")
            assert(!containsIfdef,
                "generated file contains #if statements")
            // return number of nodes in generated AST

            // everything should be ok
            //println(fileContent)
            (new_ast._2.split(",")(3).toInt, new_ast._1)
        } else {
            (0, TranslationUnit(List()))
        }
    }

    @Test
    def test_alex_3() {
        val file = new File(ifdeftoifTestPath + "3.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_4() {
        val file = new File(ifdeftoifTestPath + "4.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_5() {
        val file = new File(ifdeftoifTestPath + "5.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_6() {
        val file = new File(ifdeftoifTestPath + "6.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_7() {
        val file = new File(ifdeftoifTestPath + "7.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_8() {
        val file = new File(ifdeftoifTestPath + "8.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_9() {
        val file = new File(ifdeftoifTestPath + "9.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_10() {
        val file = new File(ifdeftoifTestPath + "10.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_11() {
        val file = new File(ifdeftoifTestPath + "11.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_12() {
        val file = new File(ifdeftoifTestPath + "12.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_13() {
        val file = new File(ifdeftoifTestPath + "13.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_14() {
        val file = new File(ifdeftoifTestPath + "14.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_15() {
        val file = new File(ifdeftoifTestPath + "15.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_16() {
        val file = new File(ifdeftoifTestPath + "16.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_17() {
        val file = new File(ifdeftoifTestPath + "17.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test
    def test_alex_18() {
        val file = new File(ifdeftoifTestPath + "18.c")
        println(i.getAstFromFile(file))
        val ast: TranslationUnit = testFile(file)._2
        val search = CompoundStatement(List(Opt(FeatureExprFactory.True, LabelStatement(Id("skip"), None))))
        // the ast must not contain the search ast
        // in the search ast, a label is used at the end of a compound statement, which is forbidden by gcc
        assert(!ast.toString.contains(search.toString), "GCC Error: label at end of compound statement")
    }

    @Test
    def test_alex_19() {
        val file = new File(ifdeftoifTestPath + "19.c")
        println(i.getAstFromFile(file))
        val ast: TranslationUnit = testFile(file)._2
        val search = CompoundStatement(List(Opt(FeatureExprFactory.True, LabelStatement(Id("skip"), None))))
        // bug was/is that a referenced label (next_link) was deleted from the ast
        val labelRef = GotoStatement(Id("next_link"))
        val labelDef = LabelStatement(Id("next_link"), None)
        assert(!ast.toString.contains(labelRef.toString) || ast.toString.contains(labelDef.toString), "label \"next_link\" removed but still referenced")
    }

    @Ignore def test_typedef_function_usage() {
        val file = new File(ifdeftoifTestPath + "typedef_function_usage.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Ignore def busybox_file_tests() {
        val fs = File.separator
        val files = List(new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "archival" + fs + "libarchive" + fs + "header_verbose_list.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "libbb" + fs + "correct_password.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "libbb" + fs + "lineedit.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "libbb" + fs + "procps.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "loginutils" + fs + "getty.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "loginutils" + fs + "passwd.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "loginutils" + fs + "sulogin.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "brctl.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "httpd.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "ifconfig.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "inetd.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "ip.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "nc.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "procps" + fs + "ps.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "procps" + fs + "top.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "sysklogd" + fs + "logread.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "sysklogd" + fs + "syslogd_and_logger.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "util-linux" + fs + "fbset.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "util-linux" + fs + "fdisk.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "util-linux" + fs + "fsck_minix.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "util-linux" + fs + "mkfs_vfat.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "util-linux" + fs + "mount.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "telnetd.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "tftp.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "udhcp" + fs + "common.pi")
            , new File(busyBoxPath + fs + "busybox-1.18.5" + fs + "networking" + fs + "udhcp" + fs + "dhcpc.pi"))
        val busyboxFM: FeatureModel = FeatureExprLib.featureModelFactory.create(new FeatureExprParser(FeatureExprLib.l).parseFile(
            busyBoxPath + fs + "busybox" + fs + "featureModel"))
        files.foreach(x => {
            testFile(x, featureModel = busyboxFM)
            println("\n")
        })
    }

    @Ignore def multiple_declarations_test() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "2.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "2.c"))
    }

    @Ignore def conditional_declaration_assignments() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditionalDeclarationAssignments.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditionalDeclarationAssignments.c"))
    }

    @Ignore def for_loop() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "for_loop.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "for_loop.c"))
    }

    @Ignore def conditional_expression() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditional_expression.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditional_expression.c"))
    }

    @Ignore def conditional_expression2() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditional_expression2.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditional_expression2.c"))
    }

    @Ignore def compareTypeCheckingTimes() {
        val applets = new File(busyBoxPath + "applets/applets.pi")
        val tr = new File(busyBoxPath + "coreutils/tr.pi")
        val bbunzip = new File(busyBoxPath + "archival/bbunzip.pi")
        val cal = new File(busyBoxPath + "coreutils/cal.pi")
        val ln = new File(busyBoxPath + "coreutils/ln.pi")
        val halt = new File(busyBoxPath + "init/halt.pi")
        val dump = new File(busyBoxPath + "libbb/dump.pi")
        val dc = new File(busyBoxPath + "miscutils/dc.pi")
        val inotifyd = new File(busyBoxPath + "miscutils/inotifyd.pi")
        val unzip = new File(busyBoxPath + "archival/unzip.pi")
        val hdpam = new File(busyBoxPath + "miscutils/hdparm.pi")


        val list = List(applets, tr, bbunzip, cal, ln, halt, dump, dc, inotifyd, unzip, hdpam)
        val csvEntries = list.map(x => (x.getName(), compareTypeChecking(x))).map(y => y._1 + "," + y._2._1.toString + "," + y._2._2.toString + "," + i.computeDifference(y._2._1, y._2._2).toString + "\n") mkString
        val csvHeader = "File name, Type check source, Type check result, Difference\n"
        writeToTextFile(path ++ "type_check.csv", csvHeader + csvEntries)
    }

    private def writeToTextFile(name: String, content: String) {
        val fw = new FileWriter(name)
        fw.write(content)
        fw.close()
    }

    private def compareTypeChecking(file: File): Tuple2[Long, Long] = {
        val source_ast = i.getAstFromFile(file)
        val defuses = getDefUse(source_ast)
        val result_ast = i.transformAst(prepareAST(source_ast), defuses._1, defuses._2, 0)._1
        val ts_source = getTypeSystem(source_ast)
        val ts_result = getTypeSystem(result_ast)

        val typeCheckSourceStart = System.currentTimeMillis()
        ts_source.checkASTSilent
        val typeCheckSourceDuration = System.currentTimeMillis() - typeCheckSourceStart
        print("++" + file.getName() + "++\n" + "TypeCheck Source: \t\t" + typeCheckSourceDuration + "\t\t\t\t")

        val typeCheckResultStart = System.currentTimeMillis()
        ts_result.checkASTSilent
        val typeCheckResultDuration = System.currentTimeMillis() - typeCheckResultStart
        print("TypeCheck Result: \t\t" + typeCheckResultDuration + "\n\n")

        (typeCheckSourceDuration, typeCheckResultDuration)
    }

    def getTypeSystem(ast: AST): CTypeSystemFrontend with CTypeCache with CDeclUse = {
        new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit]) with CTypeCache with CDeclUse
    }

    private def getDefUse(ast: TranslationUnit): (IdentityIdHashMap, IdentityIdHashMap) = {
        typecheckTranslationUnit(ast)
        (getDeclUseMap, getUseDeclMap)
    }

    private def transformSingleFile(filename: String, directory: String) {
        var foundFile = false
        val dirToAnalyse = new File(directory)

        def transformPiFiles(dirToAnalyse: File) {
            if (!foundFile) {
                // retrieve all pi from dir first
                if (dirToAnalyse.isDirectory) {
                    val piFiles = dirToAnalyse.listFiles(new FilenameFilter {
                        def accept(dir: File, file: String): Boolean =
                            file.equals(filename + ".pi")
                    })
                    val dirs = dirToAnalyse.listFiles(new FilenameFilter {
                        def accept(dir: File, file: String) = dir.isDirectory
                    })
                    if (!piFiles.isEmpty) {
                        foundFile = true
                        testFile(piFiles.head)
                    } else {
                        for (dir <- dirs) {
                            transformPiFiles(dir)
                        }
                    }

                }
            }
        }
        if (dirToAnalyse.exists()) {
            new File(path).mkdirs()
            if (!checkForExistingFiles || !(new File(path ++ "statistics.csv").exists)) {
                writeToFile(path ++ "statistics.csv", i.getCSVHeader)
            }

            transformPiFiles(dirToAnalyse)
        }
    }

    private def transformDir(dirToAnalyse: File) {
        def transformPiFiles(dirToAnalyse: File) {
            if (filesTransformed < filesToAnalysePerRun) {
                // retrieve all pi from dir first
                if (dirToAnalyse.isDirectory) {
                    val piFiles = dirToAnalyse.listFiles(new FilenameFilter {
                        def accept(dir: File, file: String): Boolean = file.endsWith(".pi")
                    })
                    val dirs = dirToAnalyse.listFiles(new FilenameFilter {
                        def accept(dir: File, file: String) = dir.isDirectory
                    })
                    for (piFile <- piFiles) {
                        runIfdefToIfOnPi(piFile)
                    }
                    for (dir <- dirs) {
                        transformPiFiles(dir)
                    }
                }
            }
        }
        new File(path).mkdirs()
        if (!checkForExistingFiles || !(new File(path ++ "statistics.csv").exists)) {
            writeToFile(path ++ "statistics.csv", i.getCSVHeader)
        }
        transformPiFiles(dirToAnalyse)
    }

    def writeToFile(fileName: String, data: String) =
        using(new FileWriter(fileName)) {
            fileWriter => fileWriter.write(data)
        }

    private def runIfdefToIfOnPi(file: File, featureModel: FeatureModel = FeatureExprFactory.empty) {
        if (filesTransformed < filesToAnalysePerRun) {
            val filePathWithoutExtension = i.getFileNameWithoutExtension(file.getPath())
            val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
            val transformedFileExists = (writeFilesIntoIfdeftoifFolder && new File(path ++ fileNameWithoutExtension ++ "_ifdeftoif.c").exists) || (!writeFilesIntoIfdeftoifFolder && new File(filePathWithoutExtension ++ "_ifdeftoif.c").exists)
            var fileName = file.getName()

            if (!checkForExistingFiles || !transformedFileExists) {
                /*for (i <- (analyseString.size / 4) until 15) {
                  print("\t")
                }*/

                filesTransformed = filesTransformed + 1

                val startParsingAndTypeChecking = System.currentTimeMillis()
                val source_ast = i.getAstFromFile(file)
                typecheckTranslationUnit(source_ast)
                val defUseMap = getDeclUseMap
                val useDefMap = getUseDeclMap
                val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
                //print("--Parsed--")

                val tuple = i.ifdeftoif(prepareAST(source_ast), defUseMap, useDefMap, featureModel, fileNameWithoutExtension, timeToParseAndTypeCheck, makeAnalysis, path ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
                tuple._1 match {
                    case None =>
                        println("!! Transformation of " ++ fileName ++ " unsuccessful because of type errors in transformation result !!")
                    case Some(x) =>
                        //PrettyPrinter.printD(x, path ++ fileNameWithoutExtension ++ ".ifdeftoif")
                        println("++Transformed: " ++ fileName ++ "++\t\t --in " + tuple._2 ++ " ms--")
                }

                val startTransformation = System.currentTimeMillis()
                val new_ast = i.transformAst(prepareAST(source_ast), defUseMap, useDefMap, 0)
                val timeToTransform = System.currentTimeMillis() - startTransformation
                //print("\t--Transformed--")

                val startPrettyPrinting = System.currentTimeMillis()
                if (writeFilesIntoIfdeftoifFolder) {
                    PrettyPrinter.printD(new_ast._1, path ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
                } else {
                    //writeToTextFile(filePathWithoutExtension ++ ".ifdeftoif", transformedCode)
                    PrettyPrinter.printD(new_ast._1, filePathWithoutExtension ++ "_ifdeftoif.c")
                }
                val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
                //print("\t--Printed--\n")
            }
        }
    }

    private def analyseDir(dirToAnalyse: File): List[Tuple2[TranslationUnit, String]] = {
        // retrieve all pi from dir first
        if (dirToAnalyse.isDirectory) {
            val piFiles = dirToAnalyse.listFiles(new FilenameFilter {
                def accept(dir: File, file: String): Boolean = file.endsWith(".pi")
            })
            val dirs = dirToAnalyse.listFiles(new FilenameFilter {
                def accept(dir: File, file: String) = dir.isDirectory
            })
            piFiles.toList.map(x => {
                val fis = new FileInputStream(x)
                val ast = parseFile(fis, x.getName, x.getParent)
                fis.close()
                (ast, x.getName)
            }) ++ dirs.flatMap(x => analyseDir(x))
        } else {
            List()
        }
    }

    private def countFiles(dirToAnalyse: File, fileExtension: String = ".pi"): Int = {
        def countHelp(file: File): Int = {
            if (file.isDirectory) {
                val piFiles = file.listFiles(new FilenameFilter {
                    def accept(dir: File, fileName: String): Boolean = fileName.endsWith(fileExtension)
                })
                val dirs = file.listFiles(new FilenameFilter {
                    def accept(dir: File, fileName: String) = dir.isDirectory
                })
                var numberOfFiles = piFiles.size
                for (dir <- dirs) {
                    numberOfFiles = numberOfFiles + countHelp(dir)
                }
                numberOfFiles
            } else {
                0
            }
        }
        countHelp(dirToAnalyse)
    }
}