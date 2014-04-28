package de.fosd.typechef.cifdeftoif

import org.junit.{Ignore, Test}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import java.io._
import java.util
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExprParser, FeatureExprFactory}
import de.fosd.typechef.lexer.FeatureExprLib
import io.Source
import scala.Some
import de.fosd.typechef.parser.c.VoidSpecifier
import de.fosd.typechef.parser.c.DoubleSpecifier
import de.fosd.typechef.conditional.One
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.Constant
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.AtomicNamedDeclarator
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.typesystem.IdentityIdHashMap
import de.fosd.typechef.parser.c.IntSpecifier
import de.fosd.typechef.parser.c.FunctionDef
import scala.Tuple2
import de.fosd.typechef.parser.c.StaticSpecifier

class IfdefToIfTest extends ConditionalNavigation with ASTNavigation with CDeclUse with CTypeSystem with TestHelper with EnforceTreeHelper {
    val makeAnalysis = true
    val writeFilesIntoIfdeftoifFolder = true
    val checkForExistingFiles = true
    val typeCheckResult = true

    val filesToAnalysePerRun = 15
    var filesTransformed = 0

    val i = new IfdefToIf with IfdefToIfStatistics
    val path = new File("..").getCanonicalPath() ++ "/ifdeftoif/"
    val singleFilePath = new File("..").getCanonicalPath() ++ "/single_files/"
    val busyBoxPath = "../TypeChef-BusyboxAnalysis/busybox-1.18.5/"
    val busyBoxFmPath = "../Typechef-BusyboxAnalysis/"
    val linuxPath = "../TypeChef-LinuxAnalysis"
    val ifdeftoifTestPath = new File(".").getCanonicalPath() ++ "/CRewrite/src/test/resources/ifdeftoif_testfiles/"
    val True = FeatureExprFactory.True
    /* val tb = java.lang.management.ManagementFactory.getThreadMXBean
  val time = tb.getCurrentThreadCpuTime // Type long; beware in nanoseconds */

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

    def getTypeSystem(ast: AST): CTypeSystemFrontend with CTypeCache with CDeclUse = {
        new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit]) with CTypeCache with CDeclUse
    }

    def writeToFile(fileName: String, data: String) =
        using(new FileWriter(fileName)) {
            fileWriter => fileWriter.write(data)
        }

    def appendToFile(fileName: String, textData: String) = {
        using(new FileWriter(fileName, true)) {
            fileWriter => using(new PrintWriter(fileWriter)) {
                printWriter => printWriter.print(textData)
            }
        }
    }

    def testFile(file: File, writeAst: Boolean = false, featureModel: FeatureModel = FeatureExprFactory.empty): Int = {
        new File(singleFilePath).mkdirs()
        val fileNameWithoutExtension = i.getFileNameWithoutExtension(file)
        val analyseString = "++Analyse: " + file.getName + "++"
        print(analyseString)
        for (i <- (analyseString.size / 4) until 15) {
            print("\t")
        }
        val startParsingAndTypeChecking = System.currentTimeMillis()
        val ast = i.getAstFromFile(file)
        val source_ast = prepareAST(ast)
        val ts = getTypeSystem(source_ast)
        //val env = createASTEnv(source_ast)
        ts.typecheckTranslationUnit(source_ast)
        val defUseMap = ts.getDeclUseMap
        val useDefMap = ts.getUseDeclMap
        val timeToParseAndTypeCheck = System.currentTimeMillis() - startParsingAndTypeChecking
        print("--Parsed--")

        val startTransformation = System.currentTimeMillis()
        val new_ast = i.transformAst(source_ast, defUseMap, useDefMap, timeToParseAndTypeCheck)
        val timeToTransform = System.currentTimeMillis() - startTransformation
        print("\t--Transformed--")
        //println("\n" + PrettyPrinter.print(new_ast._1))

        val startPrettyPrinting = System.currentTimeMillis()
        PrettyPrinter.printF(new_ast._1, singleFilePath ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
        val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
        print("\t--Printed--\n")
        if (writeAst) {
            writeToTextFile(fileNameWithoutExtension ++ "_ast.txt", source_ast.toString())
        }

        if (makeAnalysis) {
            //if (!(new File(singleFilePath ++ fileNameWithoutExtension ++ ".src")).exists) {
            PrettyPrinter.printF(source_ast, singleFilePath ++ fileNameWithoutExtension ++ ".src")
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
            new_ast._2.split(",")(3).toInt
        } else {
            0
        }
    }

    def testAst(source_ast: TranslationUnit): String = {
        typecheckTranslationUnit(source_ast)
        val defUseMap = getDeclUseMap
        val useDefMap = getUseDeclMap

        val optionsAst = i.getOptionFile(source_ast)
        val newAst = i.transformAst(prepareAST(source_ast), defUseMap, useDefMap, 0)._1
        ("+++New Code+++\n" + PrettyPrinter.print(newAst))
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

    private def getDefUse(ast: TranslationUnit): (IdentityIdHashMap, IdentityIdHashMap) = {
        typecheckTranslationUnit(ast)
        (getDeclUseMap, getUseDeclMap)
    }

    @Ignore def test_function() {
        val ast = getAST( """
      #if definedEx(A)
      int
      #elif !definedEx(B)
      double
      #else
      short
      #endif
      foo(int jahreseinkommen) {
        return 0;
      }

      #if !definedEx(C)
      int
      #elif definedEx(D)
      double
      #else
      short
      #endif
      foo2(int jahreseinkommen) {
        return 0;
      }

      bar3() {
        foo(3);
        foo2(3);
      }

       bar5() {
        foo2(5);
        foo(5);
      }
                          """)
        println(ast)
        println(testAst(ast))
    }

    @Ignore def test_function2() {
        val ast2 = getAST( """
      #if definedEx(A)
      static void
      #if definedEx(B)
      long
      #else
      int
      #endif
      mainz(int one
      #if definedEx(C)
      , int two
      #endif
      ) {
        return 0;
      }
      #endif
                           """)
        println(ast2)
        println(testAst(ast2) + "\n\n")

        val ast3 = getAST( """
      static void
      #if definedEx(B)
      long
      #endif
      #if !definedEx(B)
      int
      #endif
      mainz(int one
      #if definedEx(C)
      , int two
      #endif
      ) {
        return 0;
      }
                           """)
        println(ast3)
        println(testAst(ast3) + "\n\n")

        val ast = getAST( """
      #if definedEx(G) || definedEx(B)
      #if !definedEx(G) || !definedEx(B)
      #endif
      static void
      #if definedEx(C) && (definedEx(G) || definedEx(B)) && (!definedEx(G) || !definedEx(B))
      vfork(int tar_fd)
      #endif
      #if (!definedEx(C) || (!definedEx(G) && !definedEx(B)) || (definedEx(G) && definedEx(B)))
      vfork(int tar_fd, int gzip)
      #endif
      {
      int i;
      }
      #endif
                          """)
        println(testAst(ast))
    }

    @Ignore def test_text2() {
        val ast = getAST( """
      #if definedEx(A)
      int
      #elif !definedEx(B)
      double
      #else
      short
      #endif
      foo() {
        println("Hello World!");
        return 0;
      }

      main(void) {
        foo();
      }
                          """)
        println(testAst(ast))
    }

    @Ignore def test_tri() {
        val ast = getAST( """
      main(void) {
        int i = 5
        #if definedEx(Add)
        +
        #else
        -
        #endif
        2;
      }
                          """)
        println(ast)
        println(PrettyPrinter.print(ast))
        println(testAst(ast))
    }

    @Ignore def test_switch1 {
        val source_ast = getAST( """
      void foo_01(int a) {
        int optA = 1;
        #if definedEx(A)
        int i = 10;
        #else
        int i = 20;
        #endif
        #if definedEx(C)
        int c = 5;
        #endif
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          #if definedEx(B)
          case 2: printf("in 2\n"); break;
          #endif
          case c: printf("in c\n"); break;
          case 3: printf("in 3\n"); break;
          case i: printf("in i\n"); break;
        }
      }
                                 """)
        println(source_ast)
        println(testAst(source_ast))
    }


    @Ignore def test_switch2 {
        val source_ast = getAST( """
      void foo_02(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          #if definedEx(A)
          case 2: printf("in 2\n");
          #endif
          case 3: printf("in 3\n"); break;
        }
      }
                                 """)
        val target_ast = getAST( """
      void foo_02(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          case 2: if (optA) {
            printf("in 2\n");
          } else {
            goto sexit;
          }
          case 3: printf("in 3\n"); break;
        }
      sexit:;
      }
                                 """)

        println(testAst(source_ast))
    }


    @Ignore def test_switch3 {
        val source_ast = getAST( """
      void foo_03(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          #if definedEx(A)
          case 2: printf("in 2A\n"); break;
          #endif
          #if definedEx(B)
          case 2: printf("in 2B\n"); break;
          #endif
          #if definedEx(C)
          case 2: printf("in 2C\n"); break;
          #else
          case 2: printf("in !2C\n"); break;
          #endif
          case 3: printf("in 3\n"); break;
        }
      }
                                 """)
        val target_ast = getAST( """
      void foo_03(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          case 2: if (options.a) {
            printf("in 2A\n");
          }
          if (! options.a) {
            printf("in !2A\n"); break;
          }
          case 3: printf("in 3\n"); break;
        }
      }
                                 """)
        println(testAst(source_ast))
    }


    @Ignore def test_switch4 {
        val source_ast = getAST( """
      void foo_04(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          #if definedEx(A)
          case 2: printf("in 2\n"); break;
          #endif
          case 3: printf("in 3\n"); break;
          default: printf("in default\n"); break;
        }
      }
                                 """)
        val target_ast = getAST( """
      void foo_04(int a) {
        int optA = 1;
        switch (a) {
          case 0: printf("in 0\n"); break;
          case 1: printf("in 1\n"); break;
          case 2: if (options.a) {
            printf("in 2\n");
          }
          if (options.a) {
            break;
          }
          if (! options.a) {
            goto sdefault;
          }
          case 3: printf("in 3\n"); break;
          sdefault:;
          default: printf("in default\n"); break;
        }
      }
                                 """)
        println(testAst(source_ast))
    }

    @Ignore def if_test {
        val source_ast = getAST( """
      void foo_04(int a) {
      int i;
      #if definedEx(A)
      i = 32;
      #elif definedEx(B)
      i = 64;
      #else
      i = 128;
      #endif
      if (i) {
        return 0;
      }
      }
                                 """)
        println(source_ast)
        println(testAst(source_ast))

    }

    @Ignore def optional_if_test {
        val source_ast = getAST( """
      void foo_04(int a) {
      #if definedEx(A)
      if (a) {
        a = a * a;
      }
      #endif
      }
                                 """)
        println(source_ast)
        println(testAst(source_ast))

    }

    @Ignore def if_test2 {
        val source_ast = getAST( """
      void foo_04(int a) {
      int i = 0;
      if (i) {
        i = 32;
      }
      #if definedEx(A)
      else {
        i = 64;
      }
      #endif
      }
                                 """)
        val target_ast = getAST( """
      void foo_04(int a) {
        int i = 0;
        if (i) {
          i = 32;
        } else if (options.a) {
          i = 64;
        }
      }
                                 """)
        println(source_ast)
        println(testAst(source_ast))

    }

    @Ignore def if_test3 {
        val source_ast = getAST( """
      void foo_04(int a) {
      #if definedEx(A)
      int
      #else
      short
      #endif
      i = 0;
      if (i < 0) {
        i = 32;
      }
      #if definedEx(A)
      else if (i < 1) {
        i = 64;
      }
      #endif
      else {
        i = 128;
      }
      }
                                 """)
        val target_ast = getAST( """
      void foo_04(int a) {
        int _1_i = 0;
        short _2_i = 0;
        if (i < 0) {
          i = 32;
        } else if (options.a) {
          i = 64;
        }
      }
                                 """)
        println(source_ast)
        println(target_ast)
        println(testAst(source_ast))

    }

    @Ignore def test_jump {
        val source_ast = getAST( """
      int main(void) {
        goto j1;
        goto j2;
        goto j3;

        #if definedEx(A)
        j1:
        #endif
        #if definedEx(B)
        j1:
        #endif
        #if definedEx(C)
        j1:
        #endif
        #if definedEx(D)
        j2:
        #else
        j3:
        #endif

        return 0;
      }
                                 """);
        val target_ast = getAST( """
      int main(void) {
        if(options.a) {
          goto _0_j1;
        }
        if(options.b) {
          goto _1_j1;
        }
        if(options.c) {
          goto _2_j1;
        }

        if(options.d) {
          goto _3_j2;
        }

        if(! options.d) {
          goto _4_j3;
        }

        if(options.a) {
          _0_j1:
        }
        if(options.b) {
          _1_j1:
        }
        if(options.c) {
          _2_j1:
        }
        if(options.d) {
          _3_j2:
        }
        if(! options.d) {
          _4_j3:
        }

        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_int {
        val source_ast = getAST( """
      int main(void) {
        #if definedEx(A)
        int i = 8;
        #elif definedEx(B)
        int i = 16;
        #elif definedEx(C)
        int i = 32;
        #else
        int i = 64;
        #endif


        #if definedEx(D)
        int j = 32;
        #else
        int j = 64;
        #endif

        i = i*i;
        j = 2*j;
        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_int2 {
        val source_ast = getAST( """
      int main(void) {
        #if definedEx(A)
        int i = 8;
        #endif
        #if definedEx(B)
        int i = 16;
        #endif

        #if definedEx(D)
        int j = 32;
        #else
        int j = 64;
        #endif

        i = i*i;
        j = 2*j;
        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def ac_test {
        val source_ast = getAST( """
      #ifdef CONFIG_ACPI_PROCFS_POWER
      static const struct file_operations acpi_ac_fops = {
        .owner = THIS_MODULE,
        .open = acpi_ac_open_fs,
        .read = seq_read,
        .llseek = seq_lseek,
        .release = single_release,
        };
        #endif
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def array_test {
        val source_ast = getAST( """
      static const unsigned opt_flags[] = {
        #ifdef CONFIG_ACPI_PROCFS_POWER
        SORT_SIZE,
        #endif
        0
        };
                                 """);
        println(source_ast)
        println(testAst(source_ast))
    }

    @Ignore def normal_struct {
        val source_ast = getAST( """
      static const struct file_operations acpi_ac_fops = {
        .owner = THIS_MODULE,
        .open = acpi_ac_open_fs,
        .read = seq_read,
        .llseek = seq_lseek,
        .release = single_release,
        };
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_opt_in_struct {
        val source_ast = getAST( """
      const unsigned int e2attr_flags_value[] = {
      #ifdef ENABLE_COMPRESSION
	      EXT2_COMPRBLK_FL,
	      EXT2_DIRTY_FL,
	      EXT2_NOCOMPR_FL,
      	EXT2_ECOMPR_FL,
     #endif
	      EXT2_INDEX_FL,
	      EXT2_SECRM_FL,
      	EXT2_UNRM_FL,
	      EXT2_SYNC_FL,
	      EXT2_DIRSYNC_FL,
	      EXT2_IMMUTABLE_FL,
	      EXT2_APPEND_FL,
	      EXT2_NODUMP_FL,
	      EXT2_NOATIME_FL,
	      EXT2_COMPR_FL,
	      EXT3_JOURNAL_DATA_FL,
	      EXT2_NOTAIL_FL,
	      EXT2_TOPDIR_FL
    };

                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_opt_struct {
        val source_ast = getAST( """
      #ifdef ENABLE_COMPRESSION
      const unsigned int e2attr_flags_value[] = {
	      EXT2_COMPRBLK_FL,
	      EXT2_DIRTY_FL,
	      EXT2_NOCOMPR_FL,
      	EXT2_ECOMPR_FL,
      };
      #endif
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_opt_int {
        val source_ast = getAST( """
      int main(void) {
        #if definedEx(A)
        int i = 8;
        #endif
        #if definedEx(B)
        int i = 16;
        #endif
        #if definedEx(C)
        int i = 32;
        #else
        int i = 64;
        #endif

        #if definedEx(D)
        int j = 32;
        #else
        int j = 64;
        #endif

        i = i*i;
        j = 2*j;
        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_int_def_use {
        val source_ast = getAST( """
      int foo(int *x, int z) {
        int i2 = x + 5;
        i2 = 5;
        int y;
        y = 5;
        return x + i2 + z;
      }
      int main(void) {
        int i = 0;
        i = i + 1;
        foo(i);
        int b = 666;
        foo(b);

        int if3 = 5;
        if (if3 == 5) {
          if3 = 10;
        } else {
          if3 = 30;
        }
        int for4;
        for (for4 = 0; for4 < 10; for4++) {
          println(for4);
        }
        int j;
        j = 10;
        i = (j * (j*(j-(j+j)))) - (j*j) + j;
        return (i > j) ? i : j;
      }
                                 """);
        println(source_ast)
        println(testAst(source_ast))
    }

    @Ignore def test_array_def_use {
        val source_ast = getAST( """
      #ifdef awesome
        #define quadrat(q) ((q)*(q))
      #endif
      const int konst = 55;
      int foo(int arr[5], int z) {
        arr[0] = 10;
        arr[1] = 5;
        arr[2] = (arr[0] + arr[1]) * arr[0];
        int x = 5;
        int i2 = x + 5;
        i2 = z;
        int y;
        y = konst;
        konst = 5;
        int missing = 3;
        y = missing;
        int variable;
        #ifdef awesome
          variable = 4;
          int noType = 3;
          int onlyHere = 3;
          z = onlyHere;
          y = quadrat(z);
        #else
          variable = 7;
          float noType = 7;
        #endif
        noType += noType;
        return variable;
      }
      int main(void) {
        int a[5];
        char c;
        c = 'a';



        a[konst] = 0;
        int plusgleich = 10;
        plusgleich += 5;
        int funktion;
        foo(a[5], funktion);
        int plusplus = 1;
        plusplus++;
        return plusgleich;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Ignore def test_struct_def_use {
        // TODO Verwendung struct variablen.
        val source_ast = getAST( """
      struct leer;

      struct student {
        int id;
        char *name;
        float percentage;
      } student1, student2, student3;

      struct withInnerStruct {
      struct innerStruct{
      int inner;
      };
      int outer;
      };

      int main(void) {
        struct student st;
        struct student st2 = {10, "Joerg Liebig", 0.99};

        st.id = 5;
        student3.id = 10;
        int i = student1.id;

        student2.name = "Joerg";
        student3.name = "Andi";

        student3.percentage = 90.0;


        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Test def test_opt_def_use {
        val source_ast = getAST( """
      int o = 32;
      int fooZ();
      int fooZ() {
        #if definedEx(AA)
        const int konst = 55;
        int c = 32;
        #else
        int c = 64;
        const int konst = 100;
        #endif
        o = c+o;
        return c;
      }
      int foo(int z) {
        return z;
      }
      int fooVariableArgument(
      #if definedEx(AA)
      int
      #else
      float
      #endif
      a) {
        return 0;
      }
      #if definedEx(AA)
      int fooA(int a) {
        return a;
      }
      #else
      void fooA(int a) {

      }
      #endif
      int main(void) {
        #if definedEx(AA)
        int b = fooA(0);
        int argInt = 2;
        fooVariableArgument(argInt);
        #else
        float argFloat = 2.0;
        fooVariableArgument(argFloat);
        fooA(0);
        #endif
        #if definedEx(AA)
        int bb = 0
        #endif
        return 0;
      }
                                 """);
        println(testAst(source_ast))
    }

    @Test def test_pi() {
        val file = new File(ifdeftoifTestPath + "variable_enumerator.c")
        println(testFile(file))
    }

    @Test def test_alex_1() {
        val file = new File(ifdeftoifTestPath + "1.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_2() {
        val file = new File(ifdeftoifTestPath + "2.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_3() {
        val file = new File(ifdeftoifTestPath + "3.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_4() {
        val file = new File(ifdeftoifTestPath + "4.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_5() {
        val file = new File(ifdeftoifTestPath + "5.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_6() {
        val file = new File(ifdeftoifTestPath + "6.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_7() {
        val file = new File(ifdeftoifTestPath + "7.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_8() {
        val file = new File(ifdeftoifTestPath + "8.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_9() {
        val file = new File(ifdeftoifTestPath + "9.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_10() {
        val file = new File(ifdeftoifTestPath + "10.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_11() {
        val file = new File(ifdeftoifTestPath + "11.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_12() {
        val file = new File(ifdeftoifTestPath + "12.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_13() {
        val file = new File(ifdeftoifTestPath + "13.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_14() {
        val file = new File(ifdeftoifTestPath + "14.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_15() {
        val file = new File(ifdeftoifTestPath + "15.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_16() {
        val file = new File(ifdeftoifTestPath + "16.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }
    @Test def test_alex_17() {
        val file = new File(ifdeftoifTestPath + "17.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Test def test_opt_flags() {
        val file = new File(ifdeftoifTestPath + "opt_flags.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }


    @Test def test_typedef_function_usage() {
        val file = new File(ifdeftoifTestPath + "typedef_function_usage.c")
        println(i.getAstFromFile(file))
        testFile(file)
    }

    @Ignore def test_applets_pi() {
        val file = new File(busyBoxPath + "applets/applets.pi")
        testFile(file)
    }

    @Ignore def test_stat_pi() {
        val file = new File(busyBoxPath + "coreutils/stat.pi")
        testFile(file)
    }

    @Ignore def test_alex_pi() {
        val file = new File(linuxPath + "linux-3.4/drivers/usb/gadget/r8a66597-udc.pi")
        val macroFilter = new util.ArrayList[String]()
        macroFilter.add("x:CONFIG_")
        val fm = new FeatureExprParser(FeatureExprLib.l()).parseFile(new File(linuxPath + "approx.fm"))
        //testFile(file)

        de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

        val parse = System.currentTimeMillis()
        val ast = i.getAstFromFile(file)
        println("Parsing took: " + ((System.currentTimeMillis() - parse) / 1000) + "s")
    }

    @Ignore def test_cpio_pi() {
        val file = new File(busyBoxPath + "archival/cpio.pi")
        testFile(file)

        /*val ast = getAstFromFile(file)
        val defuse = getDeclUseMap()
        i.ifdeftoif(ast, defuse)*/
    }

    @Ignore def test_update_passwd_pi() {
        val file = new File(busyBoxPath + "libbb/update_passwd.pi")
        testFile(file)
    }

    @Ignore def test_tr_pi() {
        val file = new File(busyBoxPath + "coreutils/tr.pi")
        testFile(file)
    }

    @Ignore def test_fold_pi() {
        val file = new File(busyBoxPath + "coreutils/fold.pi")
        testFile(file)
    }

    @Ignore def test_lzop_pi() {
        val file = new File(busyBoxPath + "archival/lzop.pi")
        testFile(file)
    }

    @Ignore def test_rpm2cpio_pi() {
        val file = new File(busyBoxPath + "archival/rpm2cpio.pi")
        testFile(file)
    }

    @Ignore def test_filter_accept_all_pi() {
        val file = new File(busyBoxPath + "archival/libarchive/filter_accept_all.pi")
        testFile(file)
    }

    @Test def test_decompress_unzip_pi() {
        val file = new File(busyBoxPath + "archival/libarchive/decompress_unzip.pi")
        testFile(file)
    }

    @Ignore def test_ar_pi() {
        val file = new File(busyBoxPath + "archival/ar.pi")
        testFile(file)
    }

    @Ignore def test_file() {
        val file = new File(busyBoxPath + "archival/rpm.pi")
        testFile(file)
    }

    @Ignore def test_tar_pi() {
        val file = new File(busyBoxPath + "archival/tar.pi")
        testFile(file)
    }

    @Test def single_busybox_file_test() {
        val filename = "touch"
        transformSingleFile(filename, busyBoxPath)
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

    @Test def test_bbunzip_pi() {
        val fs = File.separator
        val busyboxFM: FeatureModel = FeatureExprLib.featureModelFactory.create(new FeatureExprParser(FeatureExprLib.l).parseFile(
            busyBoxFmPath + fs + "busybox" + fs + "featureModel"))
        val file = new File(busyBoxPath + "archival" + fs + "bbunzip.pi")
        //println(testFile(file, featureModel = busyboxFM))
        assert(testFile(file, featureModel = busyboxFM).equals(82423))
    }

    @Ignore def test_chpst_pi() {
        val file = new File(busyBoxPath + "runit/chpst.pi")
        testFile(file)
    }

    @Ignore def test_diff_pi() {
        val file = new File(busyBoxPath + "editors/diff.pi")
        testFile(file)
    }

    @Ignore def test_ls_pi() {
        val file = new File(busyBoxPath + "coreutils/ls.pi")
        testFile(file)
    }

    @Ignore def test_sed_pi() {
        val file = new File(busyBoxPath + "editors/sed.pi")
        testFile(file)
    }

    @Ignore def test_lineedit_pi() {
        val file = new File(busyBoxPath + "libbb/lineedit.pi")
        testFile(file)
    }

    @Ignore def test_if_conditional() {
        val source_ast = getAST( """
    int main(void) {
    int a = 0;
    int b = -2;
    if (a < 0
    #if definedEx(A)
    && b < 0
    #endif
    #if !definedEx(A)
    && b < 10
    #endif
    #if definedEx(B)
    && b < 20
    #endif
    #if definedEx(C)
                                   || b < 12
    #endif
    ) {
      int i = 1;
    #if definedEx(A)
      i = 5;
    #endif
    #if definedEx(C)
      i = 10;
    #endif
    i = i*i;
    }}""")
        println(source_ast)
        println(testAst(source_ast))
    }

    @Ignore def test_if_choice() {
        val source_ast = getAST( """
    int main(void) {
    int a = 0;
    int b = -2;
    #if definedEx(B)
    if (
    #if definedEx(A)
    b < 0
    #else
    b > 0
    #endif
    ) {
      int i = 1;
      #if definedEx(A)
      i = 2 + i;
      #endif
      #if definedEx(C)
      i = i*i;
      #endif
    }
    #endif
    }""")
        println(source_ast)
        println(testAst(source_ast))
    }

    @Ignore def test_if_choice2() {
        val source_ast = getAST( """
		union {
      int a;
		} magic;
    enum {
    BZIP2_MAGIC = 0
    };

    int main(void) {
    int a = 0;
    int b = -2;
    if (
    #if definedEx(A)
    1
    #endif
    #if !definedEx(A)
    0
    #endif
    && magic.a == BZIP2_MAGIC) {
      int i = 1;
    }
    }""")
        println(testAst(source_ast))
    }

    @Ignore def enum_test() {
        val source_ast = getAST(
            """
        enum  {
          PSSCAN_PID = (1 << 0),
          PSSCAN_PPID = (1 << 1),
          PSSCAN_PGID = (1 << 2),
          PSSCAN_SID = (1 << 3),
          PSSCAN_UIDGID = (1 << 4),
          PSSCAN_COMM = (1 << 5),
          PSSCAN_ARGV0 = (1 << 7),
          PSSCAN_EXE = (1 << 8),
          PSSCAN_STATE = (1 << 9),
          PSSCAN_VSZ = (1 << 10),
          PSSCAN_RSS = (1 << 11),
          PSSCAN_STIME = (1 << 12),
          PSSCAN_UTIME = (1 << 13),
          PSSCAN_TTY = (1 << 14),
          PSSCAN_SMAPS = ((1 << 15)
          #if definedEx(CONFIG_FEATURE_TOPMEM)
          * 1
          #endif

          #if !definedEx(CONFIG_FEATURE_TOPMEM)
          * 0
          #endif
          ),
          PSSCAN_ARGVN = ((1 << 16)
          #if definedEx(CONFIG_KILLALL)
          * (1
          #if (definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PGREP))
              || 1
          #endif

          #if (definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PGREP) && (!definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PGREP)))
              || 0
          #endif

          #if (definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PKILL))
              || 1
          #endif

          #if (definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PKILL) && (!definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PKILL)))
              || 0
          #endif

          #if (definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PIDOF))
              || 1
          #endif

          #if (definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PIDOF) && (!definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PIDOF)))
              || 0
          #endif

          #if (definedEx(CONFIG_KILLALL) && definedEx(CONFIG_SESTATUS))
              || 1
          #endif

          #if (definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_SESTATUS) && (!definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_SESTATUS)))
              || 0
          #endif
          )
          #endif

          #if !definedEx(CONFIG_KILLALL)
          * (0
          #if (!definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PGREP))
              || 1
          #endif

          #if (!definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PGREP) && (definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PGREP)))
              || 0
          #endif

          #if (!definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PKILL))
              || 1
          #endif

          #if (!definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PKILL) && (definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PKILL)))
              || 0
          #endif

          #if (!definedEx(CONFIG_KILLALL) && definedEx(CONFIG_PIDOF))
              || 1
          #endif

          #if (!definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_PIDOF) && (definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_PIDOF)))
              || 0
          #endif

          #if (!definedEx(CONFIG_KILLALL) && definedEx(CONFIG_SESTATUS))
              || 1
          #endif

          #if (!definedEx(CONFIG_KILLALL) && !definedEx(CONFIG_SESTATUS) && (definedEx(CONFIG_KILLALL) || !definedEx(CONFIG_SESTATUS)))
              || 0
          #endif
          )
          #endif
          ),
          PSSCAN_CONTEXT = ((1 << 17)
          #if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
          * 1
          #endif

          #if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
          * 0
          #endif
          ),
          PSSCAN_START_TIME = (1 << 18),
          PSSCAN_CPU = ((1 << 19)
          #if definedEx(CONFIG_FEATURE_TOP_SMP_PROCESS)
          * 1
          #endif

          #if !definedEx(CONFIG_FEATURE_TOP_SMP_PROCESS)
          * 0
          #endif
          ),
          PSSCAN_NICE = ((1 << 20)
          #if definedEx(CONFIG_FEATURE_PS_ADDITIONAL_COLUMNS)
          * 1
          #endif

          #if !definedEx(CONFIG_FEATURE_PS_ADDITIONAL_COLUMNS)
          * 0
          #endif
          ),
          PSSCAN_RUIDGID = ((1 << 21)
          #if definedEx(CONFIG_FEATURE_PS_ADDITIONAL_COLUMNS)
          * 1
          #endif

          #if !definedEx(CONFIG_FEATURE_PS_ADDITIONAL_COLUMNS)
          * 0
          #endif
          ),
          PSSCAN_TASKS = ((1 << 22)
          #if definedEx(CONFIG_FEATURE_SHOW_THREADS)
          * 1
          #endif

          #if !definedEx(CONFIG_FEATURE_SHOW_THREADS)
          * 0
          #endif
          ),
          PSSCAN_STAT = (PSSCAN_PPID | PSSCAN_PGID | PSSCAN_SID | PSSCAN_COMM | PSSCAN_STATE | PSSCAN_VSZ | PSSCAN_RSS | PSSCAN_STIME | PSSCAN_UTIME | PSSCAN_START_TIME | PSSCAN_TTY | PSSCAN_NICE | PSSCAN_CPU)
        } ;
            """)
        println(testAst(source_ast))

        val source_ast2 = getAST(
            """
        enum  {
          LSA_SIZEOF_SA = sizeof(union  {
            int  sa;
            int  sin;
            #if definedEx(CONFIG_FEATURE_IPV6)
            int  sin6;
            #endif
          } )
        } ;
            """)
        println(testAst(source_ast2))
        println(source_ast2)
    }

    private def writeToTextFile(name: String, content: String) {
        val fw = new FileWriter(name)
        fw.write(content)
        fw.close()
    }

    @Ignore def function_test() {
        val source_ast = getAST( """
    void open_transformer(int fd,
    #if definedEx(CONFIG_DESKTOP)
    long long
    #endif
    #if !definedEx(CONFIG_DESKTOP)

    #endif
     int (*transformer)(int src_fd, int dst_fd)) ;
                                 """)
        println(testAst(source_ast))
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
                        //PrettyPrinter.printF(x, path ++ fileNameWithoutExtension ++ ".ifdeftoif")
                        println("++Transformed: " ++ fileName ++ "++\t\t --in " + tuple._2 ++ " ms--")
                }

                val startTransformation = System.currentTimeMillis()
                val new_ast = i.transformAst(prepareAST(source_ast), defUseMap, useDefMap, 0)
                val timeToTransform = System.currentTimeMillis() - startTransformation
                //print("\t--Transformed--")

                val startPrettyPrinting = System.currentTimeMillis()
                if (writeFilesIntoIfdeftoifFolder) {
                    PrettyPrinter.printF(new_ast._1, path ++ fileNameWithoutExtension ++ "_ifdeftoif.c")
                } else {
                    //writeToTextFile(filePathWithoutExtension ++ ".ifdeftoif", transformedCode)
                    PrettyPrinter.printF(new_ast._1, filePathWithoutExtension ++ "_ifdeftoif.c")
                }
                val timeToPrettyPrint = System.currentTimeMillis() - startPrettyPrinting
                //print("\t--Printed--\n")
            }
        }
    }

    @Test def funct_test() {
        val source_ast = getAST(
            """
        #if definedEx(CONFIG_UNCOMPRESS)
        static
        #if (definedEx(CONFIG_DESKTOP) && definedEx(CONFIG_UNCOMPRESS))
        long
        #endif

        #if (definedEx(CONFIG_DESKTOP) && definedEx(CONFIG_UNCOMPRESS))
        long
        #endif
         int unpack_uncompress(int info)  {


          #if (definedEx(CONFIG_DESKTOP) && definedEx(CONFIG_UNCOMPRESS))
          long
          #endif

          #if (definedEx(CONFIG_DESKTOP) && definedEx(CONFIG_UNCOMPRESS))
          long
          #endif
           int status =  (- 1);
          if (3 < 5) {
            status = 0;
          }
          else {
            (status = 0);
          }
          return status;
        }
        #endif
            """)
        println(testAst(source_ast))
    }

    @Test def feature_test() {
        val oneVariableContradiction = FunctionDef(List(Opt(fa, StaticSpecifier()), Opt(fa.not(), VoidSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))
        val twoVariableContradiction = FunctionDef(List(Opt(fa, StaticSpecifier()), Opt(fa.not().and(fb.not()), VoidSpecifier()), Opt((fb.and(fa.not()).and(fb.or(fa))), VoidSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))
        val threeVariableContradiction = FunctionDef(List(Opt(fa, StaticSpecifier()), Opt(fa.not().and(fb.not()), VoidSpecifier()), Opt(fc.and(fa.not()).and(fb).and(fb.or(fa)), IntSpecifier()), Opt(fa.not().and(fb).and(fc.not().or(fa).or(fb.not())).and(fb.or(fa)), DoubleSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))

        val oneVariableComputation = FunctionDef(List(Opt(fa, StaticSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))
        val twoVariableComputation = FunctionDef(List(Opt(fb, StaticSpecifier()), Opt(fa.not(), VoidSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))
        val threeVariableComputation = FunctionDef(List(Opt(fb, StaticSpecifier()), Opt(fc.not(), StaticSpecifier()), Opt(fa.not(), VoidSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))
        val fiveVariableComputation = FunctionDef(List(Opt(fb, StaticSpecifier()), Opt(fc.not(), StaticSpecifier()), Opt(fa.not(), VoidSpecifier()), Opt(fx.not(), VoidSpecifier()), Opt(fy.not(), VoidSpecifier()), Opt(fc, StaticSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))

        val mixedVariableContradiction = FunctionDef(List(Opt(fa, StaticSpecifier()), Opt(fa.not().and(fb.not()), VoidSpecifier()), Opt(fc.and(fa.not()).and(fb).and(fb.or(fa)), IntSpecifier()), Opt(fa.not().and(fb).and(fc.not().or(fa).or(fb.not())).and(fb.or(fa)), DoubleSpecifier()), Opt(fx, StaticSpecifier())), AtomicNamedDeclarator(List(), Id("main"), List()), List(), CompoundStatement(List()))


        val oneContradictionResult = i.computeFeaturesForDuplication(oneVariableContradiction, FeatureExprFactory.True)
        val twoContradictionResult = i.computeFeaturesForDuplication(twoVariableContradiction, FeatureExprFactory.True)
        val threeContradictionResult = i.computeFeaturesForDuplication(threeVariableContradiction, FeatureExprFactory.True)

        val oneComputationResult = i.computeFeaturesForDuplication(oneVariableComputation, FeatureExprFactory.True)
        val twoComputationResult = i.computeFeaturesForDuplication(twoVariableComputation, FeatureExprFactory.True)
        val threeComputationResult = i.computeFeaturesForDuplication(threeVariableComputation, FeatureExprFactory.True)

        val mixedComputationResult = i.computeFeaturesForDuplication(mixedVariableContradiction, FeatureExprFactory.True)

        println("Amount of feature expressions: " + oneContradictionResult.size + ", in: " + oneContradictionResult)
        println("Amount of feature expressions: " + twoContradictionResult.size + ", in: " + twoContradictionResult)
        println("Amount of feature expressions: " + threeContradictionResult.size + ", in: " + threeContradictionResult)

        println("Amount of feature expressions: " + oneComputationResult.size + ", in: " + oneComputationResult)
        println("Amount of feature expressions: " + twoComputationResult.size + ", in: " + twoComputationResult)
        println("Amount of feature expressions: " + threeComputationResult.size + ", in: " + threeComputationResult)

        println("Amount of feature expressions: " + mixedComputationResult.size + ", in: " + mixedComputationResult)

        /*val featureList = List(fc.and(fa), fc.and(fa.not().and(fb.not())), fc.and(fa.not().and(fb).and(fb.or(fa))))
        println("C: " + i.debugNextRelevantFeatures(featureList))
        val featureList2 = List(fa, (fa.not().and(fb.not())), fa.not().and(fb).and(fb.or(fa)))
        println(i.debugNextRelevantFeatures(featureList2))
        println((fa.and(fb.not())))*/
    }

    @Ignore def option_ftest() {
        val source_ast = getAST( """
      #include "opt.h"
      extern struct sOpt opt;
      extern void initOpt();""")
        println("Source: " + source_ast)
        println("+++Pretty printed+++\n" + PrettyPrinter.print(source_ast))
    }

    @Test def multiple_declarations_test() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "2.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "2.c"))
    }

    @Test def conditional_declaration_assignments() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditionalDeclarationAssignments.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditionalDeclarationAssignments.c"))
    }
    @Test def for_loop() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "for_loop.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "for_loop.c"))
    }
    @Test def conditional_expression() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditional_expression.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditional_expression.c"))
    }
    @Test def conditional_expression2() {
        val ast = i.getAstFromFile(new File(ifdeftoifTestPath + "conditional_expression2.c"))
        println(ast)
        testFile(new File(ifdeftoifTestPath + "conditional_expression2.c"))
    }

    @Ignore def context_variableids_test() {
        println(testAst(i.getAstFromFile(new File(ifdeftoifTestPath + "context_variableids.c"))))
    }

    @Ignore def variable_struct_member_test() {
        println(testAst(i.getAstFromFile(new File(ifdeftoifTestPath + "typedef_in_struct.c"))))
    }

    @Ignore def busy_box_test() {
        val busybox = new File(busyBoxPath + "")
        if (busybox.exists()) {
            transformDir(busybox)
        }
    }

    @Test def random_test() {
        val config = new File(busyBoxFmPath + "BusyBoxDefConfig.config")

        val a = FeatureExprFactory.createDefinedExternal("A")
        val b = FeatureExprFactory.createDefinedExternal("B")
        val c = FeatureExprFactory.createDefinedExternal("C")
        val ab = a.and(b)
        val anb = a.and(b.not())
        val nab = a.not().and(b)
        val nanb = a.not().and(b.not())
        val list = List(List(a, a.not()), List(a, nab, nanb.and(c), nanb.and(c.not())), List(c, c.not()), List(a, a.not()))
        println(i.computeCarthesianProduct(list, True))
    }

    @Test def exponential_params() {
        val paramDecl = Declaration(List(Opt(True, StaticSpecifier()), Opt(True, ConstSpecifier()), Opt(True, CharSpecifier())), List(Opt(True, InitDeclaratorI(AtomicNamedDeclarator(List(), Id("params"), List(Opt(True, DeclArrayAccess(None)))), List(Opt(True, GnuAttributeSpecifier(List(Opt(True, AttributeSequence(List(Opt(True, AtomicAttribute("aligned")), Opt(True, CompoundAttribute(List(Opt(True, AttributeSequence(List(Opt(True, AtomicAttribute("1"))))))))))))))), Some(Initializer(None, StringLit(List(Opt(True, "-a\0"), Opt(True, "-o\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_NOT"), "!\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_DESKTOP"), "-and\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_DESKTOP"), "-or\0"), Opt((FeatureExprFactory.createDefinedExternal("CONFIG_DESKTOP") & FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_NOT")), "-not\0"), Opt(True, "-print\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_PRINT0"), "-print0\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_DEPTH"), "-depth\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_PRUNE"), "-prune\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_DELETE"), "-delete\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_EXEC"), "-exec\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_PAREN"), "(\0"), Opt(True, "-name\0"), Opt(True, "-iname\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_PATH"), "-path\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_REGEX"), "-regex\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_TYPE"), "-type\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_PERM"), "-perm\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_MTIME"), "-mtime\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_MMIN"), "-mmin\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_NEWER"), "-newer\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_INUM"), "-inum\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_USER"), "-user\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_GROUP"), "-group\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_SIZE"), "-size\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_CONTEXT"), "-context\0"), Opt(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_FIND_LINKS"), "-links\0")))))))))
        val ast = TranslationUnit(List(Opt(True, paramDecl)))
        println(PrettyPrinter.print(ast))
        println(testAst(ast))
    }

    @Test def test_statements() {
        val source_ast = getAST( """
    void main() {
    int i = 2
    #if definedEx(A)
    +
    #else
    -
    #endif
    2;
    i = 2*i;
    }
                                 """)
        println(source_ast)
        println(PrettyPrinter.print(prepareAST(source_ast)))
        println(testAst(source_ast))

        val source_ast2 = getAST( """
    enum {
        OPTBIT_color = 14
        #if definedEx(CONFIG_READABLE)
        * 1
        #endif
        #if !definedEx(CONFIG_READABLE)
        * 0
        #endif
    } ;
                                  """)
        println(testAst(source_ast2))

        val source_ast3 = getAST( """
    void main() {
      int j = 2
      #if definedEx(A)
      +
      #else
      -
      #endif
      2;
      j = 2*j;
      int i = 2
    #if definedEx(B)
      +
    #else
      -
    #endif
      2;
      i = 2*i;
      i = 2 * j;
    }
                                  """)
        println(testAst(source_ast3))
        println(source_ast2)
    }

    @Test def scalar_test() {
        val a = FeatureExprFactory.createDefinedExternal("A")
        val b = FeatureExprFactory.createDefinedExternal("B")
        val c = FeatureExprFactory.createDefinedExternal("C")
        val listOfLists = List(List(a, a.not()), List(), List(c, c.not()))
        println(i.computeCarthesianProduct(listOfLists, FeatureExprFactory.True))

        println(new FeatureExprParser().parse(a.and(b).or(c).toTextExpr))
    }

    @Test def feature_explosion() {
        val a = FeatureExprFactory.createDefinedExternal("A")
        val b = FeatureExprFactory.createDefinedExternal("B")
        val c = FeatureExprFactory.createDefinedExternal("C")
        val context = a.and(b)
        val typeChefMistake = a.or(b).or(c)
        val fix = i.getFeatureForContext(typeChefMistake, context)
        println("Wrong: " + typeChefMistake.implies(context).isTautology)
        println("Right: " + fix.implies(context).isTautology)
        println("Right: " + fix.implies(FeatureExprFactory.True).isTautology)
    }

    @Ignore def pretty_printer_test() {
        val file = new File(busyBoxPath + "applets/applets.pi")
        //testFile(file)
        val newFullFilePath = singleFilePath ++ i.getFileNameWithoutExtension(file) ++ "_ifdeftoif.c"
        val source_ast = i.getAstFromFile(new File(newFullFilePath))
        typecheckTranslationUnit(source_ast)
    }

    @Ignore def file_test() {
        val file = new File(ifdeftoifTestPath + "feature_test.c")

        val source_ast = i.getAstFromFile(file)
        println(source_ast.toString() ++ "\n\n")
        testFile(file)
    }

    @Test def choice_test() {
        //val c = Choice((FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_CREATE").and((FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_FROM")).not())), One(Constant("0")), One(PostfixExpr(Id("exclude_file"), FunctionCall(ExprList(List(Opt((FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_FROM").and(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_CREATE"))), PostfixExpr(Id("tbInfo"), PointerPostfixSuffix("->", Id("excludeList")))), Opt((FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_FROM").and(FeatureExprFactory.createDefinedExternal("CONFIG_FEATURE_TAR_CREATE"))), Id("header_name"))))))))
        val c = Choice(FeatureExprFactory.createDefinedExternal("A"), One(Constant("0")), Choice(FeatureExprFactory.createDefinedExternal("B"), One(Constant("1")), One(Constant("2"))))
        println(PrettyPrinter.print(ElifStatement(c, One(EmptyStatement()))))

        println("\n\n\n" + PrettyPrinter.print(ElifStatement(i.conditionalToConditionalExpr(c), One(EmptyStatement()))))
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
}