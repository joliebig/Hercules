package de.fosd.typechef

import java.io.File

import de.fosd.typechef.parser.c.{AST, TranslationUnit}

/**
 * Methods helping with simulation of CPP behavior.
 * Used if TypeChef should act like CPP.
 * (e.g. write a dependency file .d )
 *
 * Created by rhein on 5/27/14.
 */
object CPP_replacement_methods {

    def writeDependencyFile(ast: TranslationUnit, fullSubjectFileName: String, subjectBasename: String, dependencyFilePath: String): String = {
        var subjectFileNoExtension = fullSubjectFileName
        if (fullSubjectFileName.endsWith(".c"))
            subjectFileNoExtension = fullSubjectFileName.substring(0, fullSubjectFileName.length - ".c".length)
        var subjectBasenameNoExtension = subjectBasename
        if (subjectBasename.endsWith(".c"))
            subjectBasenameNoExtension = subjectBasename.substring(0, subjectBasename.length - ".c".length)

        val dependencies: Set[String] = getIncludedFiles(ast) - subjectBasename
        // dependency file name (sometimes) is .file.d (hidden)
        val dfile = new File(dependencyFilePath)
        val out = new java.io.FileWriter(dfile)
        out.write(subjectBasenameNoExtension + ".o :")
        for (dep <- dependencies)
            out.write(" " + dep)
        out.write("\n")
        out.close
        return dfile.getAbsolutePath
    }

    private def getIncludedFiles(in: Any): Set[String] = {
        var ret: Set[String] = Set()
        if (in.isInstanceOf[AST]) {
            val ast = in.asInstanceOf[AST]
            val file = ast.getFile
            if (file.isDefined) {
                var fname = file.get
                // it seems that sometimes a null pointer is stored in ast.getFile (.isDefined is true, but .get returns null)
                if (fname != null && (fname.endsWith(".c") || fname.endsWith(".h"))) {
                    if (fname.startsWith("file "))
                        fname = fname.substring("file ".length)
                    if (new File(fname).isAbsolute) {
                        ret += fname
                    } else {
                        // filenames are relative to TypeChef-Busybox directory, so we add ..
                        ret += ("../" + fname)
                    }
                }
            }
        }
        if (in.isInstanceOf[Product]) {
            val prod = in.asInstanceOf[Product]
            ret = prod.productIterator.map(getIncludedFiles(_)).
                foldLeft(ret)((B: Set[String], A: Set[String]) => A ++ B)
        } else if (in.isInstanceOf[Traversable[Any]]) {
            val trav = in.asInstanceOf[Traversable[Any]]
            ret = trav.map(getIncludedFiles(_)).
                foldLeft(ret)((B: Set[String], A: Set[String]) => A ++ B)
        }
        ret
    }
}
