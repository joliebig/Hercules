package de.fosd.typechef

import de.fosd.typechef.parser.c.{AST, TranslationUnit}
import de.fosd.typechef.featureexpr.FeatureExpr
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.cifdeftoif.IfdeftoifFrontend
import java.io.File

/**
 * Methods helping with simulation of CPP behavior.
 * Used if TypeChef should act like CPP.
 * (e.g. write a dependency file .d )
 *
 * Created by rhein on 5/27/14.
 */
object CPP_replacement_methods {

    private def getIncludedFiles(in: Any): Set[String] = {
        var ret : Set[String] = Set()
        if (in.isInstanceOf[AST]) {
            val ast = in.asInstanceOf[AST]
            val file = ast.getFile
            if (file.isDefined && ! file.isEmpty) {
                var fname = file.get
                if (fname.endsWith(".c") || fname.endsWith(".h")) {
                    if (fname.startsWith("file "))
                        fname = fname.substring("file ".length)
                    // filenames are relative to TypeChef-Busybox directory, so we add ..
                    ret += "../" + fname
                }
            }
        }
        if (in.isInstanceOf[Product]) {
            val prod = in.asInstanceOf[Product]
            ret = prod.productIterator.map(getIncludedFiles(_)).
                foldLeft(ret) ((B:Set[String], A:Set[String]) => A ++ B)
        } else if (in.isInstanceOf[Traversable[Any]]) {
            val trav = in.asInstanceOf[Traversable[Any]]
            ret = trav.map(getIncludedFiles(_)).
                foldLeft(ret) ((B:Set[String], A:Set[String]) => A ++ B)
        }
        ret
    }

    def writeDependencyFile(ast : TranslationUnit, fullSubjectFileName : String, subjectBasename : String) {
        var subjectFileNoExtension = fullSubjectFileName
        if (fullSubjectFileName.endsWith(".c"))
            subjectFileNoExtension = fullSubjectFileName.substring(0, fullSubjectFileName.length-".c".length)
        var subjectBasenameNoExtension = subjectBasename
        if (subjectBasename.endsWith(".c"))
            subjectBasenameNoExtension = subjectBasename.substring(0, subjectBasename.length-".c".length)

        val dependencies : Set[String] = getIncludedFiles(ast) - subjectBasename
        // dependency file name is .file.d (hidden)
        val dfile = new File(new File(subjectFileNoExtension).getParent, "." + subjectBasenameNoExtension + ".d")
        val out = new java.io.FileWriter(dfile)
        out.write(subjectBasenameNoExtension + ".o :")
        for (dep <- dependencies)
            out.write(" " + dep)
        out.write("\n")
        out.close
    }
}
