package de.fosd.typechef.cifdeftoif

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.SingleFeatureExpr
import de.fosd.typechef.parser.c.TranslationUnit
import org.kiama.rewriting.Rewriter._

import scala.io.Source

/**
 * Created by rhein on 6/2/14.
 */
object PreparedIfdeftoifParts {

    def replaceInAST(inAST: TranslationUnit, definitionsFile: File): (TranslationUnit, Set[SingleFeatureExpr]) = {
        if (!definitionsFile.exists) {
            println("prepared parts def file does not exist: " + definitionsFile.getAbsolutePath)
            return (inAST, Set())
        }
        var replacedFeatures: Set[SingleFeatureExpr] = Set()
        val (preparedParts, preparedPartsLists) =
            loadPreparedPartsFromFile(definitionsFile)
        var ret = inAST
        var total_num_matches = 0
        for ((search, repl) <- preparedParts) {
            val (retnew, num_matches) = replaceManyTD(ret, search, repl)
            total_num_matches += num_matches
            if (retnew != ret) {
                ret = retnew // found and replaced search object
                replacedFeatures ++= IfdeftoifUtils.getSingleFeatures(search)
            }
        }
        for ((search, replLst) <- preparedPartsLists) {
            val (retnew, num_matches) = replaceManyTD(ret, search, replLst)
            total_num_matches += num_matches
            if (retnew != ret) {
                ret = retnew // found and replaced search object
                replacedFeatures ++= IfdeftoifUtils.getSingleFeatures(search)
            }
        }
        if (total_num_matches > 0)
            println("did ast part replacement with " + (preparedPartsLists.size + preparedParts.size) + " prepared parts, replaced " + total_num_matches + " matches.")
        return (ret, replacedFeatures)
    }

    def loadPreparedPartsFromFile(file: File): (Map[Opt[_], Opt[_]],
        Map[Opt[_], List[Opt[_]]]) = {
        val ret: collection.mutable.Map[Opt[_], Opt[_]] = collection.mutable.Map.empty[Opt[_], Opt[_]]
        val retLsts: collection.mutable.Map[Opt[_], List[Opt[_]]] = collection.mutable.Map.empty[Opt[_], List[Opt[_]]]
        var searchString: String = ""
        var i: Int = 0;
        for (line <- Source.fromFile(file).getLines()) {
            i += 1;
            if (line.isEmpty) {
                // ignoreLine
            } else if (line.startsWith("//")) {
                // ignoreLine
            } else if (line.startsWith("searchObj: ")) {
                searchString = line.substring("searchObj: ".length)
            } else if (line.startsWith("replaceObj: ")) {
                val replaceString = line.substring("replaceObj: ".length)
                // now we have both parts; need to transform them to scala objects and add them to the map
                val search = loadSerializedObject(searchString)
                val replace = loadSerializedObject(replaceString)
                if (search.isInstanceOf[Opt[_]]) {
                    if (replace.isInstanceOf[Opt[_]])
                        ret.put(search.asInstanceOf[Opt[_]], replace.asInstanceOf[Opt[_]])
                    else if (replace.isInstanceOf[List[Opt[_]]])
                        retLsts.put(search.asInstanceOf[Opt[_]], replace.asInstanceOf[List[Opt[_]]])
                    else
                        println("preparedParts: could not parse replace Object in line " + i)
                } else {
                    println("preparedParts: could not parse search Object in line " + i)
                }
            }
        }
        return (ret.toMap, retLsts.toMap)
    }

    def loadSerializedObject(in: String): Any = try {
        val inBytes = (new sun.misc.BASE64Decoder()).decodeBuffer(in)
        val fr = new ObjectInputStream(new GZIPInputStream(new ByteArrayInputStream(inBytes))) {
            override protected def resolveClass(desc: ObjectStreamClass) = {
                /*println(desc);*/ super.resolveClass(desc)
            }
        }
        val obj = fr.readObject()
        fr.close()
        obj
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized object: " + e.getMessage); null
    }

    def replaceManyTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_]): (T, Int) = {
        var num_matches = 0
        val r = manytd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x == mark) {
                      num_matches+=1
                      replace :: Nil
                    } else
                        x :: Nil)
        })
      (r(t).get.asInstanceOf[T], num_matches)
    }

    def replaceManyTD[T <: Product](t: T, mark: Opt[_], replace: List[Opt[_]]): (T, Int) = {
        var num_matches = 0
        val r = manytd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x == mark) {
                      num_matches+=1
                      replace
                    } else
                        x :: Nil)
        })
      (r(t).get.asInstanceOf[T], num_matches)
    }

    def serializeObject(obj: Any): String = {
        val strw: StringWriter = new StringWriter()
        val out = new ByteArrayOutputStream()
        val fw = new ObjectOutputStream(new GZIPOutputStream(out))
        fw.writeObject(obj)
        fw.close()
        // linebreaks carry no semantics here
        return (new sun.misc.BASE64Encoder()).encode(out.toByteArray).replaceAll("(?:\\r\\n|\\n\\r|\\n|\\r)", "")
    }

}
