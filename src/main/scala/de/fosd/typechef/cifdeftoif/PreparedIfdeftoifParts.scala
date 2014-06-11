package de.fosd.typechef.cifdeftoif

import java.io._
import scala.io.Source
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.conditional.Opt

/**
 * Created by rhein on 6/2/14.
 */
object PreparedIfdeftoifParts {

    def loadPreparedPartsFromFile(file: File): (Map[Opt[_], Opt[_]],
            Map[Opt[_], List[Opt[_]]]) = {
        val ret : collection.mutable.Map[Opt[_], Opt[_]] = collection.mutable.Map.empty[Opt[_], Opt[_]]
        val retLsts : collection.mutable.Map[Opt[_], List[Opt[_]]] = collection.mutable.Map.empty[Opt[_], List[Opt[_]]]
        var searchString : String = ""
        var i : Int = 0;
        for (line <- Source.fromFile(file).getLines()) {
            i+=1;
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

    def replaceInAST(inAST: TranslationUnit, definitionsFile : File): TranslationUnit = {
        if (! definitionsFile.exists) {
            println("prepared parts def file does not exist: " + definitionsFile.getAbsolutePath)
            return inAST
        }
        val (preparedParts, preparedPartsLists) =
                loadPreparedPartsFromFile(definitionsFile)
        var ret = inAST
        for ((search, repl) <- preparedParts) {
            ret = replaceOnceTD(ret, search, repl)
        }
        for ((search, replLst) <- preparedPartsLists) {
            ret = replaceManyTD(ret, search, replLst)
        }
        return ret
    }

    def replaceOnceTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_]): T = {
        val r = manytd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x == mark)
                        replace :: Nil
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }
    def replaceManyTD[T <: Product](t: T, mark: Opt[_], replace: List[Opt[_]]): T = {
        val r = manytd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x == mark)
                        replace
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def serializeObject(obj: Any) : String = {
        val strw : StringWriter = new StringWriter()
        val out = new ByteArrayOutputStream()
        val fw = new ObjectOutputStream(new GZIPOutputStream(out))
        fw.writeObject(obj)
        fw.close()
        // linebreaks carry no semantics here
        return (new sun.misc.BASE64Encoder()).encode(out.toByteArray).replaceAll("(?:\\r\\n|\\n\\r|\\n|\\r)", "")
    }

    def loadSerializedObject(in : String) : Any = try {
        val inBytes = (new sun.misc.BASE64Decoder()).decodeBuffer(in)
        val fr = new ObjectInputStream(new GZIPInputStream(new ByteArrayInputStream(inBytes))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val obj = fr.readObject()
        fr.close()
        obj
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized object: " + e.getMessage); null
    }

}
