package de.fosd.typechef.cifdeftoif

import java.io.{File, PrintWriter}

trait IOUtilities {
    // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala

    import java.io.FileWriter

    def getFileContent(fileName: String): String = {
        //scala.io.Source.fromFile(fileName).getLines.mkString("\n")
        scala.io.Source.fromFile(fileName).mkString
    }

    def addToFile(fileName: String, textData: String) {
        if (new File(fileName).exists) {
            appendToFile(fileName, textData)
        } else {
            writeToFile(fileName, textData)
        }
    }

    def writeToFile(fileName: String, data: String) {
        using(new FileWriter(fileName)) {
            fileWriter => fileWriter.write(data)
        }
    }

    def appendToFile(fileName: String, textData: String) {
        using(new FileWriter(fileName, true)) {
            fileWriter => using(new PrintWriter(fileWriter)) {
                printWriter => printWriter.print(textData)
            }
        }
    }

    def using[A <: {def close()}, B](param: A)(f: A => B): B =
        try {
            f(param)
        } finally {
            param.close()
        }
}