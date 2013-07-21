import java.io.PrintWriter
import java.nio.charset.Charset
import java.nio.file._
import java.util
import jline.console.completer.Completer
import jline.console.ConsoleReader
import moon.{Utils, QueryTermParser}

object Example {

  type Token = String

  case class Lnode(t: Token, leafs: Lnode*)


  def main(args: Array[String]) {
    val color = true


    val path = FileSystems.getDefault().getPath("/tmp/access.log")

    val debug = Files.newBufferedWriter(path, Charset.defaultCharset(), StandardOpenOption.SYNC, StandardOpenOption.APPEND, StandardOpenOption.CREATE)



    try {
      val reader: ConsoleReader = new ConsoleReader
      reader.setPrompt("prompt> ")

      val c = new Completer {

//TODO Delete..        implicit def t2ln(t: (String, String)): Lnode = Lnode(t._1, List(Lnode(t._2)): _*)

        def complete(buffer: String, cursor: Int, candidates: util.List[CharSequence]): Int = {
          QueryTermParser.parseCommand(buffer) match {
            case Left(fpf) =>
              fpf.options.map {
                o =>
                  Utils.stripOverlapFromToken(buffer, o)
              }.foreach {
                candidates.add(_)
              }
              if (fpf.options.length > 0)
                cursor
              else
                -1
            case Right(b) =>
              cursor
          }
        }
      }
      reader.addCompleter(c)
      reader.setHistoryEnabled(true)
      var line: String = null
      val out: PrintWriter = new PrintWriter(reader.getOutput)
      while ( {
        line = reader.readLine
        line
      } != null) {
        if (color) {
          out.println("\u001B[33m======>\u001B[0m\"" + line + "\"")
        }
        else {
          out.println("======>\"" + line + "\"")
        }
        out.flush()
        if (line.equalsIgnoreCase("quit") || line.equalsIgnoreCase("exit")) {
          sys.exit()
        }
      }
    }
    catch {
      case t: Throwable => {
        t.printStackTrace()
      }
    } finally {
      debug.close()
    }

  }
}
