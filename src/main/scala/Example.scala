import java.io.PrintWriter
import java.util
import jline.console.completer.Completer
import jline.console.ConsoleReader

object Example {

  type Token  =  String
  case class Lnode(t:Token,leafs:Lnode *)


  def main(args: Array[String]) {
    val color = true
    try {
      val reader: ConsoleReader = new ConsoleReader
      reader.setPrompt("prompt> ")

      val c = new Completer {


        implicit def t2ln(t:(String,String)): Lnode = Lnode(t._1,List(Lnode(t._2)):_*)


        val ops = Lnode("foo","foo" -> "bar")


        val list: List[String] = List("bar", "bash", "boolox", "zollox")

        def complete(buffer: String, cursor: Int, candidates: util.List[CharSequence]): Int = {
          if (buffer == "" && cursor == 0) {
            candidates.add("foo")
            candidates.add("zoo")
            cursor
          }
          else if ("foo .*".r.pattern.matcher(buffer).matches()) {
            val prefix: String = buffer.stripPrefix("foo ")
            list.filter( _.startsWith(prefix)).map( _.stripPrefix(prefix) ).foreach(candidates.add)
            buffer.length
          }
          else {
            cursor
          }
        }
      }
      reader.addCompleter(c)
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
    }
  }
}
