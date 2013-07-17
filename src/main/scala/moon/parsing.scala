package moon

import org.parboiled.scala._
import moon.QueryTermParser.Op

object QueryTermParser {

  def main(args: Array[String]) {
    QueryTermParser.parse( """=SUM(A1:B3)""").toString()
    QueryTermParser.parse( """=MAX(A1:B3)""").toString()
    QueryTermParser.parse( """=MIN(A1:B3)""").toString()
    QueryTermParser.parse( """=COUNT(A1:B3)""").toString()
  }


  class Formula(v: Any) {
    def evaluate(m: Model) = ???
  }

  object Op extends Enumeration {

    type Op = Value

    val Sum = Value("SUM")
    val Count = Value("COUNT")
    val Max = Value("MAX")
    val Min = Value("MIN")

    def fromString(s: String) = {
      s match {
        case "SUM" => Op.Sum
        case "COUNT" => Op.Count
        case "MAX" => Op.Max
        case "MIN" => Op.Min
        case other => throw new UnsupportedOperationException(s"I don't understand $other")
      }
    }
  }

  /**
   *
   * @param input for example {{{=SUM(A1:A3)}}}
   * @return
   */
  def parse(input: String): Formula = {
    val qtp = new QueryTermParser {
      override val buildParseTree = true
    }
    val run = RecoveringParseRunner(qtp.Formula, 1000l).run(input)
    println("RESULT:" + run.result)


    val parseTreePrintOut = org.parboiled.support.ParseTreeUtils.printNodeTree(run)
    println("TREE:" + parseTreePrintOut)

    //    run.result match {
    //      case Some(astRoot) => astRoot.toRight(Left(new ParsingException(s"Invalid JSON source: $input")))
    //        .asInstanceOf[Formula]
    //      case None => throw new ParsingException("Invalid JSON source:\n" + ErrorUtils.printParseErrors(run))
    //    }
    new Formula()
  }

  //  def toString(in: QRES) = {
  //    in match {
  //      case Left(a) => throw new UnsupportedOperationException("Can't stringify", a)
  //      case Right(b) => {
  //        val q = b._1.toList.map(x => x._1 + ":" + x._2).mkString(" ")
  //        b._2 match {
  //          case Some(x) => q + "  " + x
  //          case None => q
  //        }
  //      }
  //    }
  //  }
}

class QueryTermParser extends Parser {

  import Model._

  def Col: Rule1[Int] = rule {
    oneOrMore(anyOf(ColLetters.toArray))
  } ~> (x => x.head - 65)

  def Num: Rule1[Int] = rule {
    oneOrMore(anyOf(Range('0', '9').map(_.toChar).toArray))
  } ~> (_.toInt)

  def Operation: Rule1[Op.Value] = rule {
    oneOrMore(anyOf(Range('A', 'Z').map(_.toChar).toArray))
  } ~> Op.fromString

  def Term: Rule2[Int, Int] = rule {
    Col ~ Num
  }

  def Pair: Rule4[Int, Int, Int, Int] = rule {
    Term ~ ":" ~ Term
  }

  def Formula = rule {
    "=" ~ Operation ~ "(" ~ Pair ~ ")" ~ EOI
  } ~~> ( (a:Op.Value,b:Int,c:Int,d:Int,e:Int) => s" $a $b $c $d $e")

}