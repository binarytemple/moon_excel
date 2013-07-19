package moon

import org.parboiled.scala._
import moon.QueryTermParser.{Formula, Op}
import moon.Spreadsheet.CellRange

object QueryTermParser {

  case class Formula(op: Op.Value, cr: CellRange) {
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
    val run = RecoveringParseRunner(qtp.FormulaExtractor, 1000l).run(input)
//    println("RESULT:" + run.result)
//    val parseTreePrintOut = org.parboiled.support.ParseTreeUtils.printNodeTree(run)
//    println("TREE:" + parseTreePrintOut)
    run.result.get
  }
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

  def FormulaExtractor = rule {
    "=" ~ Operation ~ "(" ~ Pair ~ ")" ~ EOI
  } ~~> ((a: Op.Value, b: Int, c: Int, d: Int, e: Int) => Formula(a, CellRange((b, c - 1), (d, e - 1))))

}