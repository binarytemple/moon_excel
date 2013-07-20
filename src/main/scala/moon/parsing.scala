package moon

import org.parboiled.scala._
import moon.Spreadsheet.CellRange
import org.parboiled.scala.RecoveringParseRunner
import org.parboiled.errors.InvalidInputError
import org.parboiled.buffers.InputBuffer
import org.parboiled.support.MatcherPath
import org.parboiled.matchers.{FirstOfStringsMatcher, StringMatcher, CharMatcher}

object QueryTermParser {

  lazy val qtp = new QueryTermParser {
    override val buildParseTree = true
  }

  /**
   * If parsing fails, this class will reveal where, and how.
   * @param start
   * @param end
   * @param buffer
   * @param delta
   * @param options
   */
  case class FormulaParseFail(start: Int, end: Int, buffer: InputBuffer, delta: Int, options: List[String])

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

  case class Formula(op: Op.Value, cr: CellRange) {
    def evaluate()(implicit m: Model) = {
      val input = Spreadsheet.extractRange(cr)(m).map(_.numericalValue())
      op match {
        case Op.Sum => input.sum
        case Op.Count => input.length.toDouble
        case Op.Max => input.max
        case Op.Min => input.min
      }
    }
  }

  /**
   *
   * @param input for example {{{=SUM(A1:A3)}}}
   * @return
   */
  def parse(input: String): Either[FormulaParseFail, Formula] = {
    val QuoteStripper = """^'([^']*)'$""".r

    val runner = RecoveringParseRunner(qtp.FormulaExtractor, 1000l)
    try {
      Right(runner.run(input).result.get)
    }
    catch {
      case t: Throwable =>
        import scala.collection.JavaConversions._

          //On invalid input we try to resolve the logest posible sucessive token
          val collect = runner.inner.getParseErrors.toList.collectFirst {

          case i: InvalidInputError =>
            val fails = i.getFailedMatchers.toList.map{
              mp:MatcherPath =>
                mp.element.matcher match {
                  case cm:CharMatcher => mp.parent.element.matcher match {
                    case fsm:FirstOfStringsMatcher => fsm.strings.toList.map(new String(_))
                    case sm:StringMatcher => List (new String(sm.characters))
                    case _ => List(cm.character.toString)
                  }
                }
            }.flatten
            FormulaParseFail(i.getStartIndex, i.getEndIndex, i.getInputBuffer, i.getIndexDelta,fails)
          }
        Left(collect.get)
    }
    //    println("RESULT:" + run.result)
    //    val parseTreePrintOut = org.parboiled.support.ParseTreeUtils.printNodeTree(run)
    //    println("TREE:" + parseTreePrintOut)
  }
}

class QueryTermParser extends Parser {

  import QueryTermParser.{Op, Formula}

  def Col: Rule1[Int] = rule {
    oneOrMore(anyOf(ColLetters.toArray))
  } ~> (x => x.head - 65)

  def Num: Rule1[Int] = rule {
    oneOrMore(anyOf(Range('0', '9').map(_.toChar).toArray))
  } ~> (_.toInt)

  def Operation: Rule1[Op.Value] = rule {
    //The list of operation matches.. we extract from the Op enumeration
    val ops = Op.values.toList
    var ret = str(ops.head.toString)
    ops.tail.foreach(v => ret = ret | str(v.toString))
    ret

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
