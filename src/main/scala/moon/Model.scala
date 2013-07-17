package moon


/**
 * Model of a spreadsheet containing 8 columns and ten rows.
 * The columns are labeled A-H, and the rows as 1-10
 */
object Model {

  val Cols = 8
  val ColLetters =  Range(65,65 + Cols).map(_.toChar)
  val Rows = 10

  class Cell(s: String, m: Model) {

    def eval(): Option[Double] =

      try {
        if (s.startsWith("=")) {
          QueryTermParser.parse(s).evaluate(m)
        }
        else {
          Some(s.toDouble)
        }
      } catch {
        case t: Throwable =>
          println(this + t.getMessage)
          None
      }
  }

  object Cell {
    def apply(s: String, m: Model) = {
      new Cell(s, m)
    }
  }
}

class Model {

  import Model._

  val data: Array[Array[Cell]] = Range(0, 8).toArray.map(c => Range(0, 10).toArray.map(r => Cell("undef-" +(c, r), this)))
}
