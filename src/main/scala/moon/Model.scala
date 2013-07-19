package moon


/**
 * Model of a spreadsheet containing 8 columns and ten rows.
 * The columns are labeled A-H, and the rows as 1-10
 */
object Model {


  /**
   * A class to represent a spreadsheet cell.
   * It will require methods that allow it to be assigned a numerical value,
   * to be emptied, and to return both a string representation and a numerical representation. Write test code that tests
   * the behaviour of the cell class.
   */
  case class Cell(value: String) {

    def numericalValue()(implicit m: Model): Double =
      try {
        if (value.startsWith("=")) {
          QueryTermParser.parse(value).evaluate(m)
        }
        else {
          value.toDouble
        }
      } catch {
        case t: Throwable =>
          System.err.println(s"$this ${ t.getMessage}")
          0
      }

    def printable()(implicit m: Model) = {
      val value1: Double = numericalValue()
      if (value1 == 0) {
        ""
      }
      else value1.toString
    }
  }
}

class Model {

  import Model._

  def generateDefault() = Range(0, 8).toArray.map(c => Range(0, 10).toArray.map(r => Cell("0")))

  val data: Array[Array[Cell]] = generateDefault()
}
