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
    val OnlyNum = """^-?[0-9]*(\.?[0-9]+)$""".r

    def isNumeric(s: String) = s match {
      case OnlyNum(_) => true
      case other => false
    }

    def numericalValue()(implicit m: Model): Double = {
      try {
        if (value.startsWith("=")) {
          QueryTermParser.parse(value).evaluate()
        }
        else if (isNumeric(value)) {
          value.toDouble
        }
        else {
          0
        }
      } catch {
        case t: Throwable =>
          System.err.println(s"$this ${ t.getMessage}")
          0
      }
    }

    def printable()(implicit m: Model, settings: Settings): String = {
      val ret = {
        if (isNumeric(value) || value.startsWith("=")) {
          val ret: Double = numericalValue()
          if (ret == 0)
            ""
          else
            ret.toString
        }
        else {
          value
        }
      }
      ret.substring(0, Math.min(settings.CellWidth, ret.length)).trim
    }
  }

}

class Model {

  import Model._

  def generateDefault() = Range(0, 10).toArray.map(c => Range(0, 8).toArray.map(r => Cell("0")))

  val data: Array[Array[Cell]] = generateDefault()

  def getRow(i: Int) = data(i)
}
