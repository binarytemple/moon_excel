import moon.{Settings, Spreadsheet, Model}
import moon.Model.Cell
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class CellTest extends Specification with Mockito {

  trait CellScope extends Scope {
    implicit val s = new Settings
    implicit val m = new Model {
      override def generateDefault(): Array[Array[Cell]] = Array(
        Array(
          Cell("50"), Cell("5")
        ),
        Array(
          Cell("9"), Cell("8")
        )
      )
    }
  }

  "An individual cell" should {
    "accept values, converting them to numbers" in new CellScope {
      val c = Cell("1.4")
      c.value must_== "1.4"
      c.numericalValue() must_== 1.4
    }

    "Evaluate functions" in new CellScope {
      //A quick sanity check
      Spreadsheet.extractRange("A1:B2").map(_.numericalValue) must containAllOf(Seq(50, 5, 9, 8))
      Cell("=MIN(A1:B2)").numericalValue() must_== 5
      Cell("=MIN(A1:B2)").printable() must_== "5.0"
      Cell("=MAX(A1:B2)").numericalValue() must_== 50
      Cell("=COUNT(A1:B2)").numericalValue() must_== 4
      Cell("=SUM(A1:B2)").numericalValue() must_== 72
    }
  }
}