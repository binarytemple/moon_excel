import moon.Model
import moon.Model.Cell
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class CellTest extends Specification with Mockito {
 "test the cell (task 1)" in {
    implicit val m = new Model {
      override def generateDefault(): Array[Array[Cell]] = Array(Array.empty[Cell])
    }
    val c = Cell("1.4")
    c.toString must_== "1.4"
    c.numericalValue() must_== 1.4
  }
}