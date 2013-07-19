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



  //  val m = mock[java.util.List[String]] // a concrete class would be mocked with: mock(new java.util.LinkedList[String])
  //
  //  // stub a method call with a return value
  //  m.get(0) returns "one"
  //
  //  // call the method
  //  m.get(0)
  //
  //  // verify that the call happened, this is an expectation which will throw a FailureException if that is not the case
  //  there was one(m).get(0)
  //
  //  // we can also check that another call did not occur
  //  there was no(m).get(1)


}
