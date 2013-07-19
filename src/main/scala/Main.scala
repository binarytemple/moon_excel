import moon.Spreadsheet

object Main {
  def main(args: Array[String]) {


    val s = new Spreadsheet()
    s.assign("A1", "2")
    s.assign("A2", "foobar")
    s.assign("A2", "1")
    s.assign("A3", "1")
    s.assign("A4", "1")
    s.assign("A5", "=MAX(A1:A4)")
    s.assign("B1", "2")
    s.assign("C1", "2")
    s.assign("E7", "SUM COSTS")
    s.assign("F7", "=SUM(A1:A5)")
    s.render.foreach(println)
  }
}
