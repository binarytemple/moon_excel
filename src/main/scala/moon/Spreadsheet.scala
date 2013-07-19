package moon

import moon.Model.Cell

object Spreadsheet {

  /**
   * Convert a CellId to an offset pair. The offsets are zero based.
   * @param id
   */
  def c2t(id: CellId): (Int, Int) = {
    id.toCharArray.toList match {
      case c :: r :: Nil => (c.toUpper - 65, r.toString.toInt - 1)
      case other => throw new Exception(s"Couldn't get offset from $id, it split to '$other'")
    }
  }

  def s2cr(s:String):CellRange = {
    s.split(':').toList match {
      case start :: end :: Nil => CellRange(c2t(start),c2t(end))
      case other => throw new Exception(s"Couldn't extract range from $s, it split to '$other'")
    }

  }


  case class CellRange(start: (Int, Int), end: (Int, Int))
}

/**
 * Write a class to represent the spreadsheet, using the class you have written that represents cells.

 * The spreadsheet class will also need a method that allows it to be printed out in a format similar to the example shown above.
 * Write test code that tests the behaviour of the spreadsheet class.
 * @param m The Spreadsheet data model,
 */
class Spreadsheet(var m: Model = new Model) {

  import Spreadsheet._

  /*The spreadsheet class will need methods that allow cells identified by their names (e.g. A1 or D4) to be assigned numerical values or emptied,
   as well as queried for the values they contain.*/
  def assignValue(id: CellId, value: Double): Unit = {
    val offset = c2t(id)
    try {
      this.m.data(offset._1).update(offset._2, Cell(value.toString))
    }
    catch {
      case t: Throwable => System.err.println(t)
    }
  }


  def extractRange(s:String): List[Cell] = {
    extractRange(s2cr(s))
  }

  def extractRange(c:CellRange): List[Cell] = {

    def colfilter(x: (Array[Model.Cell], Int)): Boolean = {
      x._2 >= c.start._1 && x._2 <= c.end._1
    }

    def rowfilter(x: (Model.Cell, Int)): Boolean = {
      x._2 >= c.start._2 && x._2 <= c.end._2
    }
    this.m.data.toList.zipWithIndex.filter {
      colfilter
    }.map(_._1).map(_.toList.zipWithIndex.filter(rowfilter).map(_._1)).flatten

  }


}