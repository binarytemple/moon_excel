import moon.Spreadsheet.CellRange
import moon.{Model, Spreadsheet}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class SpreadsheetTest extends Specification with Mockito {

  trait SimpleScope extends Scope {
    val s = new Spreadsheet
    implicit val model = mock[Model]
  }

  trait ComplexScope extends Scope {
    val s = new Spreadsheet
    implicit val model = mock[Model]
    s.assignValue("A1", 1)
    s.m.data(0)(0).numericalValue() must_== 1
    s.assignValue("A2", 2)
    s.m.data(0)(1).numericalValue() must_== 2
    s.assignValue("B1", 3)
    s.m.data(1)(0).numericalValue() must_== 3
    s.assignValue("B2", 4)
    s.m.data(1)(1).numericalValue() must_== 4
  }


  "test the spreadsheet helpers" in {
    Spreadsheet.c2t("A1") must_==(0, 0)
    Spreadsheet.c2t("B5") must_==(1, 4)
    Spreadsheet.c2t("ZZ") must throwA[NumberFormatException]
    Spreadsheet.c2t("Z1") should_==(25, 0)
  }

  "test the spreadsheet setting values " in new SimpleScope {
    s.assignValue("A1", 5)
    s.m.data(0)(0).numericalValue() must_== 5
    s.assignValue("A2", 3)
    s.m.data(0)(1).numericalValue() must_== 3
  }

  "test the spreadsheet extracting ranges for rows" in new SimpleScope {
    s.assignValue("A1", 5)
    s.m.data(0)(0).numericalValue() must_== 5
    s.assignValue("B1", 3)
    s.m.data(1)(0).numericalValue() must_== 3
    s.extractRange(CellRange((0, 0), (1, 0))).map(_.numericalValue()) === List(5, 3)
  }

  "test the spreadsheet extracting ranges for columns" in new ComplexScope {
    s.assignValue("A1", 5)
    s.m.data(0)(0).numericalValue() must_== 5
    s.assignValue("A2", 3)
    s.m.data(0)(1).numericalValue() must_== 3
    s.extractRange(CellRange((0, 0), (0, 1))).map(_.numericalValue()) === List(5, 3)
  }

  "test the spreadsheet extracting ranges for rows and columns" in new ComplexScope {
    s.extractRange(CellRange((0, 0), (1, 1))).map(_.numericalValue()) === List(1, 2, 3, 4)
  }

  "test the spreadsheet extracting ranges for rows and columns, using A1:B2 notation" in new ComplexScope {
    s.extractRange("A1:B2").map(_.numericalValue()) === List(1, 2, 3, 4)
  }

  "print the spreadsheet" in new ComplexScope {
    s.print()(0) must_== "     |A         |B         |C         |D         |E         |F         |G         |H         |"
    s.print()(1) must_== "1    |1.0       |2.0       |          |          |          |          |          |          |          |          |"
    s.print()(2) must_== "2    |3.0       |4.0       |          |          |          |          |          |          |          |          |"
  }


}