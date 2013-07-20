import moon.QueryTermParser
import moon.QueryTermParser.{Formula, Op}
import moon.Spreadsheet.CellRange
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class QueryTermParserTest extends Specification with Mockito {
  "The query term parser" should {
    "parse a SUM formula" in {
      QueryTermParser.parse( """=SUM(A1:B3)""") must_== Right(Formula(Op.Sum, CellRange((0, 0), (1, 2)))       )
    }
    "parse a MAX formula" in {
      QueryTermParser.parse( """=MAX(A1:B3)""") must_== Right(Formula(Op.Max, CellRange((0, 0), (1, 2)))      )
    }
    "parse a MIN formula" in {
      QueryTermParser.parse( """=MIN(A1:B3)""") must_== Right(Formula(Op.Min, CellRange((0, 0), (1, 2)))     )
    }
    "parse a COUNT formula" in {
      QueryTermParser.parse( """=COUNT(A1:B3)""") must_== Right(Formula(Op.Count, CellRange((0, 0), (1, 2))))
    }
  }
}