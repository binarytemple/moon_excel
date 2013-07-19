import moon.QueryTermParser
import moon.QueryTermParser.{Formula, Op}
import moon.Spreadsheet.CellRange
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class QueryTermParserTest extends Specification with Mockito {
  "test the parser " in {
    QueryTermParser.parse( """=SUM(A1:B3)""") must_== Formula(Op.Sum, CellRange((0, 0), (1, 2)))
    QueryTermParser.parse( """=MAX(A1:B3)""") must_== Formula(Op.Max, CellRange((0, 0), (1, 2)))
    QueryTermParser.parse( """=MIN(A1:B3)""") must_== Formula(Op.Min, CellRange((0, 0), (1, 2)))
    QueryTermParser.parse( """=COUNT(A1:B3)""") must_== Formula(Op.Count, CellRange((0, 0), (1, 2)))
  }
}