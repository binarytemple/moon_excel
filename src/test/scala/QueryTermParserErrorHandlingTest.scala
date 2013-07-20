import moon.QueryTermParser
import moon.QueryTermParser.FormulaParseFail
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class QueryTermParserErrorHandlingTest extends Specification with Mockito {
  "The query term parser" should {
    "parse an incomplete SUM formula" in {
      QueryTermParser.parse( """=SUMz""") must beLeft.like {
        case in@FormulaParseFail(a, b, c, d, e: List[String]) =>
          e must containTheSameElementsAs(List( """("""))
      }
    }
    "parse an empty formula " in {
      QueryTermParser.parse( """=""") must beLeft.like {
        case in@FormulaParseFail(a, b, c, d, e: List[String]) =>
          e must containTheSameElementsAs(List(
            "SUM", "COUNT", "MAX", "MIN"
          ))
      }
    }

  }
}