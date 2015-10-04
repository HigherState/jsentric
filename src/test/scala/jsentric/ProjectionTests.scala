package jsentric

import argonaut.Json
import org.scalatest.{FunSuite, Matchers}

class ProjectionTests extends FunSuite with Matchers {
  import jsentric.Jsentric._

  test("Projection selection") {
    object Query1 extends Contract {
      val field = \?[String]
      val nested = new \\ {
        val field2 = \[String]
        val field3 = \?[Int]
      }
    }
    val projection = Query1.field.$ & Query1.nested.field2.$
    val result = Query1.$create(c => c.field.$set("one") ~ c.nested.field2.$set("two"))
    result.$select(projection) should equal (Json("field" := "one", "nested" -> Json("field2" := "two")))

    val noMatch = Json("value" := 1, "nested" -> Json("value2" := "test"))
    noMatch.$select(projection) should equal (jEmptyObject)
  }
}
