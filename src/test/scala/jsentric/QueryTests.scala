package jsentric

import org.scalatest.{FunSuite, Matchers}

import argonaut._
import Argonaut._

class QueryTests extends FunSuite with Matchers {
  import Jsentric._

  test("Existance/nonexistance of field") {
    object Query1 extends Contract {
      val field = \?[String]("field")
      val nested = new \\("nested") {
        val field2 = \?[String]("field2")
      }
    }
    val query = Query1.field.$exists(true)

    query.isMatch(Json("field" := "value")) should be (true)
    query.isMatch(Json("field2" := "value")) should be (false)

    val query2 = Query1.field.$exists(false) && Query1.nested.field2.$exists(true)

    query2.isMatch(Json("nested" -> Json("field2" := "value"))) should be (true)
    query2.isMatch(Json("field" := "value", "nested" -> Json("field2" := "value"))) should be (false)
  }

  test("Equality")   {
    object Query2 extends Contract {
      val field = \?[String]("field")
      val nested = new \\("nested") {
        val field2 = \[Int]("field2")
      }
    }
    val query1 = Query2.field.$eq("TEST") || Query2.nested.field2.$eq(45)
    query1.isMatch(Json("field" := "TEST")) should be (true)
    query1.isMatch(jEmptyObject) should be (false)
    query1.isMatch(Json("field" := "TEST2")) should be (false)
    query1.isMatch(Json("nested" -> Json("field2" := 45))) should be (true)
    query1.isMatch(Json("field" := "TEST", "nested" -> Json("field2" := 45))) should be (true)

    val query2 = Query2.field.$ne("TEST") || Query2.nested(n => n.field2.$gte(45) && n.field2.$lt(52))
    query2.isMatch(Json("field" := "TEST")) should be (false)
    query2.isMatch(Json("field" := "TEST", "nested" -> Json("field2" := 44))) should be (false)
    query2.isMatch(Json("field" := "TEST", "nested" -> Json("field2" := 52))) should be (false)
    query2.isMatch(Json("field" := "TEST2", "nested" -> Json("field2" := 45))) should be (true)
    query2.isMatch(Json("nested" -> Json("field2" := 44))) should be (true)

    val query3 = Query2(q => q.field.$in("TEST", "TEST2") && q.nested.field2.$nin(4,5,6))
    query3.isMatch(Json("field" := "TEST")) should be (true)
    query3.isMatch(Json("field" := "TEST", "nested" -> Json("field2" := 3))) should be (true)
    query3.isMatch(Json("field" := "TEST", "nested" -> Json("field2" := 4))) should be (false)
    query3.isMatch(Json("field" := "TEST3")) should be (false)
    query3.isMatch(Json("field" := "TEST3", "nested" -> Json("field2" := 3))) should be (false)
    query3.isMatch(Json("nested" -> Json("field2" := 3))) should be (false)

    //TODO not a generalised solution
    val query4 = Query2.field.$like("value")
    query4.isMatch(Json("field" := "Value")) should be (true)
    query4.isMatch(jEmptyObject) should be (false)
    query4.isMatch(Json("field" := "Values")) should be (false)

    val query5 = Query2.field.$like("%lue")
    query5.isMatch(Json("field" := "ValuE")) should be (true)
    query5.isMatch(jEmptyObject) should be (false)
    query5.isMatch(Json("field" := "Values")) should be (false)

    val query6 = Query2.field.$regex("vaLUe", "i")
    query6.isMatch(Json("field" := "Value")) should be (true)
    query6.isMatch(jEmptyObject) should be (false)
    query6.isMatch(Json("field" := "Values")) should be (false)

  }

  test("element match") {
    object Query3 extends Contract {
      val doubles = \:[Long]("doubles")
      val nested = new \\("nested") {
        val strings = \:?[String]("strings")
      }
    }

    val query1 = Query3.doubles.$elemMatch(_.$gt(4))
    println(query1.pretty(PrettyParams.nospace))
    query1.isMatch(Json("doubles" := (3.asJson -->>: 5.asJson -->>: jEmptyArray))) should be (true)
    query1.isMatch(Json("doubles" := (2.asJson -->>: 4.asJson -->>: jEmptyArray))) should be (false)
    query1.isMatch(Json("doubles" := jEmptyArray)) should be (false)
  }

  test("boolean operators") {
    object Query4 extends Contract {
      val value = \[Double]("value")
    }

    val query1 = Query4.value.$gt(0) || Query4.value.$lt(-10)
    query1.isMatch(Json("value" := 2)) should be (true)
    query1.isMatch(Json("value" := -3)) should be (false)
    query1.isMatch(Json("value" := -15)) should be (true)

    val query2 = Jsentric.not(query1)
    query2.isMatch(Json("value" := 2)) should be (false)
    query2.isMatch(Json("value" := -3)) should be (true)
    query2.isMatch(Json("value" := -15)) should be (false)

    val query3 = Query4.value.$gte(0) && Query4.value.$lt(50)
    query3.isMatch(Json("value" := 12)) should be (true)
    query3.isMatch(Json("value" := -3)) should be (false)
    query3.isMatch(Json("value" := 50)) should be (false)
  }
}
