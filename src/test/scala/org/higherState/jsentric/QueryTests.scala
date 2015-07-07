package org.higherState.jsentric

import org.scalatest.{FunSuite, Matchers}

import argonaut._
import Argonaut._

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
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

    query.isMatch(Json("nested" -> Json("field2" := "value"))) should be (true)
    query.isMatch(Json("field" := "value", "nested" -> Json("field2" := "value"))) should be (false)
  }
}
