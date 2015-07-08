package org.higherState.jsentric

import org.scalatest.{FunSuite, Matchers}
import argonaut._
import Argonaut._

import scalaz._

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
class ValidatorTests extends FunSuite with Matchers {
  import Jsentric._

  test("Property validation") {

    object StrValid extends Contract {
      val expected = \[String]("expected")
      val maybe = \?[Int]("maybe")
      val default = \![Boolean]("default", false)
      val option = \?[Option[String]]("option")
    }

    StrValid.$validate(jEmptyObject) should be (-\/(NonEmptyList("Value required." -> Path("expected"))))
    val json1 = Json("expected" := "value")
    StrValid.$validate(json1) should be (\/-(json1))
    StrValid.$validate(Json("expected" := 3)) should be (-\/(NonEmptyList("Unexpected type 'JNumber'." -> Path("expected"))))

    val json2 = Json("expected" := "value", "maybe" := 4)
    StrValid.$validate(json2) should be (\/-(json2))
    StrValid.$validate(Json("expected" := "value", "maybe" := 4.6)) should be (-\/(NonEmptyList("Unexpected type 'JNumber'." -> Path("maybe"))))
    StrValid.$validate(Json("expected" := "value", "maybe" -> jNull)) should be (-\/(NonEmptyList("Unexpected type 'JNull$'." -> Path("maybe"))))

    val json3 = Json("expected" := "value", "default" := true)
    StrValid.$validate(json3) should be (\/-(json3))
    StrValid.$validate(Json("expected" := "value", "default" := 4.6)) should be (-\/(NonEmptyList("Unexpected type 'JNumber'." -> Path("default"))))
  }
}
