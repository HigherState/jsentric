package jsentric

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

    val json4 = Json("expected" := "value", "option" := "value")
    StrValid.$validate(json4) should be (\/-(json4))
    val json5 = Json("expected" := "value", "option" := jNull)
    StrValid.$validate(json5) should be (\/-(json5))
    StrValid.$validate(Json("expected" := "value", "option" := false)) should be (-\/(NonEmptyList("Unexpected type 'JBool'." -> Path("option"))))
  }

  test("Nested validation") {

    object NestValid extends Contract {
      val value1 = \[String]("value1")
      val nest1 = new \\("nest1") {
        val value2 = \[String]("value2")
        val value3 = \[String]("value3")
      }
      val nest2 = new \\?("nest2") {
        val nest3 = new \\("nest3") {
          val value4 = \[String]("value4")
        }
        val value5 = \[String]("value5")
      }
    }

    val json1 = Json("value1" := "V", "nest1" := Json("value2" := "V", "value3" := "V"))
    NestValid.$validate(json1) should be (\/-(json1))
    val json2 = Json("value1" := "V", "nest1" := Json("value2" := "V", "value3" := "V"), "nest2" -> Json("nest3" -> Json("value4" := "V"), "value5" := "V"))
    NestValid.$validate(json2) should be (\/-(json2))

    NestValid.$validate(Json("value1" := "V", "nest1" := Json("value3" := 3))) should
      be (-\/(NonEmptyList("Unexpected type 'JNumber'." -> "nest1"\"value3", "Value required." -> "nest1"\"value2")))

    NestValid.$validate(Json("value1" := "V", "nest2" := jEmptyObject)) should
      be (-\/(NonEmptyList("Value required." ->"nest1", "Value required." -> "nest2"\"nest3", "Value required." -> "nest2"\"value5")))
  }
}
