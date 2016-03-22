package jsentric

import org.scalatest.{FunSuite, Matchers}
import argonaut._
import Argonaut._

import scalaz._

class ValidatorTests extends FunSuite with Matchers {
  import Jsentric._

  private def toSet(dis: \/[NonEmptyList[(String, Path)], Json]): \/[Set[(String, Path)], Json] =
    dis.leftMap(_.list.toList.toSet)

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

    val json3 = Json("expected" := "value", "default" := true)
    StrValid.$validate(json3) should be (\/-(json3))
    StrValid.$validate(Json("expected" := "value", "default" := 4.6)) should be (-\/(NonEmptyList("Unexpected type 'JNumber'." -> Path("default"))))

    val json4 = Json("expected" := "value", "option" := "value")
    StrValid.$validate(json4) should be (\/-(json4))
    val json5 = Json("expected" := "value", "maybe" := jNull, "option" := jNull)
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

    toSet(NestValid.$validate(Json("value1" := "V", "nest1" := Json("value3" := 3)))) should
      be (-\/(Set("Value required." -> "nest1"\"value2", "Unexpected type 'JNumber'." -> "nest1"\"value3")))

    toSet(NestValid.$validate(Json("value1" := "V", "nest2" := jEmptyObject))) should
      be (-\/(Set("Value required." -> Path("nest1"), "Value required." -> "nest2"\"nest3", "Value required." -> "nest2"\"value5")))
  }

  test("Internal and reserved validators") {

    object IRValid extends Contract {
      val reserve = \?[String]("reserve", reserved)
      val defaultReserve = \![Int]("defaultReserve", 0, reserved)

      val intern = \?[Boolean]("intern", internal)
      val internReserve = \![Boolean]("internReserve", false, internal)
    }

    IRValid.$validate(jEmptyObject) should be (\/-(jEmptyObject))
    IRValid.$validate(Json("reserve" := "check")) should be  (-\/(NonEmptyList("Value is reserved and cannot be provided." -> Path("reserve"))))
  }

  test("Custom validator") {

    object Custom extends Contract {
      val values = \:?[(String, Int)]("values" , forall(custom((t:(String, Int)) => t._2 > 0, "Int must be greater than zero")))
      val compare = \?[Int]("compare", customCompare[Int]((d,c) => math.abs(d - c) < 3, "Cannot change by more than three"))
    }

    val success = Json("values" -> jArrayElements(jArrayElements("one".asJson, 1.asJson)))
    val failure = Json("values" -> jArrayElements(jArrayElements("one".asJson, 1.asJson), jArrayElements("negative".asJson, (-1).asJson)))

    Custom.$validate(success) should be (\/-(success))
    Custom.$validate(failure) should be (-\/(NonEmptyList("Int must be greater than zero" -> "values"\1)))

    val compare = Json("compare" := 5)
    Custom.$validate(compare, Json("compare" := 7)) should be (\/-(compare))
    Custom.$validate(compare, Json("compare" := 0)) should be (-\/(NonEmptyList("Cannot change by more than three" -> Path("compare"))))
  }

  test("Delta validation") {

    object Delta extends Contract {
      val expected = \[String]("expected")
      val immute = \[Boolean]("immute", immutable)
      val maybe = \?[Int]("maybe")
      val reserve = \?[Float]("reserve", reserved)
    }

    val replaceExpected = Json("expected" := "replace")
    val replaceImmute = Json("immute" := true)
    val replaceMaybe = Json("maybe" := 123)
    val clearMaybe = Json("maybe" -> jNull)
    val replaceReserve = Json("reserve" := 12.3)
    Delta.$validate(replaceExpected, Json("expected" := "original", "immute" := false)) should be (\/-(replaceExpected))
    Delta.$validate(replaceImmute, Json("expected" := "original", "immute" := false)) should be (-\/(NonEmptyList("Value is immutable and cannot be changed." -> Path("immute"))))
    Delta.$validate(replaceImmute, Json("expected" := "original", "immute" := true)) should be (\/-(replaceImmute))

    Delta.$validate(replaceMaybe, Json("expected" := "original", "immute" := false)) should be (\/-(replaceMaybe))
    Delta.$validate(replaceMaybe, Json("expected" := "original", "immute" := false, "maybe" := 1)) should be (\/-(replaceMaybe))
    Delta.$validate(clearMaybe, Json("expected" := "original", "immute" := false)) should be (\/-(clearMaybe))
    Delta.$validate(clearMaybe, Json("expected" := "original", "immute" := false, "maybe" := 1)) should be (\/-(clearMaybe))
    Delta.$validate(replaceReserve, Json("expected" := "original", "immute" := false, "maybe" := 1)) should be (-\/(NonEmptyList("Value is reserved and cannot be provided." -> Path("reserve"))))
  }
}
