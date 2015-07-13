package jsentric

import argonaut._
import Argonaut._
import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
class LensTests extends FunSuite with Matchers {
  import Jsentric._

  test("Expected property lens test") {
    object ExpTest extends Contract {
      val value = \[Double]("value")
      val option = \[Option[String]]("option")
      val value2 = \[Double]("value2")
    }
    val withSome = Json("value" := 3.45, "option" := "some", "value2" := -1)

    ExpTest.value.$set(42.5)(withSome) should be (Json("value" := 42.5, "option" := "some", "value2" := -1))
    ExpTest.value.$get(withSome) should be (Some(3.45))
    ExpTest.value.$modify(_ * 2)(withSome) should be (Json("value" := 6.9, "option" := "some", "value2" := -1))
    ExpTest.value.$maybeSet(None)(withSome) should be (withSome)
    ExpTest.value.$maybeSet(Some(-1))(withSome) should be (Json("value" := -1, "option" := "some", "value2" := -1))
    ExpTest.value.$copy(ExpTest.value2)(withSome) should be (Json("value" := 3.45, "option" := "some", "value2" := 3.45))

    ExpTest.option.$set(None)(withSome) should be (Json("value" := 3.45, "option" -> jNull, "value2" := -1))
    ExpTest.option.$set(Some("new"))(withSome) should be (Json("value" := 3.45, "option" := "new", "value2" := -1))
    ExpTest.option.$get(withSome) should be (Some(Some("some")))
  }

  test("Maybe property lens test") {
    object ExpTest extends Contract {
      val value = \?[Boolean]("value")
      val value2 = \[Boolean]("value2")
    }
    val withSome = Json("value" := true, "value2" := false)
    val withNone = Json("value2" := false)

    ExpTest.value.$get(withSome) should be (Some(true))
    ExpTest.value.$get(withNone) should be (None)

    ExpTest.value.$set(false)(withSome) should be (Json("value" := false, "value2" := false))
    ExpTest.value.$set(true)(withSome) should be (withSome)
    ExpTest.value.$set(false)(withNone) should be (Json("value" := false, "value2" := false))
    ExpTest.value.$set(true)(withNone) should be (withSome)

    ExpTest.value.$drop(withSome) should be (withNone)
    ExpTest.value.$drop(withNone) should be (withNone)

    ExpTest.value.$setOrDrop(Some(false))(withSome) should be (Json("value" := false, "value2" := false))
    ExpTest.value.$setOrDrop(None)(withSome) should be (withNone)

    ExpTest.value.$modify(_ => None)(withSome) should be (withNone)
    ExpTest.value.$modify(o => o.map(!_))(withSome) should be (Json("value" := false, "value2" := false))
    ExpTest.value.$modify(o => Some(o.isEmpty))(withNone) should be (withSome)

    ExpTest.value.$copy(ExpTest.value2)(withSome) should be (Json("value" := true, "value2" := true))
    ExpTest.value.$copy(ExpTest.value2)(withNone) should be (withNone)
  }

  test("Default property lens test") {
    object ExpTest extends Contract {
      val value = \![String]("value", "default")
      val value2 = \[String]("value2")
    }
    val withSome = Json("value" := "set", "value2" := "add")
    val withNone = Json("value2" := "add")

    ExpTest.value.$get(withSome) should be ("set")
    ExpTest.value.$get(withNone) should be ("default")

    ExpTest.value.$set("new")(withSome) should be (Json("value" := "new", "value2" := "add"))
    ExpTest.value.$set("new")(withNone) should be (Json("value" := "new", "value2" := "add"))
    ExpTest.value.$set("default")(withSome) should be (Json("value" := "default", "value2" := "add"))
    ExpTest.value.$set("default")(withNone) should be (Json("value" := "default", "value2" := "add"))

    ExpTest.value.$reset(withSome) should be (withNone)
    ExpTest.value.$reset(withNone) should be (withNone)

    ExpTest.value.$modify(_ + "-")(withSome) should be (Json("value" := "set-", "value2" := "add"))
    ExpTest.value.$modify(_ + "-")(withNone) should be (Json("value" := "default-", "value2" := "add"))

    ExpTest.value.$resetOrDrop(None)(withSome) should be (withNone)
    ExpTest.value.$resetOrDrop(Some("set"))(withNone) should be (withSome)
    ExpTest.value.$copy(ExpTest.value2)(withSome) should be (Json("value" := "set", "value2" := "set"))
    ExpTest.value.$copy(ExpTest.value2)(withNone) should be (Json("value2" := "default"))
  }

  test("Expected array lens test") {
    object ExpTest extends Contract {
      val value = \:[Int]("value")
      val value2 = \:[Int]("value2")
    }
    val withSome = Json("value" := List(jNumberOrNull(1),jNumberOrNull(2),jNumberOrNull(3)), "value2" := List(jNumberOrNull(2)))
    val withNone = Json("value2" := List(jNumberOrNull(2)))

    ExpTest.value.$at(2).$get(withSome) should be (Some(3))
    ExpTest.value.$at(3).$get(withSome) should be (None)
    ExpTest.value.$at(3).$get(withNone) should be (None)

    ExpTest.value.$at(2).$set(5)(withSome) should be (Json("value" := List(jNumberOrNull(1),jNumberOrNull(2),jNumberOrNull(5)), "value2" := List(jNumberOrNull(2))))
    ExpTest.value.$at(6).$set(5)(withSome) should be (Json("value" := List(jNumberOrNull(1),jNumberOrNull(2),jNumberOrNull(3), jNull, jNull, jNull, jNumberOrNull(5)), "value2" := List(jNumberOrNull(2))))

    ExpTest.value.$append(4)(withSome) should be (Json("value" := List(jNumberOrNull(1),jNumberOrNull(2),jNumberOrNull(3),jNumberOrNull(4)), "value2" := List(jNumberOrNull(2))))
    ExpTest.value.$append(4)(withNone) should be (Json("value" := List(jNumberOrNull(4)), "value2" -> jArrayElements(jNumberOrNull(2))))

    ExpTest.value.$prepend(0)(withSome) should be (Json("value" := List(jNumberOrNull(0),jNumberOrNull(1),jNumberOrNull(2),jNumberOrNull(3)), "value2" := List(jNumberOrNull(2))))
    ExpTest.value.$prepend(0)(withNone) should be (Json("value" := List(jNumberOrNull(0)), "value2" -> jArrayElements(jNumberOrNull(2))))
  }
}
