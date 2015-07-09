package jsentric

import argonaut._
import Argonaut._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
class FunctionsTests extends FunSuite with Matchers with Functions {

  test("Applying svalue delta on obj") {
    val obj = Json("one" := 1, "two" := "two")
    applyDelta(obj, jString("value")) should equal (jString("value"))
    applyDelta(obj, jNull) should equal (jNull)
  }

  test("Applying single key delta object value on obj") {
    val obj = Json("one" := 1, "two" := "two")
    applyDelta(obj, jEmptyObject) should equal (obj)
    applyDelta(obj, Json("three" := 3)) should equal (("three" := 3) ->: obj)
    applyDelta(obj, Json("two" := "three")) should equal (Json("one" := 1, "two" := "three"))
    applyDelta(obj, Json("one" -> jNull)) should equal (Json("two" := "two"))
    applyDelta(obj, Json("two" -> jEmptyObject)) should equal (Json("one" := 1))
  }

  test("Applying nested delta object") {
    val obj = Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5)))
    applyDelta(obj, Json("obj" -> jEmptyObject)) should equal (obj)
    applyDelta(obj, Json("obj" -> jNull)) should equal (Json("one" := 1))
    applyDelta(obj, Json("obj" -> Json("two" := true))) should equal (Json("one" := 1, "obj" -> Json("two" := true, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
    applyDelta(obj, Json("obj" -> Json("three" -> jNull))) should equal (Json("one" := 1, "obj" -> Json("two" := false, "four" -> Json("five" := 5))))
    applyDelta(obj, Json("obj" -> Json("two" -> jNull, "three" -> jNull, "four" -> jNull))) should equal (Json("one" := 1))
    applyDelta(obj, Json("obj" -> Json("six" := "vi"))) should equal (Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5), "six" := "vi")))
  }

  test("Get value") {
    val obj = Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5)))
    getValue(obj, Vector.empty) should be (Some(obj))
    getValue(obj, Path("two").segments) should be (None)
    getValue(obj, Path("one").segments) should be (jNumber(1))
    getValue(obj, Path("obj", "two").segments) should be (Some(jFalse))
    getValue(obj, Path("obj", "one").segments) should be (None)
    getValue(obj, Path("obj", "four").segments) should be (Some(Json("five" := 5)))
    getValue(obj, Path("obj", "four", "five").segments) should be (jNumber(5))
    getValue(obj, Path("obj", "three", 3).segments) should be (jNumber(4))
    getValue(obj, Path("obj", "three", 4).segments) should be (None)
  }

  test("Set value") {
    val obj = Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5)))
    setValue(Some(obj), Vector.empty, jString("replace")) should be (jString("replace"))
    setValue(Some(obj), Path("two").segments, jString("set")) should be (Json("one" := 1, "two" := "set", "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("one").segments, jString("replace")) should be (Json("one" := "replace", "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("obj", "two").segments, jString("replace")) should be (Json("one" := 1, "obj" -> Json("two" := "replace", "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("obj", "four", "five").segments, jString("replace")) should be (Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := "replace"))))
    setValue(Some(obj), Path("obj", "three", 3).segments, jNumberOrString(5)) should be (Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,5), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("obj", "three", 5).segments, jNumberOrString(5)) should be (Json("one" := 1, "obj" -> Json("two" := false, "three" := List(jNumberOrString(1),jNumberOrString(2),jNumberOrString(3),jNumberOrString(4),jNull,jNumberOrString(5)), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("two", "three").segments, jString("set")) should be (Json("one" := 1, "two" -> Json("three" := "set"), "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
    setValue(Some(obj), Path("two", 2).segments, jTrue)  should be (Json("one" := 1, "two" := List(jNull, jNull, jTrue), "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5))))
  }

  test("Get difference") {
    val obj = Json("one" := 1, "obj" -> Json("two" := false, "three" := List(1,2,3,4), "four" -> Json("five" := 5)))
    difference(obj, obj) should be (None)
    difference(jEmptyObject, obj) should be (None)
    difference(Json("one" := 1), obj) should be (None)
    difference(Json("one" := 1, "obj" -> Json("two" := false)), obj) should be (None)
    difference(Json("obj" -> Json("four" -> jEmptyObject)), obj) should be (None)
    difference(Json("obj" -> Json("four" -> Json("five" := 5))), obj) should be (None)

    difference(Json("one" := 2), obj) should be (Some(Json("one" := 2)))
    difference(Json("six" := 6), obj) should be (Some(Json("six" := 6)))
    difference(Json("obj" -> Json("four" -> Json("six" := 34.56))), obj) should be (Some(Json("obj" -> Json("four" -> Json("six" := 34.56)))))
    difference(Json("obj" -> Json("two" := true, "three" := List(1,2,3,4))), obj) should be (Some(Json("obj" -> Json("two" := true))))
    difference(Json("obj" -> Json("three" := List(1,2,3,4,5,6))), obj) should be (Some(Json("obj" -> Json("three" := List(1,2,3,4,5,6)))))
  }
}
