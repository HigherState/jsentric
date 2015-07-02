package org.higherState.jsentric

import argonaut._
import Argonaut._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

import scalaz.{\/-, \/}

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
class ContractTests extends FunSuite with Matchers {
  import Patterns._

  test("Contract pattern matching") {
    object Test extends Contract {
      val one = \[String]("one")
      val two = \?[Boolean]("two")
      val three = \![Int]("three", 3)
    }

    (Json("one" := "string", "two" := false) match {
      case Test.one(s) && Test.two(Some(v)) => s -> v
    }) should equal ("string" -> false)

    (Json("one" := "string") match {
      case Test.one(s) && Test.two(None) && Test.three(int) => s -> int
    }) should equal ("string" -> 3)

    (Json("one" := 123) match {
      case Test.one(s) && Test.two(None) => s
      case _ => "wrong type"
    }) should equal ("wrong type")

    (Json("two" := false) match {
      case Test.one(s) && Test.two(None) => s
      case Test.two(Some(false)) => "two match"
    }) should equal ("two match")

    (Json("three" := 4) match {
      case Test.three(i) => i
    }) should equal (4)

    (Json("three" := "not a number") match {
      case Test.three(i) => i
      case _ => "wrong type"
    }) should equal ("wrong type")
  }

  test("Nested pattern matching") {
    object Test1 extends Contract {
      val nested  = new \\("nested") {
        val one = \[Int]("one")
      }
    }

    (Json("nested" -> Json("one" := 1)) match {
      case Test1.nested.one(v) => v
    }) should equal (1)
  }

  test("Default value contract") {
    object Test extends Contract {
      val one = \![Boolean]("one", false)
    }

    (Json("one" := true) match {
      case Test.one(b) => b
    }) should equal (true)

    (Json() match {
      case Test.one(b) => b
    }) should equal (false)
  }

  test("Advanced patterns") {
    object Adv extends Contract {
      val tuple = \[(Int, String)]("tuple")
      val disrupt = \[\/[String, (Float, Boolean)]] ("disrupt")
      val option = \[Option[Seq[Int]]]("option")
    }
    val obj = Json("tuple" := List(jNumberOrString(45), jString("test")), "disrupt" := List(jNumberOrString(4.56), jFalse), "option" := List(1,2,3,4))
    (obj match {
      case Adv.tuple((l,r)) && Adv.disrupt(\/-((f, b))) =>
        (l,r, f, b)
      case Adv.tuple((l,r)) =>
        (l,r)
    }) should equal ((45, "test", 4.56F, false))
  }

  test("Array pattern matching") {
    object Arr extends Contract {
      val exp = \:[String]("exp")
      val maybe = \:?[Int]("maybe")
    }

    val obj1 = Json("exp" := List(jString("one"), jString("two")))
    (obj1 match {
      case Arr.exp(seq) => seq
    }) should equal (List("one", "two"))

    val obj2 = Json("exp" := List(jString("one"), jTrue))
    (obj2 match {
      case Arr.exp(seq) => seq
      case _ => "wrong type"
    }) should equal ("wrong type")

    val obj3 = Json("maybe" := List(jNumber(1), jNumber(2)))
    (obj3 match {
      case Arr.maybe(seq) => seq
    }) should equal (Seq(1, 2))
  }

//  test("Recursive contract") {
//    trait Recursive extends SubContract {
//      val level = \[Int]("level")
//      val child = new \\?("child") with Recursive
//    }
//    object Recursive extends Contract with Recursive
//
//    (Json("level" := 0, "child" -> Json("level" := 1, "child" -> Json("level" := 2))) match {
//      case Recursive.child.level(l1)  && Recursive.child.child.level(l2) => l1 -> l2
//    }) should equal (1 -> 2)
//  }
}
