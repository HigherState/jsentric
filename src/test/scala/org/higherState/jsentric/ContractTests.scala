package org.higherState.jsentric

import argonaut._
import Argonaut._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

import scalaz.{\/-, \/}

/**
 * Created by Jamie Pullar on 07/06/2015.
 */
class ContractTests extends FunSuite with Matchers with ScalaFutures {
  import Patterns._

  test("Contract pattern matching") {
    object Test extends Contract {
      val one = \[String]("one")
      val two = \?[Boolean]("two")
    }

    (Json("one" := "string", "two" := false) match {
      case Test.one(s) && Test.two(Some(v)) => s -> v
    }) should equal ("string" -> false)

    (Json("one" := "string") match {
      case Test.one(s) && Test.two(None) => s
    }) should equal ("string")

    (Json("one" := 123) match {
      case Test.one(s) && Test.two(None) => s
      case _ => "wrong type"
    }) should equal ("wrong type")

    (Json("two" := false) match {
      case Test.one(s) && Test.two(None) => s
      case Test.two(Some(false)) => "two match"
    }) should equal ("two match")
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
      val dis = \[\/[String, (Float, Boolean)]] ("disrupt")
      val option = \[Option[Seq[Int]]]("option")
    }
    val obj = Json("tuple" := List(jNumberOrString(45), jString("test")), "dis" := List(jNumberOrString(4.56), jFalse), "option" := List(1,2,3,4))
    (obj match {
      case Adv.tuple((l,r)) && Adv.dis(\/-((f, b))) =>
        (l,r, f, b)
    }) should equal ((45, "test", 4.56, false))
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
