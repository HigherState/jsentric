package org.higherState.jsentric

import argonaut._
import Argonaut._
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

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
}
