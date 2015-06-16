package org.higherState.jsentric

import argonaut._
import Argonaut._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, FunSuite}
import shapeless._
import shapeless.ops.hlist.Tupler


/**
 * Created by jamie on 11/06/15.
 */
class ExtractorCompositorTests  extends FunSuite with Matchers with ScalaFutures {
  import Patterns._
  import ExtractorCompositor._
  import Evaluator._
  import ops.hlist.Tupler._

  object TestObj extends Contract {
    val int = \[Int]("int")
    val bool = \[Boolean]("bool")

//    val pair = join(
//      ((j:Json) => TestObj.int.unapply(j)) :: ((j:Json) => TestObj.bool.unapply(j)) :: HNil)


  }

  test("Messing about") {
    val json = Json("int" := 1, "bool" := false)
    val hList = ((j:Json) => TestObj.int.unapply(j)) :: ((j:Json) => TestObj.bool.unapply(j)) :: HNil
    println(temp(json, hList))
    println(tupled(1 :: false :: HNil))
    //println(temp(json, hList).map(tupled(_)))

    //println(join(hList).unapply(json))

//    println(TestObj.pair.unapply(json))
//
//    json match {
//      case TestObj.pair(i, b) => println("matched")
//    }
  }

}
