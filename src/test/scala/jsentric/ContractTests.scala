package jsentric

import argonaut._
import org.scalatest.{Matchers, FunSuite}
import scalaz.{\/-, \/}

class ContractTests extends FunSuite with Matchers {
  import Jsentric._

  test("Contract pattern matching") {
    object Test extends Contract {
      val one = \[String]("one")
      val two = \?[Boolean]("two")
      val three = \![Int]("three", 3)
      val four = \[Long]("four")
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

    (Json("three" := "4") match {
      case Test.three(i) => true
      case _ => false
    }) should equal (false)

    (Json("two" := "String") match {
      case Test.two(None) => true
      case _ => false
    }) should equal (false)

    (Json("two" := jNull) match {
      case Test.two(None) => true
      case _ => false
    }) should be (true)

    (Json("two" := "false") match {
      case Test.two(Some(i)) => true
      case _ => false
    }) should equal (false)

    (Json("three" := "not a number") match {
      case Test.three(i) => i
      case _ => "wrong type"
    }) should equal ("wrong type")

    (jEmptyObject match {
      case Test.two(None) => true
      case _ => false
    }) should be (true)

    val temp = Json("four" := 9223372036854775807L)
    (Json("four" := 9223372036854775807L) match {
      case Test.four(l) => true
      case _ => false
    }) should be (true)

    (Json("four" := -9223372036854775808L) match {
      case Test.four(l) => true
      case _ => false
    }) should be (true)
  }

  test("Optimistic codecs") {
    import OptimisticCodecs._

    object Test extends Contract {
      val one = \[String]("one")
      val two = \?[Boolean]("two")
      val three = \![Int]("three", 3)
    }
    (Json("three" := "4") match {
      case Test.three(i) => i
      case _ => false
    }) should equal (4)

    (Json("two" := "false") match {
      case Test.two(Some(i)) => true
      case _ => false
    }) should equal (true)

    (Json("two" := jNull) match {
      case Test.two(None) => true
      case _ => false
    }) should be (true)

    (Json("two" := "text") match {
      case Test.two(None) => true
      case _ => false
    }) should be (true)
  }

  test("Nested pattern matching") {
    object Test1 extends Contract {
      val nested  = new \\("nested") {
        val one = \[Int]("one")
        val level2 = new \\("level2") {
          val two = \[Int]("two")
        }
      }
    }

    (Json("nested" -> Json("one" := 1)) match {
      case Test1.nested.one(v) => v
    }) should equal (1)
    (Json("nested" -> Json("level2" -> Json("two" := 34))) match {
      case Test1.nested.level2.two(v) => v
    }) should equal (34)
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
    val obj = Json("tuple" -> jArrayElements(jNumberOrString(45), jString("test")), "disrupt" -> jArrayElements(jNumberOrString(4.56), jFalse), "option" := List(1,2,3,4))
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

    val obj1 = Json("exp" -> jArrayElements(jString("one"), jString("two")))
    (obj1 match {
      case Arr.exp(seq) => seq
    }) should equal (Seq("one", "two"))

    val obj2 = Json("exp" -> jArrayElements(jString("one"), jTrue))
    (obj2 match {
      case Arr.exp(seq) => seq
      case _ => "wrong type"
    }) should equal ("wrong type")

    val obj3 = Json("maybe" := List(jNumber(1), jNumber(2)))
    (obj3 match {
      case Arr.maybe(Some(seq)) => seq
    }) should equal (Seq(1, 2))
  }

  test("Optimistic array pattern") {
    import OptimisticCodecs._

    object OptimisticArr extends Contract {
      val exp = \:[String]("exp")
      val maybe = \:?[Int]("maybe")
    }

    (Json("exp" -> jArrayElements(jString("one"), jTrue, jString("three"))) match {
      case OptimisticArr.exp(seq) => seq
      case _ => "wrong type"
    }) should equal (Seq("one", "three"))

    (Json("maybe" := "value") match {
      case OptimisticArr.maybe(None) => true
    }) should be (true)

  }

  test("Dynamic property") {
    object Dyn extends Contract {
      val nest = new \\("nest") {}
    }

    val dynamic1 = Dyn.$dynamic[Int]("int")
    (jEmptyObject match {
      case dynamic1(i) => i
    }) should be (None)

    (Json("int" := 4) match {
      case dynamic1(i) => i
    }) should be (Some(4))

    val dynamic2 = Dyn.nest.$dynamic[Boolean]("bool")

    (Json("nest" -> Json("bool" := true)) match {
      case dynamic2(b) => b
    }) should be (Some(true))
  }

  test("Recursive contract") {
    trait Recursive extends SubContract {
      val level = \[Int]("level")
      lazy val child = new \\?("child") with Recursive
    }
    object Recursive extends Contract with Recursive

    (Json("level" := 0, "child" -> Json("level" := 1, "child" -> Json("level" := 2))) match {
      case Recursive.child.level(l1) && Recursive.child.child.level(l2) => l1 -> l2
    }) should equal (1 -> 2)
  }

  test("implicit codec test") {
    implicit val D = Codec[Double]

    (jNumberOrString(3) match {
      case D(double) => double
    }) should equal (3.0)
  }

  test("nested codec") {
    object T extends Contract {
      val t1 = new \\?("t1") {
        val t2 = \:?[Json]("t2")
      }
    }
  }

  test("Type contracts") {
    object Existence extends ContractType("req") {
      val req = \[String]("req")
      val value = \[Boolean]("value")
    }
    (Json("req" := "test") match {
      case Existence(_) => true
    }) should be (true)

    (Json("value" := "test") match {
      case Existence(_) => true
      case _ => false
    }) should be (false)
  }

  test("Value contract") {
    object MapContract extends ValueContract[Map[String, Boolean]]()

    (Json("value1" := true, "value2" := false) match {
      case MapContract(m) => m.size
    }) should be (2)


  }
}