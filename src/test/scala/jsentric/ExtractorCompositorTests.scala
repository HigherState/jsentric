package jsentric

import argonaut._
import org.scalatest.{Matchers, FunSuite}
import shapeless._

class ExtractorCompositorTests  extends FunSuite with Matchers {
  import Jsentric._
  import Composite._
  import ops.hlist.Tupler._

  object TestObj extends Contract {
    val int = \[Int]("int")
    val bool = \[Boolean]("bool")
    val string = \[String]("string")

    lazy val composite = TestObj.string @: TestObj.int @: TestObj.bool
  }

  test("Messing about") {
    val json = Json("int" := 1, "bool" := false, "string" := "Test")

    json match {
      case TestObj.composite(s, i, b) =>
        s should equal ("Test")
        i should equal (1)
        b should equal (false)
    }
  }

}