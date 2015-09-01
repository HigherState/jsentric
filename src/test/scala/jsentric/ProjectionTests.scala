package jsentric

import org.scalatest.{FunSuite, Matchers}


class ProjectionTests extends FunSuite with Matchers {
  import jsentric.Jsentric._
  import Projection._

  test("Projection selection") {
    object Query1 extends Contract {
      val field = \?[String]("field")
      val nested = new \\("nested") {
        val field2 = \[String]("field2")
        val field3 = \?[Int]("field3")
      }
    }
    //val projection = Query1.field.$p(1) & Query1.nested.field2.$p(1)
    //println(projection)
  }

}

object Testing {
  import jsentric.Jsentric._
  import Projection._

  object Query1 extends Contract {
    val field = \?[String]("field")
    val nested = new \\("nested") {
      val field2 = \[String]("field2")
      val field3 = \?[Int]("field3")
    }
  }

  def main(args:Array[String]) {
    val projection = new ProjectionQuery(Query1.field).$
    val result = Query1.$create(c => c.field.$set("one") ~ c.nested.field2.$set("two"))
    println(result.$select(projection))
  }
}
