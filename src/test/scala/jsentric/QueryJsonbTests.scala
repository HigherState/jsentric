package jsentric

import org.scalatest.{FunSuite, Matchers}
import scalaz.\/-


class QueryJsonbTests extends FunSuite with Matchers {
  import Jsentric._

  test("simple operations") {
    object SimpleObject extends Contract {
      val int = \?[Int]("int")
      val string = \[String]("string")
      val array = \:[Int]("array")
      val bool = \[Boolean]("bool")
      val nested = new \\("nested") {
        val double = \[Double]("double")
        val string = \?[String]("string")
      }
    }
    val exists = SimpleObject.int.$exists(true)
    val notExists = SimpleObject.int.$exists(false)
    val equals = SimpleObject.string.$eq("value")
    val nequals = SimpleObject.bool.$ne(false)
    val compare = SimpleObject.int.$gte(45)
    val contains = SimpleObject.array.$elemMatch(_.$eq(4))
    val composite = SimpleObject.int.$gte(45) && SimpleObject.int.$lt(500)
    val in = SimpleObject.int.$in(4,7,9,10)
    val nin = SimpleObject.string.$nin("value", "value2")
    val nt = Jsentric.not(SimpleObject.string.$eq("value"))
    val nested = SimpleObject.nested.double.$lte(34)
    val like = SimpleObject.nested.string.$like("%tr%")
    val regex = SimpleObject.nested.string.$regex(".ES.*", "i")



    QueryJsonb("content", exists) should be (\/-("content ?? 'int'"))
    QueryJsonb("content", notExists) should be (\/-("NOT content ?? 'int'"))
    QueryJsonb("content", equals) should be (\/-("content @> '{\"string\":\"value\"}'::jsonb"))
    QueryJsonb("content", nequals) should be (\/-("NOT content @> '{\"bool\":false}'::jsonb"))
    QueryJsonb("content", compare) should be (\/-("(jsonb_typeof(content #> '{int}') = 'number' AND (content #>> '{int}') :: NUMERIC > 45)"))
    QueryJsonb("content", contains) should be (\/-("content @> '{\"array\":[4]}'::jsonb"))
    QueryJsonb("content", composite) should be (\/-("((jsonb_typeof(content #> '{int}') = 'number' AND (content #>> '{int}') :: NUMERIC > 45) AND (jsonb_typeof(content #> '{int}') = 'number' AND (content #>> '{int}') :: NUMERIC < 500))"))
    QueryJsonb("content", in) should be (\/-("content #> '{int}' <@ '[4,7,9,10]'::jsonb"))
    QueryJsonb("content", nin) should be (\/-("NOT content #> '{string}' <@ '[\"value\",\"value2\"]'::jsonb"))
    QueryJsonb("content", nt) should be (\/-("NOT (content @> '{\"string\":\"value\"}'::jsonb)"))
    QueryJsonb("content", nested) should be (\/-("(jsonb_typeof(content #> '{nested,double}') = 'number' AND (content #>> '{nested,double}') :: NUMERIC < 34)"))
    QueryJsonb("content", like) should be (\/-("(content #>> '{nested,string}') ILIKE '%tr%'"))
    QueryJsonb("content", regex) should be (\/-("(content #>> '{nested,string}') ~ '(?i).ES.*'"))
  }


}