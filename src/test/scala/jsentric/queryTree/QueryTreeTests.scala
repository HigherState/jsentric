package jsentric.queryTree

import jsentric.Path
import org.scalatest.{FunSuite, Matchers}

class QueryTreeTests extends FunSuite with Matchers {

  def op(fields:String*) =
    ?(Path(fields:_*), "", argonaut.Argonaut.jTrue)

  test("Query partition") {

    val and = &(Seq(op("and1"), op("and2")))
    QueryTree.partition(and, Set()) should equal (None -> Some(and))
    QueryTree.partition(and, Set(Path("and1"))) should equal (Some(&(Seq(op("and1")))) -> Some(&(Seq(op("and2")))))
    QueryTree.partition(and, Set(Path("and1"), Path("and2"))) should equal (Some(and) -> None)

    val or = |(Seq(op("or1"), op("or2")))
    QueryTree.partition(or, Set()) should equal (None -> Some(or))
    QueryTree.partition(or, Set(Path("or1"))) should equal (None -> Some(or))
    QueryTree.partition(or, Set(Path("or1"), Path("or2"))) should equal (Some(or) -> None)

    val or2 = |(Seq(op("or3"), op("or4"), op("or5")))
    val andOr = &(Seq(or, or2))
    QueryTree.partition(andOr, Set()) should equal (None -> Some(andOr))
    QueryTree.partition(andOr, Set("or1", "or4")) should equal (None -> Some(andOr))
    QueryTree.partition(andOr, Set("or1", "or2")) should equal (Some(&(Seq(or))) -> Some(&(Seq(or2))))
    QueryTree.partition(andOr, Set("or3", "or4", "or5")) should equal (Some(&(Seq(or2))) -> Some(&(Seq(or))))
    QueryTree.partition(andOr, Set("or1", "or2", "or3", "or4", "or5")) should equal (Some(andOr) -> None)

    val and2 = &(Seq(op("and3"), op("and4")))
    val orAnd = |(Seq(and, and2))
    QueryTree.partition(orAnd, Set()) should equal (None -> Some(orAnd))
    QueryTree.partition(orAnd, Set("and1", "and2")) should equal (None -> Some(orAnd))
    QueryTree.partition(orAnd, Set("and1", "and3")) should equal (
      Some(|(Seq(&(Seq(op("and1"))), &(Seq(op("and3")))))) -> Some(|(Seq(&(Seq(op("and2"))), &(Seq(op("and4"))))))
    )
    // (a & b) | (c & d) ??? Must redress this issue
    QueryTree.partition(orAnd, Set("and1", "and2", "and3")) should equal (
      Some(|(Seq(&(Seq(op("and1"), op("and2"))), &(Seq(op("and3")))))) -> Some(|(Seq(&(Seq(op("and2"))), &(Seq(op("and4"))))))
    )

    val notAnd = !!(and)
    QueryTree.partition(notAnd, Set()) should equal (None -> Some(notAnd))
    QueryTree.partition(notAnd, Set("and2")) should equal (None -> Some(notAnd))
    QueryTree.partition(notAnd, Set("and1", "and2")) should equal (Some(notAnd) -> None)

    val notOr = !!(or)
    QueryTree.partition(notOr, Set()) should equal (None -> Some(notOr))
    QueryTree.partition(notOr, Set("or1")) should equal (Some(!!(|(Seq(op("or1"))))) -> Some(!!(|(Seq(op("or2"))))) )



    ///(a & !(b & !(c & d)))
    val notAndSeq= &(Seq(op("a"), !!(&(Seq(op("b"), !!(&(Seq(op("c"), op("d")))))))))
    QueryTree.partition(notAndSeq, Set("a", "b", "c", "d")) should equal (Some(notAndSeq) -> None)
    QueryTree.partition(notAndSeq, Set("a", "c", "d")) should equal (
      Some(&(List(op("a")))) -> Some(&(List(!!(&(List(op("b"), !!(&(List(op("c"), op("d"))))))))))
    )
//    QueryTree.partition(notAndSeq, Set("b", "c")) should equal (
//      Some(&(Seq(!!(&(Seq(op("b"), !!(&(Seq(op("c")))))))))) -> Some(&(Seq(op("a"), !!(&(Seq(op("b"), !!(&(Seq(op("d")))))))))))
    //QueryTree.partition(notAndSeq, Set("a", "b", "d")) should equal (None -> Some(notAndSeq))
  }
}
