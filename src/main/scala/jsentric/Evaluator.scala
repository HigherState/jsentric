package jsentric

import argonaut.Json
import shapeless.ops.hlist.Tupler
import shapeless.{::, HNil, HList}

trait Evaluator[L <: HList] {
  type Out <: HList

  def apply(json:Json, l:L):Option[Out]
}

object Composite {

  type Aux[T <: HList, O <: HList] = Evaluator[T]{ type Out = O }

  type E[T] = Unapplicable[T]

  implicit val evaluatorHNil:Aux[HNil, HNil] = new Evaluator[HNil] {
    type Out = HNil

    def apply(json:Json, l:HNil) = Some(HNil)
  }

  implicit def evalHCons[H, T <: HList](implicit evalT: Evaluator[T]): Aux[E[H] :: T, H :: evalT.Out] =
    new Evaluator[E[H] :: T] {
      type Out = H :: evalT.Out

      def apply(json:Json, l: E[H] :: T):Option[Out] =
        for {
          h <- l.head.unapply(json)
          t <- evalT(json, l.tail)
        } yield h :: t
    }

  implicit class HListExt[T <: HList](val t:T) extends AnyVal {
    def @:[S](prev:Unapplicable[S]):Unapplicable[S] :: T =
      prev :: t
  }
}

case class JsonList[L <: HList, O <: HList, T](maybes: L, ev: Composite.Aux[L, O], tpl:Tupler.Aux[O, T]) {

  def unapply(json:Json):Option[T] = {
    ev.apply(json, maybes).map{hl =>
      tpl.apply(hl)
    }
  }

  //TODO: add $set method, look at FnFromProduct

  def @:[S, T2](prev:Unapplicable[S])(implicit tpl2:Tupler.Aux[S :: O, T2]) =
    JsonList(prev :: maybes, Composite.evalHCons[S, L](ev), tpl2)
}