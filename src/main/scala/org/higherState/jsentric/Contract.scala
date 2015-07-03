package org.higherState.jsentric

import argonaut._
import Argonaut._
import shapeless._
import shapeless.ops.hlist._

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def absolutePath:Path

  def $dynamic[T](path:String)(implicit pattern:Pattern[T]) =
    new Maybe[T](path, absolutePath \ path, EmptyValidator)
}

abstract class Contract(implicit pattern:Pattern[JsonObject]) extends BaseContract {
  implicit protected def absolutePath:Path = Path.empty

  def unapply(j:Json):Option[JsonObject] =
    pattern.unapply(j)

}

abstract class ContractType(val key:String, val matcher:Matcher = DefaultMatcher)(implicit pattern:Pattern[JsonObject]) extends BaseContract {
  implicit protected def absolutePath: Path = Path.empty
  def unapply(j:Json):Option[JsonObject] =
    pattern.unapply(j).filter(_(key).exists(matcher.isMatch))

}

abstract class ValueContract[T](val validator: Validator[T] = EmptyValidator)(implicit _pattern:Pattern[T]) extends BaseContract with Property[T] {
  implicit val absolutePath: Path = Path.empty
  val relativePath: Path = Path.empty
  def pattern: Pattern[T] = _pattern
  def unapply(j:Json):Option[T] =
    pattern.unapply(j)
}

trait Matcher  {
  def isMatch(j:Json):Boolean
  def default:Json
}

object DefaultMatcher extends Matcher {
  def isMatch(j:Json):Boolean = true
  def default:Json = jNull
}

object JsonMatchers {
  implicit def valueMatcher[T](value:T)(implicit pattern:Pattern[T]) = new Matcher {
    val default: Json = pattern.apply(value)
    def isMatch(j: Json): Boolean = j == default
  }
}

trait SubContract {
  implicit protected def absolutePath:Path
}

trait Unapplicable[T] {
  def unapply(j:Json):Option[T]

  def @:[S](prev:Unapplicable[S]):Unapplicable[S] :: Unapplicable[T] :: HNil =
    prev :: this.asInstanceOf[Unapplicable[T]] :: HNil
}

trait Property[T <: Any] extends SelfApply {
  def pattern:Pattern[T]
  def absolutePath:Path
  def relativePath:Path
  def validator:Validator[_]
}

class Expected[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[T])(implicit val pattern:Pattern[T])
  extends Property[T] with Functions with Unapplicable[T] {

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).flatMap(pattern.unapply)
}

class Maybe[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[Option[T]])(implicit val pattern:Pattern[T])
  extends Property[T] with Functions with Unapplicable[Option[T]] {

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, absolutePath.segments).fold[Option[Option[T]]](Some(None)) { v =>
      pattern.unapply(v).map(Some(_)) //always returns Some()
    }

}

class Default[T](val relativePath:Path, implicit val absolutePath:Path, val default:T, val validator:Validator[T])(implicit val pattern:Pattern[T])
  extends Property[T] with Functions with Unapplicable[T] {

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).fold[Option[T]](Some(default)) { v =>
      pattern.unapply(v)
    }
}

class EmptyProperty[T](implicit p:Pattern[T]) extends Property[T] {
  val pattern: Pattern[T] = p
  def absolutePath: Path = Path.empty
  def relativePath: Path = Path.empty
  def validator: Validator[T] = ???
}

object \ {
  def apply[T](path:Path, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) =
    new Expected[T](path, parentPath ++ path, validator)(pattern)
}

object \? {
  def apply[T](path:Path, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) =
    new Maybe[T](path, parentPath ++ path, validator)(pattern)
}

object \! {
  def apply[T](path:Path, default:T, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) =
    new Default[T](path, parentPath ++ path, default, validator)(pattern)
}

abstract class \\(path:Path, validator:Validator[JsonObject] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JsonObject])
  extends Expected[JsonObject](path, parentPath ++ path, validator)(pattern) with BaseContract

abstract class \\?(path:Path, validator:Validator[Option[JsonObject]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JsonObject])
  extends Maybe[JsonObject](path, parentPath ++ path, validator)(pattern) with BaseContract

case class \:[T](path:Path, override val validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Expected[Seq[T]](path, parentPath ++ path, validator)(pattern)

case class \:?[T](path:Path, override val validator:Validator[Option[Seq[T]]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Maybe[Seq[T]](path, parentPath ++ path, validator)(pattern)


trait Evaluator[L <: HList] {
  type Out <: HList

  def apply(json:Json, l:L):Option[Out]
}

object Evaluator {

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

object ExtractorCompositor {

  def join[L <: HList, O <: HList, T](maybes: L)(implicit ev: Evaluator.Aux[L, O], tpl:Tupler.Aux[O, T]) =
    new JsonList(maybes, ev, tpl)

  class JsonList[L <: HList, O <: HList, T](maybes: L, ev: Evaluator.Aux[L, O], tpl:Tupler.Aux[O, T]) {

    def unapply(json:Json):Option[T] = {
      ev.apply(json, maybes).map{hl =>
        tpl.apply(hl)
      }
    }
  }

}



