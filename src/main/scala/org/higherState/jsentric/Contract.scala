package org.higherState.jsentric

import argonaut._
import Argonaut._

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def absolutePath:Path
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

trait Property[T <: Any] extends SelfApply {
  def pattern:Pattern[T]
  def absolutePath:Path
  def relativePath:Path
  def validator:Validator[T]
}

class Expected[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[T])(implicit val pattern:Pattern[T])
  extends Property[T] with Functions {

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).flatMap(pattern.unapply)
}

class Maybe[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[Option[T]])(implicit val pattern:Pattern[Option[T]])
  extends Property[Option[T]] with Functions {

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, absolutePath.segments).fold[Option[Option[T]]](Some(None)) { v =>
      pattern.unapply(v) //always returns Some()
    }

}

class Default[T](val relativePath:Path, implicit val absolutePath:Path, default:T, val validator:Validator[Option[T]])(implicit val pattern:Pattern[Option[T]])
  extends Property[Option[T]] with Functions {

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).fold[Option[T]](Some(default)) { v =>
      pattern.unapply(v).map(_.getOrElse(default)) //always returns Some()
    }
}

object \ {
  def apply[T](path:Path, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) =
    new Expected[T](path, parentPath ++ path, validator)(pattern)
}

object \? {

  def apply[T](path:Path, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) =
    new Maybe[T](path, parentPath ++ path, validator)(pattern)
}

object \! {
  def apply[T](path:Path, default:T, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) =
    new Default[T](path, parentPath ++ path, default, validator)(pattern)
}

abstract class \\(path:Path, validator:Validator[JsonObject] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JsonObject])
  extends Expected[JsonObject](path, parentPath ++ path, validator)(pattern) with BaseContract

abstract class \\?(path:Path, validator:Validator[Option[JsonObject]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[JsonObject]])
  extends Maybe[JsonObject](path, parentPath ++ path, validator)(pattern) with BaseContract

case class \:[T](path:Path, override val validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Expected[Seq[T]](path, parentPath ++ path, validator)(pattern)

case class \:?[T](path:Path, override val validator:Validator[Option[Seq[T]]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[Seq[T]]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Maybe[Seq[T]](path, parentPath ++ path, validator)(pattern)