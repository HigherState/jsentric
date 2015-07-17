package jsentric

import argonaut._
import Argonaut._
import scalaz.Scalaz._
import shapeless._
import shapeless.ops.hlist._
import LooseCodecs._

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def absolutePath:Path

  def $dynamic[T](path:String)(implicit codec:CodecJson[T]) =
    new Maybe[T](path, absolutePath \ path, EmptyValidator)
}

trait Contract extends BaseContract {
  implicit protected def absolutePath:Path = Path.empty

  def unapply(j:Json):Option[Json] =
    Some(j)
}

abstract class ContractType(val $key:String, val matcher:Matcher = DefaultMatcher) extends BaseContract {
  implicit protected def absolutePath: Path = Path.empty
  def unapply(j:Json):Option[Json] =
    j.obj.exists(_($key).exists(matcher.isMatch)).option(j)

}

abstract class ValueContract[T](val validator: Validator[T] = EmptyValidator)(implicit _codec:CodecJson[T]) extends BaseContract with Property[T] {
  implicit val absolutePath: Path = Path.empty
  val relativePath: Path = Path.empty
  def codec: CodecJson[T] = _codec
  def unapply(j:Json):Option[T] =
   codec.decodeJson(j).toOption
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
  implicit def valueMatcher[T](value:T)(implicit _codec:CodecJson[T]) = new Matcher {
    val default: Json = _codec.encode(value)
    def isMatch(j: Json): Boolean = j == default
  }
}

trait SubContract {
  implicit protected def absolutePath:Path
}

trait Unapplicable[T] {
  def unapply(j:Json):Option[T]

  def @:[S, O](prev:Unapplicable[S])(implicit ev: JComposite.Aux[Unapplicable[S] :: Unapplicable[T] :: HNil, S :: T :: HNil], tpl:Tupler.Aux[S :: T :: HNil, O]) =
    JsonList(prev :: this.asInstanceOf[Unapplicable[T]] :: HNil, ev, tpl)
}

trait Property[T <: Any] extends SelfApply {
  def codec: CodecJson[T]
  def absolutePath:Path
  def relativePath:Path
  def validator:Validator[_]
}
//implicit codecs private so properties dont get implicit clashes
class Expected[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[T])(implicit private val _codec: CodecJson[T])
  extends Property[T] with Functions with Unapplicable[T] {
  def codec = _codec

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).flatMap(v => codec.decodeJson(v).toOption)
}

class Maybe[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[Option[T]])(implicit private val _codec: CodecJson[T])
  extends Property[T] with Functions with Unapplicable[Option[T]] {
  def codec = _codec

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, absolutePath.segments).fold[Option[Option[T]]](Some(None)) { v =>
      codec.decodeJson(v).toOption.map(Some(_)) //always returns Some()
    }

}

class Default[T](val relativePath:Path, implicit val absolutePath:Path, val default:T, val validator:Validator[Option[T]])(implicit private val _codec: CodecJson[T])
  extends Property[T] with Functions with Unapplicable[T] {
  def codec = _codec

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).fold[Option[T]](Some(default)) { v =>
      codec.decodeJson(v).toOption
    }
}

class EmptyProperty[T](implicit val codec: CodecJson[T]) extends Property[T] {
  def absolutePath: Path = Path.empty
  def relativePath: Path = Path.empty
  def validator: Validator[T] = ???
}

object \ {
  def apply[T](path:Path, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, codec: CodecJson[T]) =
    new Expected[T](path, parentPath ++ path, validator)(codec)
}

object \? {
  def apply[T](path:Path, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, codec: CodecJson[T]) =
    new Maybe[T](path, parentPath ++ path, validator)(codec)
}

object \! {
  def apply[T](path:Path, default:T, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, codec: CodecJson[T]) =
    new Default[T](path, parentPath ++ path, default, validator)(codec)
}

abstract class \\(path:Path, validator:Validator[Json] = EmptyValidator)(implicit parentPath:Path)
  extends Expected[Json](path, parentPath ++ path, validator) with BaseContract

abstract class \\?(path:Path, validator:Validator[Option[Json]] = EmptyValidator)(implicit parentPath:Path)
  extends Maybe[Json](path, parentPath ++ path, validator) with BaseContract

case class \:[T](path:Path, override val validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path,  private val _codec: CodecJson[Seq[T]],  private val _seqCodec: CodecJson[Seq[Json]],  private val _elementCodec: CodecJson[T])
  extends Expected[Seq[T]](path, parentPath ++ path, validator)(_codec) {
  def elementCodec = _elementCodec
  def seqCodec = _seqCodec
}

case class \:?[T](path:Path, override val validator:Validator[Option[Seq[T]]] = EmptyValidator)(implicit parentPath:Path, private val _codec: CodecJson[Seq[T]],  private val _seqCodec: CodecJson[Seq[Json]],  private val _elementCodec: CodecJson[T])
  extends Maybe[Seq[T]](path, parentPath ++ path, validator)(_codec) {
  def elementCodec = _elementCodec
  def seqCodec = _seqCodec
}







