package jsentric

import argonaut._
import Argonaut._
import scalaz.Scalaz._
import shapeless._
import shapeless.ops.hlist._

private[jsentric] sealed trait Struct extends DelayedInit {
  private[jsentric] def _propertyEdge:Option[(Path, Struct)]
  def apply[R](f:this.type => R):R = f(this)

  override def delayedInit(body: => Unit) = {
    body
    this.getClass.getMethods.foreach(m =>
      if (classOf[Struct].isAssignableFrom(m.getReturnType) && m.getTypeParameters.isEmpty) {
        m.invoke(this) match {
          case prop:Property[_] =>
            prop._propertyEdge = Some(Path(prop._nameOverride.getOrElse(m.getName)), this)
          case _ =>

        }
      }
    )
  }
}

object Struct {
  //TODO fix up
  def getPath(s:Struct):Path = {
    s._propertyEdge.fold(Path.empty){ p =>
      getPath(p._2) ++ p._1
    }
  }
}

trait BaseContract extends Struct  {

  def $dynamic[T](path:Path)(implicit codec:CodecJson[T], strictness:MaybeStrictness) = {
    val prop = new Maybe[T](EmptyValidator, None)(codec, strictness)
    prop._propertyEdge = Some(path -> this)
    prop
  }
}

trait Contract extends BaseContract {
  private[jsentric] def _propertyEdge = None
  def unapply(j:Json):Option[Json] =
    Some(j)
}

abstract class ContractType(val $key:String, val matcher:Matcher = DefaultMatcher) extends BaseContract with Unapplicable[Json] {
  private[jsentric] def _propertyEdge = None
  def unapply(j:Json):Option[Json] =
    j.obj.exists(_($key).exists(matcher.isMatch)).option(j)
}


trait Unapplicable[T] {
  def unapply(j:Json):Option[T]

  def @:[S, O](prev:Unapplicable[S])(implicit ev: Composite.Aux[Unapplicable[S] :: Unapplicable[T] :: HNil, S :: T :: HNil], tpl:Tupler.Aux[S :: T :: HNil, O]) =
    JsonList(prev :: this.asInstanceOf[Unapplicable[T]] :: HNil, ev, tpl)
}

trait Property[T <: Any] extends Struct {
  private[jsentric] var _propertyEdge:Option[(Path, Struct)] = None
  private[jsentric] def _nameOverride:Option[String]
  private[jsentric] def _codec: CodecJson[T]
  private[jsentric] def _pathValidator:Validator[_]
  private[jsentric] def _isValidType(j:Json):Boolean
}

//implicit codecs private so properties dont get implicit clashes
class Expected[T] private[jsentric](private[jsentric] val _pathValidator:Validator[T], private[jsentric] val _nameOverride:Option[String])(implicit private val __codec: CodecJson[T])
  extends Property[T] with Functions with Unapplicable[T] {

  private[jsentric] def _codec = __codec
  private[jsentric] def _isValidType(j:Json) =
    !__codec.decodeJson(j).isError

  def unapply(j:Json):Option[T] =
    getValue(j, Struct.getPath(this).segments).flatMap(v => __codec.decodeJson(v).toOption)
}


class Maybe[T] private[jsentric](private[jsentric] val _pathValidator:Validator[Option[T]], private[jsentric] val _nameOverride:Option[String])
              (implicit private val __codec: CodecJson[T], strictness:MaybeStrictness)
  extends Property[T] with Functions with Unapplicable[Option[T]] {

  private[jsentric] def _codec = __codec
  private[jsentric] def _isValidType(j:Json) =
    strictness(j, __codec).nonEmpty

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, Struct.getPath(this).segments).fold[Option[Option[T]]](Some(None)) { v =>
      strictness(v, __codec)
    }
}

class Default[T] private[jsentric](val _default:T, private[jsentric] val _pathValidator:Validator[Option[T]], private[jsentric] val _nameOverride:Option[String])(implicit private val __codec: CodecJson[T], strictness:MaybeStrictness)
  extends Property[T] with Functions with Unapplicable[T] {

  private[jsentric] def _codec = __codec
  private[jsentric] def _isValidType(j:Json) =
    strictness(j, __codec).nonEmpty

  def unapply(j:Json):Option[T] =
    getValue(j, Struct.getPath(this).segments).fold[Option[T]](Some(_default)) { v =>
      strictness(v, __codec).map(_.getOrElse(_default))
    }
}

abstract class ValueContract[T] private[jsentric](val _pathValidator: Validator[T] = EmptyValidator)(implicit __codec:CodecJson[T]) extends BaseContract with Property[T] {
  private[jsentric] def _codec = __codec
  private[jsentric] def _nameOverride: Option[String] = None
  private[jsentric] def _isValidType(j:Json) =
    !__codec.decodeJson(j).isError

  def unapply(j:Json):Option[T] =
    __codec.decodeJson(j).toOption
}

class EmptyProperty[T](implicit val _codec: CodecJson[T]) extends Property[T] {

  private[jsentric] def _nameOverride: Option[String] = None
  def _pathValidator: Validator[T] = ???
  def _isValidType(j:Json) = false
}

object \ {

  def apply[T](implicit codec: CodecJson[T]) =
    new Expected[T](EmptyValidator, None)(codec)

  def apply[T](validator:Validator[T])(implicit codec: CodecJson[T]) =
    new Expected[T](validator, None)(codec)

  def apply[T](name:String)(implicit codec: CodecJson[T]) =
    new Expected[T](EmptyValidator, Some(name))(codec)

  def apply[T](name:String, validator:Validator[T])(implicit codec: CodecJson[T]) =
    new Expected[T](validator, Some(name))(codec)
}

object \? {

  def apply[T](implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Maybe[T](EmptyValidator, None)(codec, strictness)

  def apply[T](validator:Validator[Option[T]])(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Maybe[T](validator, None)(codec, strictness)

  def apply[T](name:String)(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Maybe[T](EmptyValidator, Some(name))(codec, strictness)

  def apply[T](name:String, validator:Validator[Option[T]])(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Maybe[T](validator, Some(name))(codec, strictness)
}

object \! {

  def apply[T](default:T)(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Default[T](default, EmptyValidator, None)(codec, strictness)

  def apply[T](default:T, validator:Validator[Option[T]])(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Default[T](default, validator, None)(codec, strictness)

  def apply[T](name:String, default:T)(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Default[T](default, EmptyValidator, Some(name))(codec, strictness)

  def apply[T](name:String, default:T, validator:Validator[Option[T]])(implicit codec: CodecJson[T], strictness:MaybeStrictness) =
    new Default[T](default, validator, Some(name))(codec, strictness)
}

abstract class \\ private(_pathValidator:Validator[Json], _nameOverride:Option[String])
  extends Expected[Json](_pathValidator, _nameOverride)(OptimisticCodecs.jsonCodec) with BaseContract {

  def this() =
    this(EmptyValidator, None)
  def this(validator:Validator[Json]) =
    this(validator, None)
  def this(name:String) =
    this(EmptyValidator, Some(name))
  def this(name:String, validator:Validator[Json]) =
    this(validator, Some(name))

}

abstract class \\? private(_pathValidator:Validator[Option[Json]], _nameOverride:Option[String])(implicit strictness:MaybeStrictness)
  extends Maybe[Json](_pathValidator, _nameOverride)(OptimisticCodecs.jsonCodec, strictness) with BaseContract {

  def this()(implicit strictness:MaybeStrictness) =
    this(EmptyValidator, None)
  def this(validator:Validator[Option[Json]])(implicit strictness:MaybeStrictness) =
    this(validator, None)
  def this(name:String)(implicit strictness:MaybeStrictness) =
    this(EmptyValidator, Some(name))
  def this(name:String, validator:Validator[Option[Json]])(implicit strictness:MaybeStrictness) =
    this(validator, Some(name))
}

class ExpectedArray[T] private[jsentric](override val _pathValidator:Validator[Seq[T]], override val _nameOverride:Option[String])(
  implicit override private[jsentric] val _codec: CodecJson[Seq[T]],
  private[jsentric] val _seqCodec: CodecJson[Seq[Json]],
  private[jsentric] val _elementCodec: CodecJson[T],
  private[jsentric] val _strictness:MaybeStrictness)
  extends Expected[Seq[T]](_pathValidator, _nameOverride)(_codec)

class MaybeArray[T] private[jsentric](override val _pathValidator:Validator[Option[Seq[T]]], override val _nameOverride:Option[String])(
  implicit override private[jsentric] val _codec: CodecJson[Seq[T]],
  private[jsentric] val _optionCodec: CodecJson[Option[Seq[T]]],
  private[jsentric] val _seqCodec: CodecJson[Seq[Json]],
  private[jsentric] val _elementCodec: CodecJson[T],
  private[jsentric] val _strictness:MaybeStrictness
  ) extends Maybe[Seq[T]](_pathValidator, _nameOverride)(_codec, _strictness)

object \: {

  def apply[T](
    implicit codec: CodecJson[Seq[T]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness):ExpectedArray[T] =
      new ExpectedArray[T](EmptyValidator, None)

  def apply[T](validator:Validator[Seq[T]])(
    implicit codec: CodecJson[Seq[T]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness):ExpectedArray[T] =
     new ExpectedArray[T](validator, None)

  def apply[T](name:String)(
    implicit codec: CodecJson[Seq[T]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness):ExpectedArray[T] =
      new ExpectedArray[T](EmptyValidator, Some(name))

  def apply[T](name:String, validator:Validator[Seq[T]])(
    implicit codec: CodecJson[Seq[T]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness) =
      new ExpectedArray[T](validator, Some(name))
}


object \:? {

  def apply[T](
    implicit codec: CodecJson[Seq[T]],
    optionCodec: CodecJson[Option[Seq[T]]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness) =
      new MaybeArray[T](EmptyValidator, None)
  def apply[T](validator:Validator[Option[Seq[T]]])(
    implicit codec: CodecJson[Seq[T]],
    optionCodec: CodecJson[Option[Seq[T]]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness) =
      new MaybeArray[T](validator, None)
  def apply[T](name:String)(
    implicit codec: CodecJson[Seq[T]],
    optionCodec: CodecJson[Option[Seq[T]]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness) =
      new MaybeArray[T](EmptyValidator, Some(name))
  def apply[T](name:String, validator:Validator[Option[Seq[T]]])(
    implicit codec: CodecJson[Seq[T]],
    optionCodec: CodecJson[Option[Seq[T]]],
    seqCodec: CodecJson[Seq[Json]],
    elementCodec: CodecJson[T],
    strictness:MaybeStrictness) =
      new MaybeArray[T](validator, Some(name))
}







