package jsentric

import argonaut._
import Argonaut._

trait Lens extends Functions {

  implicit def lensCombinator(f: Json => Json):LensCombinator =
    new LensCombinator(f)

  implicit def valueContractExt[T](c:ValueContract[T]):ValueContractExt[T] =
    new ValueContractExt[T](c)

  implicit def contractExt[T <: BaseContract](c:T):ContractExt[T] =
    new ContractExt(c)

  implicit def contractTypeExt[T <: ContractType](c:T):ContractTypeExt[T] =
    new ContractTypeExt(c)

  implicit def expectedLens[T](prop: Expected[T]):ExpectedLens[T] =
    new ExpectedLens(prop)

  implicit def maybeLens[T](prop: Maybe[T]):MaybeLens[T] =
    new MaybeLens(prop)

  implicit def defaultLens[T](prop: Default[T]):DefaultLens[T] =
    new DefaultLens(prop)

  implicit def jsonLens[T](json:Json):JsonLens[T] =
    new JsonLens(json)

  implicit def arrayLens[T](prop: \:[T]):ArrayLens[T] =
    new ArrayLens(prop)

  implicit def maybeArrayLens[T](prop: \:?[T]):MaybeArrayLens[T] =
    new MaybeArrayLens(prop)
}

object Lens extends Lens

class LensCombinator(val f: Json => Json) extends AnyVal {
  def ~(f2: Json => Json): Json => Json =
    (j: Json) => f2(f(j))
}

class ValueContractExt[T](val c:ValueContract[T]) extends AnyVal {
  def $create(value:T):Json =
    c.codec(value)
}

class ContractExt[T <: BaseContract](val c:T) extends AnyVal {
  def $create(f:c.type => Json => Json):Json =
    f(c)(jEmptyObject)
}

class ContractTypeExt[T <: ContractType](val c:T) extends AnyVal {
  def $create(f:c.type => Json => Json):Json =
    f(c)(Json(c.$key -> c.matcher.default))
  def $create() =
    Json(c.$key -> c.matcher.default)
}

sealed trait PropertyLens[T] extends Any with Functions {
  def prop:Property[T]

  def $get(j:Json):Option[T] =
    getValue(j, prop.absolutePath.segments).flatMap(j => prop.codec.decodeJson(j).toOption)

  def $set =
    (value:T) => (j:Json) => setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(value))
  def $maybeSet =
    (value:Option[T]) => (j:Json) =>
      value.fold(j) { v =>
        setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(v))
      }
}

class ExpectedLens[T](val prop: Expected[T]) extends AnyVal with PropertyLens[T] {
  //applies set if value is nonEmpty, does not drop on empty
  def $modify =
    (func:T => T) => (j:Json) =>
      $get(j).fold[Json](j)(v => setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(func(v))))
  def $copy =
    (p:Property[T]) => (j:Json) => {
      getValue(j, prop.absolutePath.segments) match {
        case None => j
        case Some(value) =>
          insertValue(Some(j), p.absolutePath.segments, value)
      }
    }
}

class MaybeLens[T](val prop: Maybe[T]) extends AnyVal with PropertyLens[T] {
  def $drop =
    (j:Json) => dropValue(j, prop.absolutePath.segments)
  def $setOrDrop =
    (value:Option[T]) => (j:Json) => value.fold(dropValue(j, prop.absolutePath.segments)){v =>
      setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(v))
    }
  def $modify =
    (func:Option[T] => Option[T]) => (j:Json) =>
      $setOrDrop(func($get(j)))(j)
  def $copy =
    (p:Property[T]) => (j:Json) => {
      getValue(j, prop.absolutePath.segments) match {
        case None =>
          j
        case Some(value) =>
          insertValue(Some(j), p.absolutePath.segments, value)
      }
    }

  def $nullify =
    (j:Json) => setValue(Some(j), prop.absolutePath.segments, jNull)
}

class DefaultLens[T](val prop: Default[T]) extends AnyVal with Functions {
  def $get(j:Json):T =
    getValue(j, prop.absolutePath.segments).flatMap(js => prop.codec.decodeJson(js).toOption).getOrElse(prop.default)
  def $set =
    (value:T) => (j:Json) => setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(value))
  def $maybeSet =
    (value:Option[T]) => (j:Json) =>
      value.map { v =>
        setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(v))
      }
  def $reset =
    (j:Json) => dropValue(j, prop.absolutePath.segments)
  def $setOrReset =
    (value:Option[T]) => (j:Json) => value.fold(dropValue(j, prop.absolutePath.segments)){v =>
      setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(v))
    }
  def $modify =
    (func:T => T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.codec.encode(func($get(j))))
  def $copy =
    (p:Property[T]) => (j:Json) => {
      getValue(j, prop.absolutePath.segments) match {
        case None =>
          insertValue(Some(j), p.absolutePath.segments, prop.codec.encode(prop.default))
        case Some(value) =>
          insertValue(Some(j), p.absolutePath.segments, value)
      }
    }
  def $nullify =
    (j:Json) => setValue(Some(j), prop.absolutePath.segments, jNull)
}

class JsonLens[T](val json:Json) extends AnyVal with Functions {
  def select(properties:Property[_]*):Json = {
    properties.foldLeft(jEmptyObject) { (j, p) =>
      getValue(json, p.absolutePath.segments).fold(j){v =>
        setValue(Some(j), p.absolutePath.segments, v)
      }
    }
  }
  def exclude(properties:Property[_]*):Json = {
    properties.foldLeft(json){ (j, p) =>
      dropValue(j, p.absolutePath.segments)
    }
  }
  def append(params:(String, Json)*):Json =
    json.withObject(params.foldLeft(_)((o, p) => p +: o))

  def concat(value:Json):Json =
    json.arrayOrObject[Json](
      json -->>: value -->>: jEmptyArray,
      arr => jArray(value.array.fold(arr :+ value)(a => arr ++ a)),
      obj => value.obj.fold(jArray(List(jObject(obj), value)))(o => jObject(o.toList.foldLeft(obj)((t, p) => p +: t)))
    )

  def delta(delta:Json):Json =
    applyDelta(json, delta)

  def diff(source:Json):Option[Json] =
    difference(json, source)
}

class ArrayLens[T](val prop: \:[T]) extends AnyVal with Functions {
  def $at(index:Int) =
    new Maybe[T](Path(index), prop.absolutePath \ index, EmptyValidator)(prop.elementCodec, prop.optionElementCodec)

  def $head = $at(0)

  def $append =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqCodec(current(j) :+ prop.elementCodec.encode(value)))

  def $prepend =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqCodec(prop.elementCodec(value) +: current(j)))

  protected def current(j:Json) =
    getValue(j, prop.absolutePath.segments).flatMap(js => prop.seqCodec.decodeJson(js).toOption).getOrElse(Seq.empty)
}

class MaybeArrayLens[T](val prop: \:?[T]) extends AnyVal with Functions {
  def $at(index:Int) =
    new Maybe[T](Path(index), prop.absolutePath \ index, EmptyValidator)(prop.elementCodec, prop.optionElementCodec)

  def $head = $at(0)

  def $append =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqCodec(current(j) :+ prop.elementCodec.encode(value)))

  def $prepend =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqCodec(prop.elementCodec(value) +: current(j)))

  protected def current(j:Json) =
    getValue(j, prop.absolutePath.segments).flatMap(js => prop.seqCodec.decodeJson(js).toOption).getOrElse(Seq.empty)
}