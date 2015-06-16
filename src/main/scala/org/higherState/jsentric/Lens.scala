package org.higherState.jsentric

import argonaut._
import Argonaut._

trait Lens extends Functions {

  implicit def lensCombinator(f: Json => Json) =
    new LensCombinator(f)

  implicit def contractExt[T <: BaseContract](c:T) =
    new ContractExt(c)

  implicit def contractTypeExt[T <: ContractType](c:T) =
    new ContractTypeExt(c)

  implicit def valueLens[T](prop: Property[T]) =
    new ValueLens(prop)

  implicit def jsonLens[T](json:Json) =
    new JsonLens(json)

  implicit def maybeLens[T](prop: Property[Option[T]]) =
    new MaybeLens(prop)

  implicit def arrayLens[T](prop: \:[T]) =
    new ArrayLens(prop)

  implicit def maybeArrayLens[T](prop: \:?[T]) =
    new MaybeArrayLens(prop)
}

object Lens extends Lens

class LensCombinator(val f: Json => Json) extends AnyVal {
  def ~(f2: Json => Json): Json => Json =
    (j: Json) => f2(f(j))
}

class ContractExt[T <: BaseContract](val c:T) extends AnyVal {
  def $create(f:c.type => Json => Json):Json =
    f(c)(jEmptyArray)
}

class ContractTypeExt[T <: ContractType](val c:T) extends AnyVal {
  def $create(f:c.type => Json => Json):Json =
    f(c)(Json(c.key -> c.matcher.default))
  def $create() =
    Json(c.key -> c.matcher.default)
}

class ValueLens[T](val prop: Property[T]) extends AnyVal with Functions {

  def $get(j:Json):Option[T] =
    getValue(j, prop.absolutePath.segments).flatMap(prop.pattern.unapply)
  def $set =
    (value:T) => (j:Json) => setValue(Some(j), prop.absolutePath.segments, prop.pattern(value))
  //applies set if value is nonEmpty, does not drop on empty
  def $optionSet =
    (value:Option[T]) => (j:Json) => value.fold(j){v =>
      setValue(Some(j), prop.absolutePath.segments, prop.pattern(v))
    }
  def $modify =
    (func:T => T) => (j:Json) =>
      $get(j).fold[Json](j)(v => setValue(Some(j), prop.absolutePath.segments, prop.pattern(func(v))))
  def $maybeModify =
    (func:Option[T] => T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.pattern(func($get(j))))
  def $copy =
    (p:Property[T]) => (j:Json) => {
      getValue(j, prop.absolutePath.segments) match {
        case None =>
        case Some(value) =>
          insertValue(Some(j), p.absolutePath.segments, value)
      }
    }
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
}

class MaybeLens[T](val prop: Property[Option[T]]) extends AnyVal with Functions {
  def $drop =
    (j:Json) => dropValue(j, prop.absolutePath.segments)

  def $setOrDrop =
    (value:Option[Option[T]]) => (j:Json) => value.fold(dropValue(j, prop.absolutePath.segments)){v =>
      setValue(Some(j), prop.absolutePath.segments, prop.pattern(v))
    }
}

class ArrayLens[T](val prop: \:[T]) extends AnyVal with Functions {
  def $at(index:Int) = ArrayElement[T](index, prop.absolutePath)(prop.elementPattern)

  def $head = ArrayElement[T](0, prop.absolutePath)(prop.elementPattern)

  def $append =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

  def $prepend =
    (value:T) => (j:Json) =>
      prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

  protected def current(j:Json) = getValue(j, prop.absolutePath.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
}

class MaybeArrayLens[T](val prop: \:?[T]) extends AnyVal with Functions {
  def $at(index:Int) = ArrayElement[T](index, prop.absolutePath)(prop.elementPattern)

  def $head = ArrayElement[T](0, prop.absolutePath)(prop.elementPattern)

  def $append =
    (value:T) => (j:Json) =>
      setValue(Some(j), prop.absolutePath.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

  def $prepend =
    (value:T) => (j:Json) =>
      prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

  protected def current(j:Json) = getValue(j, prop.absolutePath.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
}

case class ArrayElement[T](index:Int, arrayPath:Path)(implicit val pattern:Pattern[T]) extends Property[T]  {

  def relativePath: Path = Path(index)
  def validator: Validator[T] = EmptyValidator
  def absolutePath = arrayPath \ index
}