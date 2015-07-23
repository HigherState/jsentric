package jsentric

import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import Lens._

trait Query extends Functions with Lens {

  implicit def jsonQueryExt(json:Json):JsonQueryExt =
    new JsonQueryExt(json)

  implicit def valueQuery[T](prop:Property[T]):ValueQuery[T] =
    new ValueQuery(prop)

  implicit def maybeQuery[T](prop:Maybe[T]):MaybeQuery[T] =
    new MaybeQuery(prop)

  implicit def numericQuery[T >: JNumeric](prop:Property[T]):NumericQuery[T] =
    new NumericQuery(prop)

  implicit class ArrayQuery[T](val prop: Expected[Seq[T]])(implicit codec: CodecJson[T]) {

    def $elemMatch(f:Property[T] => Json):Json =
      nest(("$elemMatch" -> f(new EmptyProperty[T])) ->: jEmptyObject)

    private def nest(obj:Json) =
      Query.pathToObject(prop.absolutePath.segments, obj)
  }

  implicit class MaybeArrayQuery[T](val prop: Maybe[Seq[T]])(implicit codec: CodecJson[T]) {

    def $elemMatch(f:Property[T] => Json):Json =
      nest(("$elemMatch" -> f(new EmptyProperty[T])) ->: jEmptyObject)


    private def nest(obj:Json) =
      Query.pathToObject(prop.absolutePath.segments, obj)
  }

  implicit class SetQuery[T](val prop: Expected[Set[T]])(implicit codec: CodecJson[T]) {

    def $elemMatch(f:Property[T] => Json):Json =
      nest(("$elemMatch" -> f(new EmptyProperty[T])) ->: jEmptyObject)


    private def nest(obj:Json) =
      Query.pathToObject(prop.absolutePath.segments, obj)
  }

  implicit class MaybeSetQuery[T](val prop: Maybe[Set[T]])(implicit codec: CodecJson[T]) {

    def $elemMatch(f:Property[T] => Json):Json =
      nest(("$elemMatch" -> f(new EmptyProperty[T])) ->: jEmptyObject)

    private def nest(obj:Json) =
      Query.pathToObject(prop.absolutePath.segments, obj)
  }

  def not(json:Json) =
    Json("$not" -> json)

}

object Query {
  private[jsentric] def apply(value:Option[Json], query:JsonObject):Boolean =
    query.toList.forall{
      case ("$and", JArray(values)) =>
        values.flatMap(_.obj).forall(apply(value, _))
      case ("$or", JArray(values)) =>
        values.flatMap(_.obj).exists(apply(value, _))
      case ("$eq", v) =>
        value.contains(v)
      case ("$ne", v) =>
        !value.contains(v) //neq doesnt require existence, as per mongodb
      case ("$lt", v) =>
        value.exists(order.lift(_, v).contains(Ordering.LT))
      case ("$gt", v) =>
        value.exists(order.lift(_, v).contains(Ordering.GT))
      case ("$lte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.LT || r == Ordering.EQ))
      case ("$gte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.GT || r == Ordering.EQ))
      case ("$in", JArray(values)) =>
        value.exists(j => values.exists(order.lift(_, j).contains(Ordering.EQ)))
      case ("$nin", JArray(values)) =>
        !value.exists(j => values.exists(order.lift(_, j).contains(Ordering.EQ))) //nin doesnt require existence, as per mongodb
      case ("$exists", JBool(v)) =>
        value.isDefined == v
      case ("$not", v) =>
        v.obj.exists(o => !apply(value, o))
      case ("$elemMatch", JObject(j)) =>
        value.collect{ case JArray(seq) => seq.exists(s => apply(Some(s), j))}.getOrElse(false)
      case ("$elemMatch", v) =>
        value.collect{ case JArray(seq) => seq.contains(v)}.getOrElse(false)
      case (key, JObject(obj)) =>
        apply(value.flatMap(_.field(key)), obj)
      case (key, v) =>
        value.flatMap(_.obj).fold(false){l =>
          l(key).contains(v)
        }
    }

  private[jsentric] def pathToObject(path:Segments,obj:Json):Json= {
    path match {
      case Seq() => obj
      case head :+ Left(tail) => pathToObject(head, Json(tail -> obj))
    }
  }

  def order:PartialFunction[(Json, Json), Ordering] = {
    case (JDouble(x), JDouble(y)) => x ?|? y
    case (JLong(x), JLong(y)) => x ?|? y
    case (JString(x), JString(y)) => x ?|? y
    case (JBool(x), JBool(y)) => x ?|? y
    case (JDouble(x), JLong(y)) => x ?|? y
    case (JLong(x), JDouble(y)) => x.toDouble ?|? y
  }
}

class JsonQueryExt(val json:Json) extends AnyVal with Functions {
  def isMatch(value:Json) =
    json.obj.fold(json == value){o => Query.apply(Some(value), o)}

  def &&(d:Json):Json =
    json.obj.collect{
      case obj if (obj ?? "$or") && d.objectFields.exists(_.contains("$or")) =>
        Json("$and" -> jArray(List(json, d)))
      case obj if obj ?? "$and" =>
        d.obj.collect {
          case obj2 if obj ?? "$and" =>
            Json("$and" -> obj("$and").get.concat(obj2("$and").get))
        }.getOrElse(Json("$and" -> obj("$and").get.concat(d)))
    }.getOrElse(applyDelta(json, d))

  def ||(d:Json):Json =
    json.obj.flatMap{o =>
      o("$or").flatMap{j =>
        j.array.map(a => Json("$or" -> jArray(a :+ d)))
      }
    }.getOrElse(Json("$or" -> jArray(List(json, d))))
}
//Handle default?
class ValueQuery[T](val prop: Property[T]) extends AnyVal {

  def $eq(value:T) = nest(prop.codec(value))
  //Currently not supporting chaining of $ne in an && for the same field
  def $ne(value:T) = nest(Json("$ne" -> prop.codec(value)))
  def $in(values:T*) = nest(Json("$in" -> jArray(values.toList.map(prop.codec.apply))))
  def $nin(values:T*) = nest(Json("$nin" -> jArray(values.toList.map(prop.codec.apply))))

  private def nest(obj:Json) =
    Query.pathToObject(prop.absolutePath.segments, obj)
}

class MaybeQuery[T](val prop:Maybe[T]) extends AnyVal {
  def $exists(value:Boolean) = nest(Json("$exists" := value))
  private def nest(obj:Json) =
    Query.pathToObject(prop.absolutePath.segments, obj)
}

class NumericQuery[T >: JNumeric](val prop: Property[T]) extends AnyVal {

  def $lt(value:Double) = nest(Json("$lt" -> jNumberOrString(value)))
  def $lt(value:Long) = nest(Json("$lt" -> jNumber(value)))

  def $gt(value:Double) = nest(Json("$gt" -> jNumberOrString(value)))
  def $gt(value:Long) = nest(Json("$gt" -> jNumber(value)))

  def $lte(value:Double) = nest(Json("$lt" -> jNumberOrString(value)))
  def $lte(value:Long) = nest(Json("$lt" -> jNumber(value)))

  def $gte(value:Double) = nest(Json("$gt" -> jNumberOrString(value)))
  def $gte(value:Long) = nest(Json("$gt" -> jNumber(value)))

  private def nest(obj:Json) =
    Query.pathToObject(prop.absolutePath.segments, obj)
}


