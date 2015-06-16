package org.higherState.jsentric

import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import Lens._

trait Query extends Functions with Lens {

  implicit def jsonQueryExt(json:Json) =
    new JsonQueryExt(json)

  implicit def valueQuery[T](prop:Property[T]) =
    new ValueQuery(prop)

  implicit def numericQuery[T >: JNumeric](prop:Property[T]) =
    new NumericQuery(prop)
}

object Query {
  private[jsentric] def apply(value:Option[Json], query:JsonObject):Boolean =
    query.toList.forall{
      case ("$and", JArray(values)) =>
        values.flatMap(_.obj).forall(apply(value, _))
      case ("$or", JArray(values)) =>
        values.flatMap(_.obj).exists(apply(value, _))
      case ("$eq", v) =>
        value.exists(_ == v)
      case ("$ne", v) =>
        !value.exists(_ == v) //neq doesnt require existence, as per mongodb
      case ("$lt", v) =>
        value.exists(order.lift(_, v).exists(_ == Ordering.LT))
      case ("$gt", v) =>
        value.exists(order.lift(_, v).exists(_ == Ordering.GT))
      case ("$lte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.LT || r == Ordering.EQ))
      case ("$gte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.GT || r == Ordering.EQ))
      case ("$in", JArray(values)) =>
        value.exists(j => values.exists(order.lift(_, j) == Ordering.EQ))
      case ("$nin", JArray(values)) =>
        !value.exists(j => values.exists(order.lift(_, j) == Ordering.EQ)) //nin doesnt require existence, as per mongodb
      case ("$exists", JBool(v)) =>
        value.isDefined == v
      case (key, JObject(obj)) =>
        apply(value.flatMap(_.field(key)), obj)
      case (key, v) =>
        value.flatMap(_.obj).fold(false){l =>
          l(key).exists(j => j == v)
        }
    }

  private[jsentric] def pathToObject(path:Segments,obj:Json):Json= {
    path match {
      case Seq() => obj
      case head :+ Left(tail) => pathToObject(head, Json(tail -> obj))
    }
  }

  def order:PartialFunction[(Json, Json), Ordering] = {
    case (JNumber(x), JNumber(y)) => x ?|? y
    case (JString(x), JString(y)) => x ?|? y
    case (JBool(x), JBool(y)) => x ?|? y
  }
}

class JsonQueryExt(val json:Json) extends AnyVal with Functions {
  def isMatch(value:Json) =
    value.obj.fold(json == value){o => Query.apply(Some(value), o)}

  def &&(d:Json) =
    json.obj.collect{
      case obj if (obj ?? "$or") && d.objectFields.exists(_.contains("$or")) =>
        Json("$and" -> jArray(List(json, d)))
      case obj if obj ?? "$and" =>
        d.obj.collect {
          case obj2 if obj ?? "$and" =>
            Json("$and" -> obj("$and").get.concat(obj2("$and").get))
        }.getOrElse(Json("$and" -> obj("$and").get.concat(d)))
    }.getOrElse(applyDelta(json, d))

  def ||(d:Json) =
    json.obj.map{o =>
      o("$or").flatMap{j =>
        j.array.map(a => Json("$or" -> jArray(a :+ d)))
      }
    }.getOrElse(Json("$or" -> jArray(List(json, d))))
}

class ValueQuery[T](val prop: Property[T]) extends AnyVal {

  def $eq(value:T) = nest(prop.pattern.apply(value))
  def $ne(value:T) = nest(Json("$ne" -> prop.pattern.apply(value)))
  def $in(values:T*) = nest(Json("$in" -> jArray(values.toList.map(prop.pattern.apply))))
  def $nin(values:T*) = nest(Json("$nin" -> jArray(values.toList.map(prop.pattern.apply))))
  def $exists(value:Boolean) = nest(Json("$exists" := value))

  private def nest(obj:Json) =
    Query.pathToObject(prop.absolutePath.segments, obj)
}

class NumericQuery[T >: JNumeric](val prop: Property[T]) extends AnyVal {

  def $lt(value:Double) = nest(Json("$lt" := value))
  def $lt(value:Long) = nest(Json("$lt" := value))

  def $gt(value:Double) = nest(Json("$gt" := value))
  def $gt(value:Long) = nest(Json("$gt" := value))

  def $lte(value:Double) = nest(Json("$lt" := value))
  def $lte(value:Long) = nest(Json("$lt" := value))

  def $gte(value:Double) = nest(Json("$gt" := value))
  def $gte(value:Long) = nest(Json("$gt" := value))

  private def nest(obj:Json) =
    Query.pathToObject(prop.absolutePath.segments, obj)
}