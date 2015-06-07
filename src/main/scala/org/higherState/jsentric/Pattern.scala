package org.higherState.jsentric

import argonaut._
import Argonaut._
import scalaz.{\/-, -\/, \/}

trait Pattern[T] {
  def apply(t:T):Json
  def schema:Json
  def unapply(json:Json):Option[T]
}
object JsonSchema {
  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"
  val REQUIRED = "required"
  val DEFAULT = "default"
}
abstract class RequiredValuePattern[T](typeName: String) extends Pattern[T] {
  import JsonSchema._

  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
    (value, currentState) match {
      case (None, None) =>
        Seq("Value expected." -> path)
      case (Some(v), _) if this.unapply(v).isEmpty =>
        Seq(s"Unexpected type '${v.getClass.getSimpleName}'." -> path)
      case _ =>
        Nil
    }

  def schema:Json =
    (TYPE -> jString(typeName)) ->: (REQUIRED -> jTrue) ->: jEmptyObject
}

trait SeqExtractor[T] {
  def unapply(s:Seq[Json]):Option[Seq[T]]
}

trait Patterns {

  import JsonSchema._

  implicit val booleanPattern:Pattern[Boolean] = new RequiredValuePattern[Boolean]("boolean"){
    def apply(t: Boolean): Json =
      jBool(t)

    def unapply(json: Json): Option[Boolean] =
      json.bool
  }

  implicit val stringPattern:Pattern[String] = new RequiredValuePattern[String]("string") {
    def apply(t: String): Json =
      jString(t)

    def unapply(json: Json): Option[String] =
      json.string
  }

  implicit val longPattern:Pattern[Long] = new RequiredValuePattern[Long]("long") {
    def unapply(json: Json): Option[Long] =
      json.number.collect {
        case j if j % 1 == 0 && j <= Long.MaxValue && j >= Long.MinValue => j.toLong
      }

    def apply(t: Long): Json =
      jNumber(t)
  }

  implicit val intPattern:Pattern[Int] = new RequiredValuePattern[Int]("int") {
    def unapply(json: Json): Option[Int] =
      json.number.collect {
        case j if j % 1 == 0 && j <= Int.MaxValue && j >= Int.MinValue => j.toInt
      }

    def apply(t: Int): Json =
      jNumber(t)
  }

  implicit val doublePattern:Pattern[Double] = new RequiredValuePattern[Double]("double") {
    def unapply(json: Json): Option[Double] =
      json.number

    def apply(t: Double): Json =
      jNumber(t)
  }

  implicit val float:Pattern[Float] = new RequiredValuePattern[Float]("float") {
    def unapply(json: Json): Option[Float] =
      json.number.collect {
        case j if j >= Float.MinValue && j <= Float.MaxValue => j.toFloat
      }

    def apply(t: Float): Json =
      jNumber(t)
  }

  implicit val jsonPattern:Pattern[Json] = new RequiredValuePattern[Json]("json"){
    def unapply(json: Json): Option[Json] =
      Some(json)

    def apply(t:Json): Json = t
  }

  implicit val jsonObjectPattern:Pattern[JsonObject] = new RequiredValuePattern[JsonObject]("object"){
    def unapply(json: Json): Option[JsonObject] =
      json.obj

    def apply(t: JsonObject): Json =
      jObject(t)
  }

  implicit val jsonObjectMapPattern:Pattern[JsonObjectMap] = new RequiredValuePattern[JsonObjectMap]("object"){
    def unapply(json: Json): Option[JsonObjectMap] =
      json.obj.map(_.toInsertionMap)

    def apply(t: JsonObjectMap): Json =
      jObjectMap(t)
  }

  implicit val jsonMapPattern:Pattern[JsonMap] = new RequiredValuePattern[JsonMap]("object"){
    def unapply(json: Json): Option[JsonMap] =
      json.obj.map(_.toMap)

    def apply(t: JsonMap): Json =
      jObjectFields(t.toSeq:_*)
  }

  implicit val jsonArrayPattern:Pattern[JsonArray] = new RequiredValuePattern[JsonArray]("array"){
    def unapply(json: Json): Option[JsonArray] =
      json.array

    def apply(t:JsonArray): Json =
      jArray(t)
  }

  implicit val jSeqPattern:Pattern[Seq[Json]] = new RequiredValuePattern[Seq[Json]]("array"){
    def unapply(json: Json): Option[Seq[Json]] =
      json.array

    def apply(t: Seq[Json]): Json =
      jArray(t.toList)
  }

  implicit val jVectorPattern:Pattern[Vector[Json]] = new RequiredValuePattern[Vector[Json]]("array"){
    def unapply(json: Json): Option[Vector[Json]] =
      json.array.map(_.toVector)

    def apply(t: Vector[Json]): Json =
      jArray(t.toList)
  }

  //all or nothing extraction
  implicit def allExtractedPattern[T](implicit pattern:Pattern[T]) = new SeqExtractor[T] {
    def unapply(j:Seq[Json]):Option[Seq[T]] = {
      val t = j.toIterator.map(pattern.unapply).takeWhile(_.isDefined).flatten.toSeq
      if (t.size == j.size) Some(t)
      else None
    }
  }

  implicit def seqPattern[T](implicit seqExtractor:SeqExtractor[T], pattern:Pattern[T]) = new Pattern[Seq[T]] {
    def unapply(json: Json): Option[Seq[T]] =
      json.array.flatMap(seqExtractor.unapply)

    def apply(t: Seq[T]): Json =
      jArray(t.map(pattern.apply).toList)

    def schema = (TYPE := "array") ->: (ITEMS -> pattern.schema) ->: jEmptyObject
  }

  implicit def optionPattern[T](implicit pattern:Pattern[T]) = new Pattern[Option[T]] {
    def unapply(json: Json): Option[Option[T]] = {
      if (json.isNull) Some(None)
      else pattern.unapply(json).map(Some(_))
    }

    def apply(t: Option[T]): Json =
      t.fold[Json](jNull)(pattern.apply)

    def schema =
      (REQUIRED -> jFalse) ->: pattern.schema
  }

  implicit def tuplePattern[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    def unapply(json: Json): Option[(T1, T2)] =
      json.array.collect {
        case pattern1(l) :: pattern2(r) :: Nil => l -> r
      }

    def apply(t: (T1, T2)): Json =
     pattern1(t._1) -->>: pattern2(t._2) -->>: jEmptyArray

    def schema =
      (TYPE := "array") ->: (ITEMS -> (pattern1.schema -->>: pattern2.schema -->>: jEmptyArray)) ->: jEmptyObject
  }

  implicit def eitherPattern[T1,T2] (implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[Either[T1, T2]] {
    def unapply(json: Json): Option[Either[T1, T2]] =
      pattern1.unapply(json).map(Left(_)).orElse(pattern2.unapply(json).map(Right(_)))

    def apply(t: Either[T1, T2]): Json =
      t match {
        case Left(t1) => pattern1.apply(t1)
        case Right(t2) => pattern2.apply(t2)
      }

    def schema =
      (TYPE -> (pattern1.schema -->>: pattern2.schema -->>: jEmptyObject)) ->: jEmptyObject
  }

  implicit def disruptPattern[T1,T2] (implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[\/[T1, T2]] {
    def unapply(json: Json): Option[\/[T1, T2]] =
      pattern1.unapply(json).map(-\/(_)).orElse(pattern2.unapply(json).map(\/-(_)))

    def apply(t: \/[T1, T2]): Json =
      t match {
        case -\/(t1) => pattern1.apply(t1)
        case \/-(t2) => pattern2.apply(t2)
      }

    def schema =
      (TYPE -> (pattern1.schema -->>: pattern2.schema -->>: jEmptyObject)) ->: jEmptyObject
  }
}

object Patterns extends Patterns
