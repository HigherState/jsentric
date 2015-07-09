package jsentric

import argonaut._
import Argonaut._
import scalaz.Scalaz._

//Migrated to Argonaut as is, might be possible to replace with Argonaut functionality
trait Functions extends Any {
  /**
   * Concatenate and replace json structure with delta, where a JNull or returning an Empty JObject value will clear the key value pair
   * @param delta
   * @return
   */
  def applyDelta(target:Json, delta:Json):Json =
    (target.obj, delta.obj) match {
      case (Some(ot), Some(od)) =>
        jObject(od.toList.foldLeft(ot)({
          case (acc, (k, j)) if j.isNull =>
            acc - k
          case (acc, (k, v)) => acc(k) match {
            case None => acc + (k, v)
            case Some(l) =>
              val d = applyDelta(l, v)
              if (d == jEmptyObject)
                acc - k
              else
                acc + (k, d)
          }
        }))
      case _ =>
        delta
    }

  def difference(delta:Json, source:Json):Option[Json] =
    (delta, source) match {
      case (d, s) if d == s =>
        None
      case (JObject(d), JObject(j)) =>
        val s = j.toMap
        val o = d.toList.flatMap { kvp =>
          s.get(kvp._1).fold(Option(kvp)){ v =>
            difference(kvp._2, v).map(kvp._1 -> _)
          }
        }
        o.nonEmpty.option(Json(o:_*))
      case (d, _) =>
        Some(d)
    }

  @deprecated("Use argonaut deepmerge")
  def mergeDelta(target:Json, delta:Json):Json =
    target.deepmerge(delta)


  def getValue(target:Json, segments:Segments):Option[Json] =
    (segments, target) match {
      case (Vector(), _) =>
        Some(target)
      case (Left(head) +: tail, j) =>
        j.field(head).flatMap(getValue(_, tail))
      case (Right(head) +: tail, j) =>
        j.array.flatMap(_.lift(head)).flatMap(getValue(_, tail))
      case _ => None
    }

  def setValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(j)) =>
        (key -> setValue(j.field(key), tail, value)) ->: j
      case (Left(key) +: tail, _) =>
        Json(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(j)) =>
        val array = j.arrayOrEmpty
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          jArray(left.padTo(index, jNull) :+ setValue(None, tail, value))
        else
          jArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        jArray(List.fill(index)(jNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  /**
   * Like set value except for in array, final value will be inserted in location rather than replaced
   * @param target
   * @param segments
   * @param value
   * @return
   */
  def insertValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(j)) =>
        (key -> setValue(j.field(key), tail, value)) ->: j
      case (Left(key) +: tail, _) =>
        Json(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(j)) =>
        val array = j.arrayOrEmpty
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          jArray(left.padTo(index, jNull) :+ setValue(None, tail, value))
        else if (tail.isEmpty)
          jArray(left ++ (value :: right))
        else
          jArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        jArray(List.fill(index)(jNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  def dropValue(target:Json, segments:Segments):Json =
    (segments, target) match {
      case (Left(key) +: Vector(), j) =>
        j.withObject{obj =>
          obj - key
        }
      case (Left(key) +: tail, j) =>
        j.withObject{obj =>
          obj(key).fold(obj){v => obj + (key, dropValue(v, tail))}
        }
      case (Right(index) +: Vector(), j) =>
        j.withArray{arr =>
          val (left, right) = arr.splitAt(index)
          left ++ right.drop(1)
        }
      case (Right(index) +: tail, j) =>
        j.withArray {arr =>
            val (left, right) = arr.splitAt(index)
            left ++ (dropValue(right.head, tail) :: right.drop(1))
        }
      case _ =>
        target
    }
}

object Functions extends Functions
