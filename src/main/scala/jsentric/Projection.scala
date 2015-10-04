package jsentric

import argonaut.Argonaut._
import argonaut.Json

trait Projection {

  implicit def jsonProjectionExt(json:Json):JsonProjectionExt =
    new JsonProjectionExt(json)

  implicit def projectionExt[T](prop:Property[T]):ProjectionQuery[T] =
    new ProjectionQuery(prop)
}

object Projection extends Projection {
  def paths(projection:Json):Option[Set[Path]] =
    paths(projection, Vector.empty)
  private def paths(projection:Json, segments:Segments):Option[Set[Path]] = {
    projection.obj.flatMap{ o =>
      val pairs = o.toList.flatMap {
        case (key, JLong(1) | JDouble(1)) =>
          Some(Set(Path(segments :+ Left(key))))
        case (key, j) if j.isObject =>
          paths(j, segments :+ Left(key))
        case _ =>
          None
      }
      if (pairs.length < o.size) None
      else Some(pairs.reduce(_ ++ _))
    }
  }
}


class JsonProjectionExt(val json:Json) extends AnyVal with Functions {
  def $select(value:Json) =
    select(json, value)

  def &(d:Json):Json =
    applyDelta(json, d)
}

class ProjectionQuery[T](val prop:Property[T]) extends AnyVal {

  def $:Json =
    nest(jNumber(1))

  private def nest(obj:Json) =
    Query.pathToObject(Struct.getPath(prop).segments, obj)
}
