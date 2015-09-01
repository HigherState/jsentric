package jsentric

import argonaut.Argonaut._
import argonaut.Json

trait Projection {

  implicit def jsonProjectionExt(json:Json):JsonProjectionExt =
    new JsonProjectionExt(json)

  implicit def projection[T](prop:Property[T]):ProjectionQuery[T] =
    new ProjectionQuery(prop)
}

object Projection extends Projection


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
    Query.pathToObject(prop.absolutePath.segments, obj)
}
