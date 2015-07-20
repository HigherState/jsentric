package jsentric

import argonaut._
import Argonaut._

object JArray {
  def unapply(json:Json):Option[JsonArray] =
    json.array
}
object JObject {
  def unapply(json:Json):Option[JsonObject] =
    json.obj
}
object JNumber {
  def unapply(json:Json):Option[Double] =
    json.number.map(_.toDouble)
}
object JString {
  def unapply(json:Json):Option[String] =
    json.string
}
object JBool {
  def unapply(json:Json):Option[Boolean] =
    json.bool
}
object JMap {
  def unapply(json:Json):Option[Map[JsonField, Json]] =
    json.obj.map(_.toMap)
}