package jsentric

import argonaut.Argonaut._
import argonaut.{CodecJson, Json}


trait Matcher  {
  def isMatch(j:Json):Boolean
  def default:Json
}

object DefaultMatcher extends Matcher {
  def isMatch(j:Json):Boolean = true
  def default:Json = jNull
}

object JsonMatchers {
  implicit def valueMatcher[T](value:T)(implicit _codec:CodecJson[T]) = new Matcher {
    val default: Json = _codec.encode(value)
    def isMatch(j: Json): Boolean = j == default
  }
}
