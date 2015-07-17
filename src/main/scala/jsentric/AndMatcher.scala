package jsentric

import argonaut.{CodecJson, Json}

trait AndMatcher {
  object && {
    def unapply[A](a: A) = Some((a, a))
  }
}
trait CodecMatcher {
  def Codec[T](implicit codec:CodecJson[T]) =
    new CodecWrapper[T](codec)
}

class CodecWrapper[T](val codec:CodecJson[T]) extends AnyVal {
  def unapply(j: Json): Option[T] =
    codec.decodeJson(j).toOption

  def apply(t:T):Json =
    codec.encode(t)
}