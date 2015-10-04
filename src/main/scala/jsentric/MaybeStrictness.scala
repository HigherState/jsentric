package jsentric

import argonaut.{CodecJson, Json}

trait MaybeStrictness {
  def apply[T](value:Json, codec:CodecJson[T]):Option[Option[T]]
}
object MaybeOptimistic extends MaybeStrictness {
  override def apply[T](value: Json, codec: CodecJson[T]): Option[Option[T]] =
    Some(codec.decodeJson(value).toOption)
}
object MaybeNull extends MaybeStrictness {
  override def apply[T](value: Json, codec: CodecJson[T]): Option[Option[T]] =
    if (value.isNull) Some(None)
    else codec.decodeJson(value).toOption.map(Some(_))
}
object MaybeStrict extends MaybeStrictness {
  override def apply[T](value: Json, codec: CodecJson[T]): Option[Option[T]] =
    if (value.isNull) Some(None)
    else codec.decodeJson(value).toOption.map(Some(_))
}