package jsentric

import argonaut._
import Argonaut._
import scalaz.{\/-, -\/, \/}

object JsonSchema {
  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"
  val REQUIRED = "required"
  val DEFAULT = "default"
}

trait SeqExtractor[T] {
  def unapply(s:Seq[Json]):Option[Seq[T]]
}

trait Codecs extends EncodeJsons with DecodeJsons {

  implicit lazy val booleanCodec =
    argonaut.CodecJson.derived[Boolean]

  implicit lazy val stringCodec =
    argonaut.CodecJson.derived[String]

  implicit lazy val longCodec =
    argonaut.CodecJson.derived[Long]

  implicit lazy val intCodec =
    argonaut.CodecJson.derived[Int]

  implicit lazy val doubleCodec =
    argonaut.CodecJson.derived[Double]

  implicit lazy val floatCodec =
    argonaut.CodecJson.derived[Float]

  implicit lazy val jsonObjectEncoder = new EncodeJson[JsonObject] {
    def encode(a: JsonObject): Json =
      jObject(a)
  }
  implicit lazy val jsonObjectDecoder =
    optionDecoder(_.obj, "object")

  implicit lazy val jsonObjectCodec =
    argonaut.CodecJson.derived[JsonObject]

  implicit lazy val jsonCodec =
    argonaut.CodecJson.derived[Json]

  implicit lazy val jsonArrayCodec =
    argonaut.CodecJson.derived[JsonArray]

  implicit def optionCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived(OptionEncodeJson(codec.Encoder), OptionDecodeJson(codec.Decoder))


  implicit def tupleCodec[T1,T2](implicit codec1:CodecJson[T1], codec2:CodecJson[T2]) =
    argonaut.CodecJson.derived(Tuple2EncodeJson(codec1.Encoder, codec2.Encoder), Tuple2DecodeJson(codec1.Decoder, codec2.Decoder))

  implicit def eitherCodec[L, R](implicit left:CodecJson[L], right:CodecJson[R]) =
    argonaut.CodecJson.derived(
      new EncodeJson[Either[L,R]]{
        def encode(a: Either[L, R]): Json = {
          a match {
            case Left(l) => left.encode(l)
            case Right(r) => right.encode(r)
          }
        }
      },
      optionDecoder[Either[L,R]](e => left.decodeJson(e).toOption.map(Left(_)).orElse(right.decodeJson(e).toOption.map(Right(_))), "either")
    )

  implicit def disruptCodec[L, R](implicit left:CodecJson[L], right:CodecJson[R]) =
    argonaut.CodecJson.derived(
      new EncodeJson[\/[L,R]]{
        def encode(a: \/[L, R]): Json = {
          a match {
            case -\/(l) => left.encode(l)
            case \/-(r) => right.encode(r)
          }
        }
      },
      optionDecoder[\/[L,R]](e => left.decodeJson(e).toOption.map(-\/(_)).orElse(right.decodeJson(e).toOption.map(\/-(_))), "either")
    )
}

trait OptimisticCodecs extends Codecs {

  implicit val maybeStrictness:MaybeStrictness = MaybeOptimistic

  override implicit def BooleanDecodeJson: DecodeJson[Boolean] = {
    optionDecoder( x =>
      x.bool.orElse(x.string.collect {
        case t if t.equalsIgnoreCase("true") => true
        case f if f.equalsIgnoreCase("false") => false
      })
    , "Boolean")
  }

  implicit lazy val jSeqCodec:CodecJson[Seq[Json]] =
    seqCodec(jsonCodec)

  implicit lazy val jSetCodec:CodecJson[Set[Json]] =
    setCodec(jsonCodec)

  implicit lazy val jVectorCodec:CodecJson[Vector[Json]] =
    vectorCodec(jsonCodec)

  implicit def seqCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Seq[T]](
      new EncodeJson[Seq[T]]{
        def encode(a: Seq[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Seq[T]](_.array.map(_.flatMap(t => codec.decodeJson(t).toOption)), "array")
    )

  implicit def setCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Set[T]](
      new EncodeJson[Set[T]]{
        def encode(a: Set[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Set[T]](_.array.map(_.flatMap(t => codec.decodeJson(t).toOption).toSet), "array")
    )

  implicit def vectorCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Vector[T]](
      new EncodeJson[Vector[T]]{
        def encode(a: Vector[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Vector[T]](_.array.map(_.flatMap(t => codec.decodeJson(t).toOption).toVector), "array")
    )

  implicit def mapCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Map[String, T]](
      new EncodeJson[Map[String, T]]{
        def encode(a: Map[String, T]): Json =
          a.mapValues(codec.encode).asJson
      },
      optionDecoder[Map[String, T]](_.obj.map(_.toList.flatMap(t => codec.decodeJson(t._2).toOption.map(t._1 -> _)).toMap), "object")
    )
}

trait PessimisticCodecs extends Codecs {
  import scalaz._
  import Scalaz._

  implicit val maybeStrictness:MaybeStrictness = MaybeNull

  override implicit def BooleanDecodeJson:DecodeJson[Boolean] =
    optionDecoder(x => x.bool, "Boolean")

  override implicit def DoubleDecodeJson: DecodeJson[Double] =
    optionDecoder(x => x.number.map(_.toDouble), "Double")

  override implicit def FloatDecodeJson: DecodeJson[Float] =
    optionDecoder(x => x.number.map(_.toDouble).collect{ case f if f >= Float.MinValue && f <= Float.MaxValue => f.toFloat}, "Float")

  override implicit def IntDecodeJson: DecodeJson[Int] =
    optionDecoder(x => x.number.flatMap {
      case JsonLong(l) =>
        if (l >= Int.MinValue && l <= Int.MaxValue) Some(l.toInt)
        else None
      case n => Option(n.toDouble).collect { case f if f >= Int.MinValue && f <= Int.MaxValue && f % 1 == 0 => f.toInt }
    }, "Int")

  override implicit def LongDecodeJson: DecodeJson[Long] =
    optionDecoder(x => x.number.flatMap {
      case JsonLong(l) => Some(l)
      case n => Option(n.toDouble).collect { case f if f >= Long.MinValue && f <= Long.MaxValue && f % 1 == 0 => f.toLong }
    }, "Long")

  override implicit def ShortDecodeJson: DecodeJson[Short] =
    optionDecoder(x => x.number.flatMap {
      case JsonLong(l) =>
        if (l >= Short.MinValue && l <= Short.MaxValue) Some(l.toShort)
        else None
      case n => Option(n.toDouble).collect { case f if f >= Short.MinValue && f <= Short.MaxValue && f % 1 == 0 => f.toShort }
    }, "Short")

  override implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson {r =>
      if (r.focus.isNull) DecodeResult.ok(None)
      else e.decodeJson(r.focus).map(Some(_))
    }

  implicit def jSeqCodec:CodecJson[Seq[Json]] =
    seqCodec(jsonCodec)

  implicit def jSetCodec:CodecJson[Set[Json]] =
    setCodec(jsonCodec)

  implicit def jVectorCodec:CodecJson[Vector[Json]] =
    vectorCodec(jsonCodec)

  implicit def seqCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Seq[T]](
      new EncodeJson[Seq[T]]{
        def encode(a: Seq[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Seq[T]](_.array.flatMap(_.map(t => codec.decodeJson(t).toOption).sequence[Option, T]), "array")
    )

  implicit def setCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Set[T]](
      new EncodeJson[Set[T]]{
        def encode(a: Set[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Set[T]](_.array.flatMap(_.map(t => codec.decodeJson(t).toOption).sequence[Option, T].map(_.toSet)), "array")
    )

  implicit def vectorCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Vector[T]](
      new EncodeJson[Vector[T]]{
        def encode(a: Vector[T]): Json =
          jArray(a.map(codec.encode).toList)
      },
      optionDecoder[Vector[T]](_.array.flatMap(_.map(t => codec.decodeJson(t).toOption).sequence[Option, T].map(_.toVector)), "array")
    )

  implicit def mapCodec[T](implicit codec:CodecJson[T]) =
    argonaut.CodecJson.derived[Map[String, T]](
      new EncodeJson[Map[String, T]]{
        def encode(a: Map[String, T]): Json =
          a.mapValues(codec.encode).asJson
      },
      optionDecoder[Map[String, T]](_.obj.flatMap(_.toList.map(t => codec.decodeJson(t._2).toOption.map(t._1 -> _)).sequence[Option, (String, T)].map(_.toMap)), "object")
    )
}


object PessimisticCodecs extends PessimisticCodecs
object OptimisticCodecs extends OptimisticCodecs