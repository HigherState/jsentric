package jsentric

import argonaut._
import scala.util.Try

case class Path(segments:Segments) extends AnyVal {
  def \(part:String) = Path(segments :+ Left(part))
  def \(part:Int) = Path(segments :+ Right(part))
  def ++(path:Path) = Path(segments ++ path.segments)

  def hasSubPath(path:Path) =
    path.segments.zip(segments).foldLeft(true) {
      case (a, (s, p)) =>  a && s == p
    }

  //TODO handle chars \ " etc
  override def toString =
    segments.map(_.merge).mkString("\\")

}

object Path extends PathExt {
  type Mix = Int with String
  val empty = Path(Vector.empty)

  def apply[T >: Mix](s:T*):Path =
    Path(
      s.collect {
        case i:Int => Right(i)
        case s:String => Left(s)
      }.toVector
    )

  //TODO handle chars \ " etc
  def fromString(s:String):Path =
    Path(s.split('\\').map{s =>
      Try(s.toInt).map(Right(_)).getOrElse(Left(s))
    }.toVector)
}
trait PathExt extends Functions {

  implicit def jPath(json:Json) =
    new JPath(json)

  implicit def JMaybePath(json:Option[Json]) =
    new JMaybePath(json)
}

class JPath(val json:Json) extends AnyVal with Functions {
  def \(key:String):Option[Json] =
    json.field(key)

  def \(key:Int):Option[Json] =
    json.array.flatMap(_.lift(key))

  def \(path:Path):Option[Json] =
    getValue(json, path.segments)
}

class JMaybePath(val json:Option[Json]) extends AnyVal with Functions {
  def \(key:String):Option[Json] =
    json.flatMap(_.field(key))
  def \(key:Int):Option[Json] =
    json.flatMap(_.array.flatMap(_.lift(key)))
  def \(path:Path):Option[Json] =
    json.flatMap(j => getValue(j, path.segments))
}
