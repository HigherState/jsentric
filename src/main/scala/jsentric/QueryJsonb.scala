package jsentric

import argonaut._
import Argonaut._
import scalaz._
import Scalaz._

/*
Experimental feature for converting from mongo db style query to a PostGres jsonb query
Uses the jdbc ?? escape for ?
 */
object QueryJsonb {

  type JbValid = NonEmptyList[(String, Path)] \/ String


  def apply(field:String, query:Json): JbValid =
    query.obj.fold[JbValid](\/-(s"Value = ${escape(query.toString())}::jsonb")){ j =>
      treeToPostgres(field)(buildTree(j, Path.empty), false).map(_.mkString)
    }


  private def buildTree(query:JsonObject, path:Path):Tree = {
    &(query.toList.map{
      case ("$and", JArray(values)) =>
        &(values.flatMap(_.obj.map(buildTree(_, path))))
      case ("$or", JArray(values)) =>
        |(values.flatMap(_.obj.map(buildTree(_, path))))
      case ("$not", JObject(value)) =>
        !!(buildTree(value, path))
      case ("$elemMatch", JObject(value)) =>
        ∃(path, buildTree(value, Path.empty))
      case ("$elemMatch", j) =>
        ∃(path, ?(Path.empty, "$eq", j))
      case (o@("$eq" | "$ne" | "$lt" | "$gt" | "$lte" | "$gte" | "$in" | "$nin" | "$exists"), v) =>
        ?(path, o, v)
      case (key, JObject(v)) =>
        buildTree(v, path \ key)
      case (key, j) =>
        ?(path \ key, "$eq", j)
    }.toSeq)
  }

  private def treeToPostgres(field:String):Function[(Tree, Boolean), NonEmptyList[(String, Path)] \/ Vector[String]] = {
    case (&(Seq(value)), g) =>
      treeToPostgres(field)(value, false).map(_ ++ g.option(")"))
    case (|(Seq(value)), g) =>
      treeToPostgres(field)(value, false).map(_ ++ g.option(")"))
    case (&(head +: tail), g) =>
      builder(treeToPostgres(field)(head, false), treeToPostgres(field)(&(tail), true)) { (h, t) =>
        ((!g).option("(") ++: h :+ " AND ") ++ t
      }
    case (|(head +: tail), g) =>
      builder(treeToPostgres(field)(head, false), treeToPostgres(field)(&(tail), true)) { (h, t) =>
        ((!g).option("(") ++: h :+ " OR ") ++ t
      }
    case (!!(tree), g) =>
      treeToPostgres(field)(tree, false).map(v => "NOT (" +: v :+ ")")
    //TODO empty Path
    case (?(path, "$eq", value), _) =>
      \/-(field +: " @> '" +: toObject(path.segments, value) :+ "'::jsonb")
    case (?(path, "$ne", value), _) =>
      \/-("NOT " +: field +: " @> '" +: toObject(path.segments, value) :+ "'::jsonb")
    case (?(path, "$in", value), _) if value.isArray =>
      \/-(Vector(field, " #> '", toPath(path), "' <@ '", escape(value.toString()), "'::jsonb"))
    case (?(path, "$nin", value), _) if value.isArray =>
      \/-(Vector("NOT ", field, " #> '", toPath(path), "' <@ '", escape(value.toString()), "'::jsonb"))
    case (?(path, o@("$nin" | "$in"), _), _) =>
      -\/(NonEmptyList(s"Operation $o expected an array" -> path))
    case (?(path, "$exists", JBool(true)), _) =>
      \/-(field +: toSearch(path.segments))
    case (?(path, "$exists", JBool(false)), _) =>
      \/-("NOT " +: field +: toSearch(path.segments))
    case (?(path, "$exists", _), _) =>
      -\/(NonEmptyList("Operation $exists requires a boolean" -> path))
    //TODO resolve duplicate jsonb_typeOfs which can occur in ands
    case (?(path, Op(op), value), _) =>
      val p = toPath(path)
      //TODO: use applicative builder..
      for {
        v <- serialize(value -> path)
        c <- getCast(value -> path)
        t <- getType(value -> path)
      } yield Vector(
        "(",
        s"jsonb_typeof($field #> '$p') = '$t'",
        " AND ",
        s"($field #>> '$p') :: $c $op $v",
        s")")

    case (∃(path, ?(Path(Seq()), "$eq", value)), _) =>
      \/-(field +: " @> '" +: toObject(path.segments, jArrayElements(value)) :+ "'::jsonb")
    case (∃(path, _), _) =>
      -\/(NonEmptyList("Currently only equality is supported in element match." -> path))
    case (?(path, op, _), _) =>
      -\/(NonEmptyList(s"Unable to parse query operation $op." -> path))
  }

  private def toObject(segments:Segments, value:Json):Vector[String] =
    segments match {
      case head +: tail =>
        "{\"" +: escape(head) +: "\":" +: toObject(tail, value) :+ "}"
      case _ => Vector(escape(value.toString()))
    }

  private def serialize:Function[(Json, Path), JbValid] = {
    case (JString(s), _) => \/-(escape(s))
    case (JBool(true), _) => \/-("true")
    case (JBool(false), _) => \/-("false")
    case (JLong(l), _) => \/-(l.toString)
    case (JDouble(d), _) => \/-(d.toString)
    case (j, _) if j.isNull => \/-("null")
    case (_, path) =>
      -\/(NonEmptyList("Unsupported type" -> path))
  }

  private def escape(s:Either[String, Int]):String =
    escape(s.merge.toString)
  private def escape(s:String):String =
    s.replace("'","''")
  private def toPath(path:Path) =
    path.segments.map(escape).mkString("{", ",", "}")
  private def toSearch:Function[Segments, Vector[String]] = {
    case tail +: Seq() =>
      Vector(" ?? '", escape(tail), "'")
    case head +: tail =>
      " -> '" +: escape(head) +: "'" +: toSearch(tail)
  }

  private def getType:Function[(Json,Path), JbValid] = {
    case (j,_) if j.isNumber => \/-("number")
    case (j,_) if j.isString => \/-("string")
    case (j,_) if j.isBool => \/-("boolean")
    case (j,_) if j.isObject => \/-("object")
    case (j,_) if j.isArray => \/-("array")
    case (j,_) if j.isNull => \/-("null")
    case (_, path) =>
      -\/(NonEmptyList("Unsupported type" -> path))
  }

  private def getCast:Function[(Json, Path), JbValid] = {
    case (j,_) if j.isNumber => \/-("NUMERIC")
    case (j,_) if j.isString => \/-("TEXT")
    case (j,_) if j.isBool => \/-("BOOLEAN")
    case (_, path) =>
      -\/(NonEmptyList("Unsupported type" -> path))
  }
  object Op {
    def unapply(op: String): Option[String] =
      op match {
        case "$lt" => Some("<")
        case "$lte" => Some("<=")
        case "$gt" => Some(">")
        case "$gte" => Some(">=")
        case _ => None
      }
  }

  private def builder[T](
                          left:NonEmptyList[(String, Path)] \/ Vector[String],
                          right:NonEmptyList[(String, Path)] \/ Vector[String])
                        (f:(Vector[String], Vector[String]) => T):NonEmptyList[(String, Path)] \/ T = {
    (left, right) match {
      case (\/-(l), \/-(r)) => \/-(f(l,r))
      case (-\/(l), -\/(r)) => -\/(l.append(r))
      case (_, -\/(r)) => -\/(r)
      case (-\/(l), _) => -\/(l)
    }
  }

  private trait Tree
  private case class ?(path:Path, op:String, value:Json) extends Tree
  private case class ∃(path:Path, tree:Tree) extends Tree
  private case class &(seq:Seq[Tree]) extends Tree
  private case class |(seq:Seq[Tree]) extends Tree
  private case class !!(tree:Tree) extends Tree
}


