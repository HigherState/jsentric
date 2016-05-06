package jsentric.queryTree

import argonaut.Argonaut._
import argonaut.JsonObject
import jsentric.{JArray, JObject, JString, Path}
import scalaz.Scalaz._

object QueryTree {

  def apply(query:JsonObject) = {
    buildTree(query, Path.empty)
  }
  private def buildTree(query:JsonObject, path:Path):Tree = {
    &(query.toList.flatMap{
      case ("$and", JArray(values)) =>
        Some(&(values.flatMap(_.obj.map(buildTree(_, path)))))
      case ("$or", JArray(values)) =>
        Some(|(values.flatMap(_.obj.map(buildTree(_, path)))))
      case ("$not", JObject(value)) =>
        Some(!!(buildTree(value, path)))
      case ("$elemMatch", JObject(value)) =>
        Some(∃(path, buildTree(value, Path.empty)))
      case ("$elemMatch", j) =>
        Some(∃(path, ?(Path.empty, "$eq", j)))
      case (o@("$eq" | "$ne" | "$lt" | "$gt" | "$lte" | "$gte" | "$in" | "$nin" | "$exists"), v) =>
        Some(?(path, o, v))
      case ("$regex", JString(s)) =>
        val options = query("$options").collect{ case JString(o) => s"(?$o)"}.getOrElse("")
        Some(/(path, (options + s).r))
      case ("$like", JString(s)) =>
        Some(%(path, s, ("(?i)" + s.replace("%", ".*")).r))
      case ("$options", _) =>
        None
      case (key, JObject(v)) =>
        Some(buildTree(v, path \ key))
      case (key, j) =>
        Some(?(path \ key, "$eq", j))
    })
  }

  def partition(tree:Tree, paths:Set[Path]):(Option[Tree], Option[Tree]) = {
    tree match {
      case |(trees) =>
        val (l,r) = trees.map(partition(_, paths)).unzip
        if (l.count(_.nonEmpty) < trees.length) //not all elements present in query
          None -> Some(tree)
        else {
          val lm = Some(|(l.flatten))
          if (r.forall(_.isEmpty))
            lm -> None
          else
            lm -> Some(tree)
        }

      case &(trees) =>
        val (l,r) = trees.map(partition(_, paths)).unzip
        val lm = l.flatten
        val rm = r.flatten
        lm.nonEmpty.option(&(lm)) -> rm.nonEmpty.option(&(rm))

      case ?(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case /(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case %(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case ∃(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = negPartition(t, paths)
        l.map(!!) -> r.map(!!)
    }
  }

  private def negPartition(tree:Tree, paths:Set[Path]):(Option[Tree], Option[Tree]) = {
    tree match {
      case |(trees) =>
        val (l,r) = trees.map(negPartition(_, paths)).unzip
        val lm = l.flatten
        val rm = r.flatten
        lm.nonEmpty.option(|(lm)) -> rm.nonEmpty.option(|(rm))

      case &(trees) =>
        val (l,r) = trees.map(negPartition(_, paths)).unzip
        if (l.count(_.nonEmpty) < trees.length) //not all elements present in query
          None -> Some(tree)
        else {
          val lm = Some(&(l.flatten))
          if (r.forall(_.isEmpty))
            lm -> None
          else
            lm -> Some(tree)
        }

      case ?(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case /(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case %(path, _, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case ∃(path, _) =>
        if (paths.exists(path.hasSubPath)) Some(tree) -> None
        else None -> Some(tree)

      case !!(t) =>
        val (l, r) = partition(t, paths)
        l.map(!!) -> r.map(!!)
    }
  }
}


