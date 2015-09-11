package jsentric.queryTree

import argonaut.Json
import jsentric.{Query, Path}

import scala.util.matching.Regex

sealed trait Tree {
  def isMatch(json:Json) =
    Query.apply(json, this)
}
final case class ?(path:Path, op:String, value:Json) extends Tree
final case class ∃(path:Path, tree:Tree) extends Tree
final case class /(path:Path, regex:Regex) extends Tree
final case class %(path:Path, like:String, regex:Regex) extends Tree
final case class &(seq:Seq[Tree]) extends Tree
final case class |(seq:Seq[Tree]) extends Tree
final case class !!(tree:Tree) extends Tree
