package jsentric.queryTree

import argonaut.Json
import jsentric.Path

sealed trait Tree
final case class ?(path:Path, op:String, value:Json) extends Tree
final case class ∃(path:Path, tree:Tree) extends Tree
final case class &(seq:Seq[Tree]) extends Tree
final case class |(seq:Seq[Tree]) extends Tree
final case class !!(tree:Tree) extends Tree