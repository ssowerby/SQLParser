package com.spuriouscode.sql

import model.Node


class SqlBuilder {

  def build( node: Node ) : String = node.buildSQL(this)

  def optionally( optNode: Option[Node] )( code: String=>String ) : String =
    optNode.map(code compose build).getOrElse("")

  def list( nodes: Seq[Node], sep: String )( code: String => String ) : String =
    if (nodes.isEmpty) "" else code(nodes.map(build).mkString(sep))

  def binaryExpr( lhs: Node, op: String, rhs: Node ) : String =
    "(" + build(lhs) + " " + op + " " + build(rhs) + ")"
}


object SqlBuilder {

  def prefixWith( prefix:String )( str: String ) = prefix + str

}
