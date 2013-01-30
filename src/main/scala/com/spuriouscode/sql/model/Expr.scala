package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Expr extends Node {

  protected def wrap( builder: SqlBuilder, expr: Expr ) : String = expr match {
    case c:ComplexExpr =>
      "(" + builder.build(c) + ")"
    case e:Expr =>
      builder.build(e)
  }

}


case class Literal( value: String ) extends Expr {
  def buildSQL(builder: SqlBuilder) = value
}

trait Ref extends Expr
trait ComplexExpr extends Expr

case class ParsedColumnReference( source: Option[String], column: String ) extends Ref  {
  def buildSQL(builder: SqlBuilder) = source.map { _ + "." }.getOrElse("") + column
}

case class ColumnReference( source: Option[Source], column: String ) extends Ref  {
  def buildSQL(builder: SqlBuilder) = source match {
    case None =>
      column
    case Some(s) =>
      builder.build(s) + "." + column
  }
}

case class BinaryExpr( lhs: Expr, op: String, rhs: Expr ) extends ComplexExpr with Resolvable[Expr] {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + op + " " + wrap(builder, rhs)

  override def resolve(pf: PartialFunction[Expr, Expr]) = BinaryExpr(pf(lhs), op, pf(rhs))
}
