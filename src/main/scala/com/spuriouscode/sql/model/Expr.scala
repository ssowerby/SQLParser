package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Expr extends Node


case class Literal( value: String ) extends Expr {
  def buildSQL(builder: SqlBuilder) = value
}

trait Ref extends Expr

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

case class BinaryExpr( lhs: Expr, op: String, rhs: Expr ) extends Expr with Resolvable {
  def buildSQL(builder: SqlBuilder) = builder.binaryExpr(lhs, op, rhs)

  override def resolve( resolver: Resolver ) = BinaryExpr(resolver(lhs), op, resolver(rhs))
}
