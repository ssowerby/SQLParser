package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Constraint extends Expr

case class TestConstraint( expr: Expr, test: String ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = wrap(builder, expr) + " " + test
}

case class RelativeConstraint( lhs: Expr, op: String, rhs: Expr ) extends Constraint  {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + op + " " + wrap(builder, rhs)
}

case class ListConstraint( lhs: Expr, op: String, values: List[Expr] ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + op + " (" + builder.list(values, ","){ x=>x } + ")"
}

case class ConstraintConjunction( lhs: Constraint, conjunction: String, rhs: Constraint ) extends ComplexExpr with Constraint {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + conjunction + " " + wrap(builder, rhs)
}

case class NotConstraint( constraint: Constraint ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = "not " + builder.build(constraint)
}