package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Constraint extends Expr

case class TestConstraint( expr: Expr, test: String ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = wrap(builder, expr) + " " + test
}

case class RelativeConstraint( lhs: Expr, op: String, rhs: Expr ) extends Constraint with Resolvable[Expr] {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + op + " " + wrap(builder, rhs)

  override def resolve(pf: PartialFunction[Expr, Expr]) = RelativeConstraint(pf(lhs),op, pf(rhs))
}

case class SetConstraint( lhs: Expr, op: String, values: Set[Expr] ) extends Constraint with Resolvable[Expr] {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + op + " (" + builder.list(values.toList, ","){ x=>x } + ")"

  def resolve(pf: PartialFunction[Expr, Expr]) = SetConstraint(pf(lhs), op, values.map(pf))
}

case class ConstraintConjunction( lhs: Constraint, conjunction: String, rhs: Constraint ) extends ComplexExpr with Constraint with Resolvable[Constraint] {
  def buildSQL(builder: SqlBuilder) = wrap(builder, lhs) + " " + conjunction + " " + wrap(builder, rhs)

  override def resolve(pf: PartialFunction[Constraint, Constraint]) = ConstraintConjunction(pf(lhs), conjunction, pf(rhs))

  override def hashCode() = lhs.hashCode() | rhs.hashCode() | 31 * conjunction.hashCode

  override def equals(obj: Any) = {
    obj match {
      case cc:ConstraintConjunction =>
        // Conjunction must match but two sides can be either way round
        conjunction == cc.conjunction && (
            (lhs == cc.lhs && rhs == cc.rhs) ||
            (lhs == cc.rhs && rhs == cc.lhs)
          )

      case _ =>
        false
    }
  }
}

case class NotConstraint( constraint: Constraint ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = "not " + builder.build(constraint)
}