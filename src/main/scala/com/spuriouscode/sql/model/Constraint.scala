package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Constraint extends Expr with Resolvable

case class TestConstraint( expr: Expr, test: String ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = builder.build(expr) + " " + test

  def resolve(resolver: Resolver) = TestConstraint(resolver(expr), test)
}

case class RelativeConstraint( lhs: Expr, op: String, rhs: Expr ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = builder.binaryExpr(lhs, op, rhs)

  override def resolve( resolver: Resolver ) = RelativeConstraint(resolver(lhs),op, resolver(rhs))

  override def hashCode() = if (op == "=") {
    (lhs.hashCode | rhs.hashCode()) * 31 + op.hashCode
  }
  else {
    lhs.hashCode * 31 | op.hashCode *15 | rhs.hashCode()
  }

  override def equals(obj: Any) =
    (op,obj) match {
      case ("=", RelativeConstraint(l, "=", r)) if (l == rhs && r == lhs) =>
        true
      case (o1, RelativeConstraint(l, o2, r)) if (o1 == o2 && l == lhs && r == rhs) =>
        true
      case other =>
        false
    }

}

case class SetConstraint( lhs: Expr, op: String, values: Set[Expr] ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = builder.build(lhs) + " " + op + " (" + builder.list(values.toList, ","){ x=>x } + ")"

  def resolve( resolver: Resolver ) = SetConstraint(resolver(lhs), op, values.map{ resolver(_) })
}

case class ConstraintConjunction( lhs: Constraint, conjunction: String, rhs: Constraint ) extends Expr with Constraint  {
  def buildSQL(builder: SqlBuilder) = "(" + builder.build(lhs) + " " + conjunction + " " + builder.build(rhs) + ")"

  override def resolve( resolver: Resolver ) = ConstraintConjunction(resolver(lhs), conjunction, resolver(rhs))

  override def hashCode() = lhs.hashCode() | rhs.hashCode() | 31 * conjunction.hashCode

  override def equals(obj: Any) = {
    obj match {
      case cc:ConstraintConjunction =>
        // Conjunction must match but two sides can be either way round
        conjunction == cc.conjunction && Set(lhs, rhs) == Set(cc.lhs, cc.rhs)

      case _ =>
        false
    }
  }
}

case class NotConstraint( constraint: Constraint ) extends Constraint {
  def buildSQL(builder: SqlBuilder) = "not " + builder.build(constraint)
  def resolve(resolver: Resolver) = NotConstraint(resolver(constraint))
}