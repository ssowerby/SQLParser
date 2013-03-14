package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

case class Selector( expr: Expr, alias: Option[String] ) extends Expr with Resolvable {
  def buildSQL(builder: SqlBuilder) = builder.build(expr) + alias.fold("")(" as \"" + _ + "\"")

  def resolve(resolver: Resolver) = Selector(resolver(expr), alias)
}

