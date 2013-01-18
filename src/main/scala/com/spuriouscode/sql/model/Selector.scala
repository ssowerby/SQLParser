package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

case class Selector( expr: Expr, alias: Option[String] ) extends Expr {
  def buildSQL(builder: SqlBuilder) = builder.build(expr) + alias.fold("")(" as \"" + _ + "\"")
}

