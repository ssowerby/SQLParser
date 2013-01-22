package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder
import com.spuriouscode.sql.SqlBuilder._

case class Select (
  selectors: List[Selector],
  sources: Set[Source],
  where: Option[Constraint],
  groupBy: Set[Expr],
  having: Option[Constraint]
) extends Node {

  def buildSQL( builder: SqlBuilder ) =
    builder.list(selectors, ",")(prefixWith("select ")) +
    builder.list(sources.toList, ",")(prefixWith(" from ")) +
    builder.optionally(where)(prefixWith(" where ")) +
    builder.list(groupBy.toList, ",")(prefixWith(" group by ")) +
    builder.optionally(having)(prefixWith(" having "))
}
