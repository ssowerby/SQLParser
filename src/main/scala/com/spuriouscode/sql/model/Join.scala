package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder
import SqlBuilder._

case class Join ( joinType: Option[String], source: Source, constraint: Option[Constraint] ) extends Node with Resolvable {
  def buildSQL(builder: SqlBuilder) = joinType.map(prefixWith(" ")).getOrElse("") +
                                      " join " +
                                      builder.build(source) +
                                      builder.optionally(constraint)(prefixWith(" on "))

  def resolve( resolver: Resolver ) = Join(joinType, source, constraint.map { resolver(_) })
}

case class JoinedSource( source: Source, joins: List[Join] ) extends Node with Resolvable {
  def buildSQL(builder: SqlBuilder) = builder.build(source) + builder.list(joins, " ")(s=>s)

  def resolve(resolver: Resolver) = JoinedSource(resolver(source), joins.map { resolver(_) })
}

object JoinedSource {

  implicit def sourceToJoinedSource( source: Source ) = JoinedSource(source, List())

}