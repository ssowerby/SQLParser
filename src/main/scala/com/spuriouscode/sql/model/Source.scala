package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Source extends Node {

  def alias: Option[String]

}

case class Table( name: String, alias: Option[String] ) extends Source {
  def buildSQL(builder: SqlBuilder) =
    name + alias.map { " as " + _ }.getOrElse("")

  override def hashCode() = name.hashCode()

  override def equals(obj: Any) = {
    obj match {
      case other:Table =>
        name.equals(other.name)

      case _ =>
        super.equals(obj)
    }
  }
}