package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Source extends Node {
  def alias: String
}

case class Table( name: String, _alias: Option[String] ) extends Source {

  private def autoAlias:String =name.substring(1 + name.lastIndexOf('.'))

  def alias = _alias.getOrElse(autoAlias)

  def buildSQL(builder: SqlBuilder) =
    name + _alias.map { " as " + _ }.getOrElse("")

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


case class InlineView( select: Select, alias: String ) extends Source {
  def buildSQL(builder: SqlBuilder) = builder.build(select) + " as " + alias

  override def hashCode() = select.hashCode()

  override def equals(obj: Any) = {
    obj match {
      case other:InlineView =>
        select.equals(other.select)

      case _ =>
        super.equals(obj)
    }
  }
}
