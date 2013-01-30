package com.spuriouscode.sql.model


trait Resolvable[T <: Expr] {

  def resolve( pf: PartialFunction[T,T] ) : T
}
