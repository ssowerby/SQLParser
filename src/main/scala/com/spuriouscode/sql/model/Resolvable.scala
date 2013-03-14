package com.spuriouscode.sql.model


trait Resolver {
  def apply[E]( e: E ) : E
}

trait Resolvable {
  def resolve( resolver: Resolver ) : Any
}

