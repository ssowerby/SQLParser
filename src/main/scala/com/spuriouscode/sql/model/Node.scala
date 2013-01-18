package com.spuriouscode.sql.model

import com.spuriouscode.sql.SqlBuilder

trait Node {
  def buildSQL( builder: SqlBuilder ) : String
}


object Node {


}