package com.spuriouscode.sql.parser

import util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import com.spuriouscode.sql.model._
import com.spuriouscode.sql.SqlBuilder


trait Parser extends JavaTokenParsers {

 // override val whiteSpace = """[ \t]+""".r

  lazy val literalValue = (wholeNumber | floatingPointNumber | stringLiteral | ("true" | "false")) ^^ Literal.apply
  lazy val prefixedColumn = ident ~ "." ~ ident ^^ { case (prefix ~ dot ~ column) => ParsedColumnReference(Some(prefix), column) }
  lazy val unprefixedColumn = ident ^^ { ParsedColumnReference(None, _) }
  lazy val columnref = prefixedColumn | unprefixedColumn
  lazy val factor = literalValue | columnref
  lazy val term = factor ~ rep(("+"|"-") ~ factor) ^^ fold(BinaryExpr.apply)
  lazy val expr = term ~ rep(("*"|"/") ~ term) ^^ fold(BinaryExpr.apply)
  lazy val unaliasedSelector = expr ^^ { e => Selector(e,None) }
  lazy val alias = ("as".? ~ (stringLiteral|ident) ) ^^ { case (as ~ id ) => id }
  lazy val aliasedSelector = expr ~ alias ^^ { case (e ~ a) => Selector(e, Some(a)) }
  lazy val selector = aliasedSelector | unaliasedSelector
  lazy val unaliasedSource = ident
  lazy val source: Parser[Source] = unaliasedSource ~ alias.? ^^ { case s ~ a => Table(s, a) }
  lazy val relop = "<>|<=|>=|<|>|=".r
  lazy val valueConstraint: Parser[Constraint] = expr ~ relop ~ expr ^^ { case lhs ~ op ~ rhs => RelativeConstraint(lhs, op, rhs) }
  lazy val baseConstraint: Parser[Constraint] = "(" ~> constraint <~ ")" | valueConstraint
  lazy val constraintFactor: Parser[Constraint] = baseConstraint | "not" ~> baseConstraint ^^ NotConstraint.apply
  lazy val constraintTerm: Parser[Constraint] = constraintFactor ~ rep("and" ~ constraintFactor) ^^ fold(ConstraintConjunction.apply)
  lazy val constraint: Parser[Constraint] = constraintTerm ~ rep("or" ~ constraintTerm) ^^ fold(ConstraintConjunction.apply)
  lazy val select = "select " ~> repsep(selector, ",") ~
                      opt("from" ~> rep1sep(source, ","))  ~
                      opt("where" ~> constraint) ^^ { case sels ~ froms ~ where =>
    Select(sels, froms.getOrElse(Nil), where, Nil, None)
  }

  private def fold[T<:Expr]( factory: (T,String,T) => T )( arg: ~[T,List[~[String,T]]]) : T =
    arg._2.foldLeft(arg._1){ case (l,op~r) => factory(l,op,r) }

  val parser = select
}


object Test extends App with Parser {

  val result = parseAll(parser, """select x, y, z + 2 as "z", z "a" from foo, bar as b where x > 2 and y<=3 """)

  println("Result = " + result)

  val sb = new SqlBuilder

  println("SQL = " + sb.build(result.get))
}
