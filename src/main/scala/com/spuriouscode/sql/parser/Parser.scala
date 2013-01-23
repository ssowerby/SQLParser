package com.spuriouscode.sql.parser

import util.parsing.combinator.JavaTokenParsers
import com.spuriouscode.sql.model._


class Parser extends JavaTokenParsers  {

  override val whiteSpace = """[ \t]+""".r

  lazy val keyword = "select" | "from" | "order" | "group" | "by" | "where" | "having"

  override lazy val ident = not(keyword) ~> super.ident
  lazy val literalValue = (wholeNumber | floatingPointNumber | stringLiteral | ("true" | "false")) ^^ Literal.apply
  lazy val prefixedColumn = ident ~ "." ~ ident ^^ { case (prefix ~ dot ~ column) => ParsedColumnReference(Some(prefix), column) }
  lazy val unprefixedColumn = ident ^^ { ParsedColumnReference(None, _) }
  lazy val columnref = prefixedColumn | unprefixedColumn
  lazy val factor = literalValue | columnref
  lazy val term = coordinatingConjunction(factor, ("+"|"-"), BinaryExpr.apply)
  lazy val expr = coordinatingConjunction(term, ("*"|"/"), BinaryExpr.apply)
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
  lazy val constraintTerm: Parser[Constraint] = coordinatingConjunction(constraintFactor , "and", ConstraintConjunction.apply)
  lazy val constraint: Parser[Constraint] = coordinatingConjunction(constraintTerm, "or", ConstraintConjunction.apply)

  private def coordinatingConjunction[T]( clause: Parser[T], conjunction: Parser[String], builder: (T,String,T) => T  ) : Parser[T] =
    clause ~ rep(conjunction ~ clause) ^^ { arg => arg._2.foldLeft(arg._1){ case (l,op~r) => builder(l,op,r) } }

  lazy val select = "select " ~> repsep(selector, ",") ~
                      opt("from" ~> rep1sep(source, ","))  ~
                      opt("where" ~> constraint) ^^ { case sels ~ froms ~ where =>
    val sources = froms.getOrElse(Nil).toSet
    val sourcesByAlias = sources.filter{ _.alias.isDefined }.groupBy{ _.alias.get }.mapValues{ _.head }
    val resolvedSelectors = sels.map(resolveSelector(sourcesByAlias))
    val resolvedConstraint = where.map(resolveConstraint(sourcesByAlias))

    Select(resolvedSelectors, sources, resolvedConstraint, Set.empty, None)
  }


  private def resolveSelector( sourceByAlias: Map[String,Source] ) :  PartialFunction[Selector,Selector] = {
    case Selector(ref:ParsedColumnReference, selectorAlias) =>
      Selector(resolveReference(sourceByAlias, ref), selectorAlias)

    case s:Selector =>
      s
  }


  private def resolveConstraint( sourceByAlias: Map[String,Source] ) : PartialFunction[Constraint,Constraint] = {
    case RelativeConstraint(ref:ParsedColumnReference, op, rhs) =>
        RelativeConstraint(resolveReference(sourceByAlias, ref), op, rhs)

    case ConstraintConjunction(lhs, op, rhs) =>
      ConstraintConjunction(resolveConstraint(sourceByAlias)(lhs), op, resolveConstraint(sourceByAlias)(rhs))

    case c:Constraint =>
      c
  }


  private def resolveReference( sourceByAlias: Map[String,Source], ref: ParsedColumnReference ) : ColumnReference = {
    val source = ref.source.map { s =>
        sourceByAlias.getOrElse(s, sys.error("Unknown source alias " + s))
    }
    ColumnReference(source, ref.column)
  }

}
