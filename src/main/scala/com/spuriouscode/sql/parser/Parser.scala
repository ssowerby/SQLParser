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
  lazy val factor: Parser[Expr] = ("(" ~> expr <~ ")") | literalValue | columnref
  lazy val term: Parser[Expr] = coordinatingConjunction(factor, ("+"|"-"), BinaryExpr.apply)
  lazy val expr: Parser[Expr] = coordinatingConjunction(term, ("*"|"/"), BinaryExpr.apply)
  lazy val unaliasedSelector = expr ^^ { e => Selector(e,None) }
  lazy val alias = ("as".? ~ (stringLiteral|ident) ) ^^ { case (as ~ id ) => id }
  lazy val aliasedSelector = expr ~ alias ^^ { case (e ~ a) => Selector(e, Some(a)) }
  lazy val selector = aliasedSelector | unaliasedSelector
  lazy val table = ident ~ alias.? ^^ { case s ~ a => Table(s, a) }
  lazy val inlineView = ("(" ~> select <~ ")") ~ alias ^^ { case (sel~a) => InlineView(sel, a) }
  lazy val source: Parser[Source] = table | inlineView
  lazy val relop = "<>|<=|>=|<|>|=".r
  lazy val setConstraint: Parser[Constraint] = expr ~ ("in"|"not in") ~ ("(" ~> rep1sep(expr, ",") <~ ")") ^^ {  case lhs ~ op ~ values => SetConstraint(lhs, op, values.toSet) }
  lazy val valueConstraint: Parser[Constraint] = expr ~ relop ~ expr ^^ { case lhs ~ op ~ rhs => RelativeConstraint(lhs, op, rhs) }
  lazy val baseConstraint: Parser[Constraint] = "(" ~> constraint <~ ")" | valueConstraint | setConstraint
  lazy val constraintFactor: Parser[Constraint] = baseConstraint | "not" ~> baseConstraint ^^ NotConstraint.apply
  lazy val constraintTerm: Parser[Constraint] = coordinatingConjunction(constraintFactor , "and", ConstraintConjunction.apply)
  lazy val constraint: Parser[Constraint] = coordinatingConjunction(constraintTerm, "or", ConstraintConjunction.apply)

  private def coordinatingConjunction[T]( clause: Parser[T], conjunction: Parser[String], builder: (T,String,T) => T  ) : Parser[T] =
    clause ~ rep(conjunction ~ clause) ^^ { arg => arg._2.foldLeft(arg._1){ case (l,op~r) => builder(l,op,r) } }

  lazy val select: Parser[Select] = "select " ~> repsep(selector, ",") ~
                                    opt("from" ~> rep1sep(source, ","))  ~
                                    opt("where" ~> constraint) ^^ { case sels ~ froms ~ where =>
    val sources = froms.getOrElse(Nil).toSet
    val sourceByAlias = sources.filter{ _.alias.isDefined }.groupBy{ _.alias.get }.mapValues{ _.head }
    val resolvedSelectors = sels.map{ sel => Selector(resolve[Expr](sourceByAlias)(sel.expr), sel.alias) }
    val resolvedConstraint = where.map{ c => resolve[Constraint](sourceByAlias)(c) }

    Select(resolvedSelectors, sources, resolvedConstraint, Set.empty, None)
  }

  private def resolve[T<:Expr]( sourceByAlias: Map[String,Source] ) : PartialFunction[T,T] = {
    case ref:ParsedColumnReference =>
      val source = ref.source.map { s =>
          sourceByAlias.getOrElse(s, sys.error("Unknown source alias " + s))
      }
      ColumnReference(source, ref.column).asInstanceOf[T]

    case r: Resolvable[T] =>
      r.resolve(resolve[T](sourceByAlias))

    case e =>
      e
  }

}
