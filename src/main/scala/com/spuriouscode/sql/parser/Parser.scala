package com.spuriouscode.sql.parser

import util.parsing.combinator.JavaTokenParsers
import com.spuriouscode.sql.model._


class Parser extends JavaTokenParsers  {

  override val whiteSpace = """[ \t]+""".r

  lazy val keyword = "select" | "from" | "order" | "group" | "by" | "where" | "having" | "left" | "right" | "outer" | "full" | "cross" | "inner" | "on"

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
  lazy val directedOuterJoin = ("left"|"right") <~ opt("outer") ^^ { _ + " outer" }
  lazy val fullOuterJoin = ("full" ~ "outer") ^^^ {
    "full outer"
  }
  lazy val joinType = fullOuterJoin | directedOuterJoin | "inner" | "cross" | "outer"
  lazy val join = opt(joinType) ~ "join" ~ source ~ opt("on" ~> constraint) ^^ { case jt ~ _ ~ s ~ c => Join(jt, s, c) }
  lazy val joinedSource = source ~ rep(join) ^^ { case source ~ joins => JoinedSource(source, joins) }
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
                                    opt("from" ~> rep1sep(joinedSource, ","))  ~
                                    opt("where" ~> constraint) ^^ { case sels ~ froms ~ where =>
    val sources = froms.getOrElse(Nil).toSet
    val sourceByAlias = extractAliases(sources.map{_.source}) ++ extractAliases(sources.flatMap{_.joins.map{_.source}})
    val resolvedSources = sources.map(resolve(sourceByAlias))
    val resolvedSelectors = sels.map(resolve(sourceByAlias))
    val resolvedConstraint = where.map(resolve(sourceByAlias))

    Select(resolvedSelectors, resolvedSources, resolvedConstraint, Set.empty, None)
  }

  private def extractAliases( sources: Iterable[Source] ) : Map[String,Source] =
    sources.groupBy{ _.alias }.mapValues{ _.head }


  private def resolve[T]( sourceByAlias: Map[String,Source] )( t: T ) : T = {
    val resolver = new Resolver {

      def apply[E](e: E) : E = {
        val resolved = e match {

          case ref:ParsedColumnReference =>
            val source = ref.source.map { s =>
              sourceByAlias.getOrElse(s, sys.error("Unknown source alias " + s))
            }
            ColumnReference(source, ref.column).asInstanceOf[T]

          case r: Resolvable =>
            r.resolve(this)

          case e =>
            e
        }
        resolved.asInstanceOf[E]
      }
    }

    resolver(t)
  }
}
