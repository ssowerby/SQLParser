import com.spuriouscode.sql.model._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestSelectParsing extends FunSuite with ShouldMatchers with ParsingInTests {

  test ("simple select statement with no aliases") {
    val sel = parseSelect("select x from y")
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", None)), None, Set.empty, None))
  }

  test ("simple select statement with a source alias") {
    val sel = parseSelect("select x from y as foo")
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", Some("foo"))), None, Set.empty, None))
  }

  test ("simple select statement with a selector alias") {
    val sel = parseSelect("select x as foo from y")
    sel should be (Select(List(Selector(ColumnReference(None, "x"), Some("foo"))), Set(Table("y", None)), None, Set.empty, None))
  }

  test ("simple select statement with a source alias and an unaliased column reference using the source alias") {
    val sel = parseSelect("select foo.x from y as foo")
    val s1 = Table("y", None)
    sel should be (Select(List(Selector(ColumnReference(Some(s1), "x"), None)), Set(s1), None, Set.empty, None))
  }

  test ("simple select statement with a source alias and an aliased column reference using the source alias") {
    val sel = parseSelect("select foo.x a from y as foo")
    val s1 = Table("y", None)
    sel should be (Select(List(Selector(ColumnReference(Some(s1), "x"), Some("a"))), Set(s1), None, Set.empty, None))
  }

  test ("select statement with a single constraint") {
    val sel = parseSelect("select x from y where z > 0")
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", None)), Some(RelativeConstraint(ColumnReference(None, "z"), ">", Literal("0"))), Set.empty, None))
  }

  test ("select statement with multiple constraints") {
    val sel = parseSelect("select x from y where x < 0 and y > 2 and z = 3")
    val xc = RelativeConstraint(ColumnReference(None, "x"), "<", Literal("0"))
    val yc = RelativeConstraint(ColumnReference(None, "y"), ">", Literal("2"))
    val zc = RelativeConstraint(ColumnReference(None, "z"), "=", Literal("3"))

    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", None)),
      Some(ConstraintConjunction(ConstraintConjunction(xc, "and", yc), "and", zc)),
      Set.empty, None))
  }

  test ("select statement with both AND and OR constraints") {
    val sel = parseSelect("select x from y where x < 0 and y > 2 or z = 3")
    val xc = RelativeConstraint(ColumnReference(None, "x"), "<", Literal("0"))
    val yc = RelativeConstraint(ColumnReference(None, "y"), ">", Literal("2"))
    val zc = RelativeConstraint(ColumnReference(None, "z"), "=", Literal("3"))

    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", None)),
      Some(ConstraintConjunction(ConstraintConjunction(xc, "and", yc), "or", zc)),
      Set.empty, None))
  }

  test ("select statement with a bracketed expression") {
    val sel = parseSelect("select 2+(x*5) from y")
    sel should be (Select(List(Selector(BinaryExpr(Literal("2"), "+", BinaryExpr(ColumnReference(None, "x"), "*", Literal("5"))),None)), Set(Table("y", None)), None, Set.empty, None))
  }

  test ("select statement with column reference in constraint values") {
    val sel = parseSelect("select x from y where z in (1,a+1,2)")
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None)), Set(Table("y", None)),
      Some(SetConstraint(ColumnReference(None, "z"), "in", Set(Literal("1"), Literal("2"), BinaryExpr(ColumnReference(None, "a"), "+", Literal("1"))))),
      Set.empty, None))
  }

  test ("select statement with inline view") {
    val sel = parseSelect("select z.x from (select a x from y) z")
    val t = Table("y", None)
    val v = InlineView(Select(List(Selector(ColumnReference(None, "a"), Some("x"))), Set(t), None, Set.empty, None), "z")
    sel should be (Select(List(Selector(ColumnReference(Some(v), "x"), None)), Set(v), None, Set.empty, None))
  }

  test ("select with left outer join") {
    val sel = parseSelect("select x, z.id from y left outer join z on z.id = y.id")
    val y = Table("y", None)
    val z = Table("z", None)
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None), Selector(ColumnReference(Some(z), "id"), None)),
      Set(JoinedSource(y, List(Join(Some("left outer"), z, Some(RelativeConstraint(ColumnReference(Some(z), "id"), "=", ColumnReference(Some(y), "id"))))))), None, Set.empty, None))
  }

  test ("select with right outer join") {
    val sel = parseSelect("select x, z.id from y right outer join z on z.id = y.id")
    val y = Table("y", None)
    val z = Table("z", None)
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None), Selector(ColumnReference(Some(z), "id"), None)),
      Set(JoinedSource(y, List(Join(Some("right outer"), z, Some(RelativeConstraint(ColumnReference(Some(z), "id"), "=", ColumnReference(Some(y), "id"))))))), None, Set.empty, None))
  }

  test ("select with full outer join") {
    val sel = parseSelect("select x, z.id from y full outer join z on z.id = y.id")
    val y = Table("y", None)
    val z = Table("z", None)
    sel should be (Select(List(Selector(ColumnReference(None, "x"), None), Selector(ColumnReference(Some(z), "id"), None)),
      Set(JoinedSource(y, List(Join(Some("full outer"), z, Some(RelativeConstraint(ColumnReference(Some(z), "id"), "=", ColumnReference(Some(y), "id"))))))), None, Set.empty, None))
  }

}
