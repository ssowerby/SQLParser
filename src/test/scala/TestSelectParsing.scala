import com.spuriouscode.sql.model._
import org.scalatest.{FunSuite, FunSpec}
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

}
