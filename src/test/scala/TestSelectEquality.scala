import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TestSelectEquality extends FunSuite with ShouldMatchers with ParsingInTests {

  test("order of sources is irrelevant") {
    val s1 = parseSelect("select x.a, y.b from xxx x, yyy y")
    val s2 = parseSelect("select x.a, y.b from yyy y, xxx x")

    s1 should be(s2)
  }

  test("aliases for sources are irrelevant") {
    val s1 = parseSelect("select x.a, y.b from xxx x, yyy y")
    val s2 = parseSelect("select y.a, x.b from yyy x, xxx y")

    s1 should be(s2)
  }

  test("order of selectors does matter") {
    val s1 = parseSelect("select x.a, y.b from xxx x, yyy y")
    val s2 = parseSelect("select y.b, x.a from xxx x, yyy y")

    s1 should not(be(s2))
  }

  test("order of constraints does mot matter") {
    val s1 = parseSelect("select x from y where a = 1 and b = 2")
    val s2 = parseSelect("select x from y where b = 2 and a = 1")

    s1 should be(s2)
  }

  test("order of set constraint values does not matter") {
    val s1 = parseSelect("select x from y where a in (1,2,3)")
    val s2 = parseSelect("select x from y where a in (3,1,2)")

    s1 should be(s2)
  }

}
