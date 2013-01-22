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

}
