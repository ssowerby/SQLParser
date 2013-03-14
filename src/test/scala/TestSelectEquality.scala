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

  test("order of where clause equals constraint does not matter") {
    val s1 = parseSelect("select x from y where a=b")
    val s2 = parseSelect("select x from y where b=a")

    s1 should be(s2)
    s1.hashCode should be(s2.hashCode)
  }

  test("order of join clause equals constraint does not matter") {
    val s1 = parseSelect("select x from y left outer join z on a=b")
    val s2 = parseSelect("select x from y left outer join z on b=a")

    s1 should be(s2)
    s1.hashCode should be(s2.hashCode)
  }

  test("order of anded where clauses does not matter") {
    val s1 = parseSelect("select x from y where a=b and c=d")
    val s2 = parseSelect("select x from y where c=d and a=b")

    s1 should be(s2)
    s1.hashCode should be(s2.hashCode)
  }

  test("order of anded clauses in join does not matter") {
    val s1 = parseSelect("select x from y left outer join z on a=b and c=d")
    val s2 = parseSelect("select x from y left outer join z on c=d and a=b")

    s1 should be(s2)
    s1.hashCode should be(s2.hashCode)
  }

  test("order of anded where clauses and order of equals operands does not matter") {
     val s1 = parseSelect("select x from y where a=b and c=d")
     val s2 = parseSelect("select x from y where d=c and b=a")

     s1 should be(s2)
     s1.hashCode should be(s2.hashCode)
   }

  test("order of anded clauses in join along with operands in equals constraint does not matter") {
    val s1 = parseSelect("select x from y left outer join z on a=b and c=d")
    val s2 = parseSelect("select x from y left outer join z on d=c and b=a")

    s1 should be(s2)
    s1.hashCode should be(s2.hashCode)
  }

}
