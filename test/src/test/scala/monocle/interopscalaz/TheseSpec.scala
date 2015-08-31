package monocle.interopscalaz

import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.PrismTests

class TheseSpec extends MonocleSuite {
  checkAll("These - Disjunction" , PrismTests(theseToDisjunction[Int, String]))
}
