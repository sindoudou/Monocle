package monocle.interopscalaz

import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.PrismTests

class Either3Spec extends MonocleSuite {
  checkAll("left3"  , PrismTests(left3[String, Int, Char]))
  checkAll("middle3", PrismTests(middle3[String, Int, Char]))
  checkAll("right3" , PrismTests(right3[String, Int, Char]))
}
