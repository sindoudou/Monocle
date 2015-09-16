package monocle.std

import cats.laws.discipline.arbitrary._
import cats.std.all._
import monocle.MonocleSuite
import monocle.law.discipline.function.{EachTests, EmptyTests}
import monocle.law.discipline.{IsoTests, PrismTests}

class OptionSpec extends MonocleSuite {
  checkAll("some", PrismTests(some[Int]))
  checkAll("none", PrismTests(none[Long]))
  checkAll("optionToDisjunction",  IsoTests(optionToDisjunction[Int]))
  checkAll("pOptionToDisjunction", IsoTests(pOptionToDisjunction[Int, Int]))

  checkAll("each Option", EachTests[Option[Int], Int])
  checkAll("empty Option",EmptyTests[Option[Int]])
}
