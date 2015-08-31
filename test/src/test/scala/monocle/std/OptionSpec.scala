package monocle.std

import cats.laws.discipline.arbitrary._
import cats.std.all._
import monocle.MonocleSuite
import monocle.law.discipline.function.{EachTests, EmptyTests}
import monocle.law.discipline.{IsoTests, PrismTests}

class OptionSpec extends MonocleSuite {

  checkAll("some", PrismTests(monocle.std.some[Int]))
  checkAll("none", PrismTests(monocle.std.none[Long]))

  checkAll("each Option", EachTests[Option[Int], Int])
  checkAll("empty Option",EmptyTests[Option[Int]])

  checkAll("optionToDisjunction",  IsoTests(monocle.std.option.optionToDisjunction[Int]))
  checkAll("pOptionToDisjunction", IsoTests(monocle.std.option.pOptionToDisjunction[Int, Int]))
}
