package monocle.interopscalaz

import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.{IsoTests, PrismTests}

class ValidationSpec extends MonocleSuite {
  checkAll("Validation is isomorphic to Disjunction", IsoTests(validation.validationToDisjunction[String, Int]))
  checkAll("success", PrismTests(validation.success[String, Int]))
  checkAll("failure", PrismTests(validation.failure[String, Int]))
}
