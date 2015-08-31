package monocle.interopscalaz

import cats.laws.discipline.arbitrary._
import cats.std.either._
import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.{IsoTests, PrismTests}

class DisjunctionSpec extends MonocleSuite {

  checkAll("disjunction left" , PrismTests(disjunction.left[String, Int]))
  checkAll("disjunction right", PrismTests(disjunction.right[String, Int]))

  checkAll("disjunction to Xor"       , IsoTests(disjunction.disjunctionToXor[String, Int]))
  checkAll("disjunction to Validation", IsoTests(disjunction.disjunctionToValidation[String, Int]))
  checkAll("disjunction to Either"    , IsoTests(disjunction.disjunctionToEither[String, Int]))
}
