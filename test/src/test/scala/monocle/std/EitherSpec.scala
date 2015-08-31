package monocle.std

import cats.std.either._
import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.PrismTests

class EitherSpec extends MonocleSuite {
  checkAll("either left" , PrismTests(stdLeft[String, Int]))
  checkAll("either right", PrismTests(stdRight[String, String]))
}
