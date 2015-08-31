package monocle.std

import cats.std.double._
import cats.std.int._
import monocle.MonocleSuite
import monocle.law.discipline.PrismTests

class DoubleSpec extends MonocleSuite {
  checkAll("Double to Int", PrismTests(doubleToInt))
}
