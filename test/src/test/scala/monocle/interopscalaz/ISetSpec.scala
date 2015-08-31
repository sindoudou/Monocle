package monocle.interopscalaz

import cats.std.int._
import cats.std.unit._
import monocle.MonocleSuite
import monocle.law.discipline.function.{AtTests, EmptyTests}

import scalaz.ISet
import scalaz.std.anyVal._

class ISetSpec extends MonocleSuite {

  checkAll("at ISet", AtTests.defaultIntIndex[ISet[Int], Unit])
  checkAll("empty ISet", EmptyTests[ISet[Int]])

}
