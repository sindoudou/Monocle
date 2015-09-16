package monocle.interopscalaz

import cats.std.int._
import cats.std.string._
import monocle.MonocleSuite
import monocle.law.discipline.function._

import scalaz.IMap
import scalaz.std.anyVal._

class IMapSpec extends MonocleSuite {

  checkAll("at IMap", AtTests.defaultIntIndex[IMap[Int, String], String])
  checkAll("each IMap", EachTests[IMap[Int, String], String])
  checkAll("empty IMap", EmptyTests[IMap[Int, String]])
  checkAll("filterIndex IMap", FilterIndexTests.evenIndex[IMap[Int, Char], Char])
  checkAll("index IMap", IndexTests.defaultIntIndex[IMap[Int, String], String])

}
