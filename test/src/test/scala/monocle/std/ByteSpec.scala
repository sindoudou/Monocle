package monocle.std

import cats.std.boolean._
import cats.std.byte._
import monocle.MonocleSuite
import monocle.function._
import monocle.law.discipline.{OptionalTests, PrismTests}

class ByteSpec extends MonocleSuite {

  checkAll("Byte index bit", OptionalTests(index[Byte, Int, Boolean](0)))

  checkAll("Byte to Boolean", PrismTests(byteToBoolean))

}
