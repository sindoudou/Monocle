package monocle

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.{LowerCase, UpperCase}
import eu.timepit.refined.numeric.Interval

package object refined {
  type ZeroTo[T] = Int Refined Interval.Closed[W.`0`.T, T]

  type ByteBits = ZeroTo[W.`7`.T]
  type CharBits = ZeroTo[W.`15`.T]
  type IntBits = ZeroTo[W.`31`.T]
  type LongBits = ZeroTo[W.`63`.T]

  type LowerCaseChar = Char Refined LowerCase
  type UpperCaseChar = Char Refined UpperCase

}
