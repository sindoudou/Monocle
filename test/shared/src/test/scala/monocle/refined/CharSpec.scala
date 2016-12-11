package monocle.refined

import eu.timepit.refined.api.Refined
import monocle._
import monocle.law.discipline.PrismTests
import monocle.refined.internal.Chars
import org.scalacheck.{Arbitrary, Gen}

import scalaz.Equal

class CharSpec extends MonocleSuite {
  val lowerCaseCharacter = 'a'

  implicit val lowerCaseCharArb: Arbitrary[Char] = Arbitrary(
    Gen.const(lowerCaseCharacter)
  )

  implicit val lowerCaseRefinedCharArb: Arbitrary[LowerCaseChar] = Arbitrary(
    Gen.const(Refined.unsafeApply(lowerCaseCharacter))
  )

  implicit val eqA: Equal[LowerCaseChar] = Equal.equalA[LowerCaseChar]
  implicit val eqS: Equal[Char] = Equal.equalA[Char]


  checkAll("lower cases", PrismTests(toCase[LowerCaseChar](Chars.lowerCaseChar)))
}
