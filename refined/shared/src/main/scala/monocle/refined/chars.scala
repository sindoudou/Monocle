package monocle.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.{LowerCase, UpperCase}
import monocle._
import monocle.refined.internal.Chars


object chars extends CharsInstances

trait CharsInstances {
  implicit val lowerCase: Prism[Char, LowerCaseChar] = toCase[LowerCase](Chars.lowerCaseChar)
  implicit val upperCase: Prism[Char, UpperCaseChar] = toCase[UpperCase](Chars.upperCaseChar)

  def toCase[P](implicit C: Chars): Prism[Char, Refined[Char, P]] = {
    Prism[Char, Refined[Char, P]] {
      case char if C.testChar(char) => Some(Refined.unsafeApply(char))
      case _ => None
    } {_.get}
  }
}

