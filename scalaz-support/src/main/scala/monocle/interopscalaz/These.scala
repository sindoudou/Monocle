package monocle.interopscalaz

import cats.data.Ior
import monocle.{Iso, PIso, Prism}

import scalaz.\&/.{Both, That, This}
import scalaz.syntax.either._
import scalaz.{-\/, \&/, \/, \/-}

object these extends TheseOptics

trait TheseOptics {

  def pTheseToIor[E1, E2, A1, A2]: PIso[E1 \&/ A1, E2 \&/ A2, E1 Ior A1, E2 Ior A2] =
    PIso[E1 \&/ A1, E2 \&/ A2, E1 Ior A1, E2 Ior A2](
      _.fold(Ior.left, Ior.right, Ior.both)
    )(_.fold(This(_), That(_), Both(_, _)))

  def theseToIor[E, A]: Iso[E \&/ A, E Ior A] =
    pTheseToIor[E, E, A, A]

  def theseToDisjunction[A, B]: Prism[A \&/ B, A \/ B] = Prism[A \&/ B, A \/ B]{
    case This(a) => Some(a.left[B])
    case That(b) => Some(b.right[A])
    case Both(_, _) => None
  }{
    case -\/(a) => This(a)
    case \/-(b) => That(b)
  }
}
