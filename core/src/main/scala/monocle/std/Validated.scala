package monocle.std

import cats.data.{Validated, Xor}
import monocle.{Iso, PIso, PPrism, Prism}

object validated extends ValidatedOptics

trait ValidatedOptics {
  final def pValid[E, A, B]: PPrism[Validated[E, A], Validated[E, B], A, B] =
    PPrism[Validated[E, A], Validated[E, B], A, B](
      _.fold(e => Xor.left(Validated.invalid[E, B](e)), Xor.right)
    )(Validated.valid)

  final def valid[E, A]: Prism[Validated[E, A], A] =
    pValid[E, A, A]

  final def pInvalid[E, A, F]: PPrism[Validated[E, A], Validated[F, A], E, F] =
    PPrism[Validated[E, A], Validated[F, A], E, F](
      _.fold(Xor.right, a => Xor.left(Validated.valid[F, A](a)))
    )(Validated.invalid)

  final def invalid[E, A]: Prism[Validated[E, A], E] =
    pInvalid[E, A, E]

  final def pValidatedToXor[E1, E2, A1, A2]: PIso[Validated[E1, A1], Validated[E2, A2], E1 Xor A1, E2 Xor A2] =
    PIso[Validated[E1, A1], Validated[E2, A2], E1 Xor A1, E2 Xor A2](_.toXor)(_.toValidated)

  final def validatedToXor[E, A]: Iso[Validated[E, A], E Xor A] =
    pValidatedToXor[E, E, A, A]
}
