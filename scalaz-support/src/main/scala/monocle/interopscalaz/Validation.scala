package monocle.interopscalaz

import cats.data.Validated
import monocle.std.validated
import monocle.{Iso, PIso, PPrism, Prism}

import scalaz.{Validation, \/}

object validation extends ValidationOptics

trait ValidationOptics {

  final def pValidationToValidated[E1, E2, A1, A2]: PIso[Validation[E1, A1], Validation[E2, A2], Validated[E1, A1], Validated[E2, A2]] =
    PIso[Validation[E1, A1], Validation[E2, A2], Validated[E1, A1], Validated[E2, A2]](
      _.fold(Validated.invalid, Validated.valid))(_.fold(Validation.failure, Validation.success)
    )

  final def validationToValidated[E, A]: Iso[Validation[E, A], Validated[E, A]] =
    pValidationToValidated[E, E, A, A]

  final def pSuccess[E, A, B]: PPrism[Validation[E, A], Validation[E, B], A, B] =
    pValidationToValidated composePrism validated.pValid

  final def success[E, A]: Prism[Validation[E, A], A] =
    pSuccess[E, A, A]

  final def pFailure[E, A, F]: PPrism[Validation[E, A], Validation[F, A], E, F] =
    pValidationToValidated composePrism validated.pInvalid

  final def failure[E, A]: Prism[Validation[E, A], E] =
    pFailure[E, A, E]

  final def pValidationToDisjunction[E1, E2, A1, A2]: PIso[Validation[E1, A1], Validation[E2, A2], E1 \/ A1, E2 \/ A2] =
    PIso[Validation[E1, A1], Validation[E2, A2], E1 \/ A1, E2 \/ A2](_.disjunction)(_.validation)

  final def validationToDisjunction[E, A]: Iso[Validation[E, A], E \/ A] =
    pValidationToDisjunction[E, E, A, A]
}
