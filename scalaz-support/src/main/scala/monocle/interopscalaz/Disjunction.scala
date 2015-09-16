package monocle.interopscalaz

import cats.data.Xor
import monocle.std.xor
import monocle.{Iso, PIso, PPrism, Prism}

import scalaz.{Validation, \/}

object disjunction extends DisjunctionOptics

trait DisjunctionOptics {

  final def pDisjunctionToXor[E1, E2, A1, A2]: PIso[E1 \/ A1, E2 \/ A2, E1 Xor A1, E2 Xor A2] =
    PIso[E1 \/ A1, E2 \/ A2, E1 Xor A1, E2 Xor A2](_.fold(Xor.left, Xor.right))(_.fold(\/.left, \/.right))

  final def disjunctionToXor[E, A]: Iso[E \/ A, E Xor A] =
    pDisjunctionToXor[E, E, A, A]

  final def pLeft[A, B, C]: PPrism[A \/ B, C \/ B, A, C] =
    pDisjunctionToXor composePrism xor.pLeft

  final def left[A, B]: Prism[A \/ B, A] =
    pLeft[A, B, A]

  final def pRight[A, B, C]: PPrism[A \/ B, A \/ C, B, C] =
    pDisjunctionToXor composePrism xor.pRight

  final def right[A, B]: Prism[A \/ B, B] =
    pRight[A, B, B]

  final def pDisjunctionToValidation[E1, E2, A1, A2]: PIso[E1 \/ A1, E2 \/ A2, Validation[E1, A1], Validation[E2, A2]] =
    validation.pValidationToDisjunction[E2, E1, A2, A1].reverse

  final def disjunctionToValidation[E, A]: Iso[E \/ A, Validation[E, A]] =
    pDisjunctionToValidation[E, E, A, A]

  final def pDisjunctionToEither[E1, E2, A1, A2]: PIso[E1 \/ A1, E2 \/ A2, Either[E1, A1], Either[E2, A2]] =
    PIso[E1 \/ A1, E2 \/ A2, Either[E1, A1], Either[E2, A2]](_.toEither)(_.fold(\/.left, \/.right))

  final def disjunctionToEither[E, A]: Iso[E \/ A, Either[E, A]] =
    pDisjunctionToEither[E, E, A, A]
}
