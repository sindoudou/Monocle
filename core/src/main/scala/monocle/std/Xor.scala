package monocle.std

import cats.data.{Validated, Xor}
import monocle.{Iso, PIso, PPrism, Prism}

object xor extends XorOptics

trait XorOptics {

  final def pLeft[A, B, C]: PPrism[A Xor B, C Xor B, A, C] =
    PPrism[A Xor B, C Xor B, A, C](_.swap.bimap(Xor.right, identity))(Xor.left)

  final def left[A, B]: Prism[A Xor B, A] =
    pLeft[A, B, A]

  final def pRight[A, B, C]: PPrism[A Xor B, A Xor C, B, C] =
    PPrism[A Xor B, A Xor C, B, C](_.bimap(Xor.left, identity))(Xor.right)

  final def right[A, B]: Prism[A Xor B, B] =
    pRight[A, B, B]

  final def pXorToValidated[E1, E2, A1, A2]: PIso[E1 Xor A1, E2 Xor A2, Validated[E1, A1], Validated[E2, A2]] =
    validated.pValidatedToXor[E2, E1, A2, A1].reverse

  final def xorToValidated[E, A]: Iso[E Xor A, Validated[E, A]] =
    pXorToValidated[E, E, A, A]

  final def pXorToEither[E1, E2, A1, A2]: PIso[E1 Xor A1, E2 Xor A2, Either[E1, A1], Either[E2, A2]] =
    PIso[E1 Xor A1, E2 Xor A2, Either[E1, A1], Either[E2, A2]](_.toEither)(_.fold(Xor.left, Xor.right))

  final def xorToEither[E, A]: Iso[E Xor A, Either[E, A]] =
    pXorToEither[E, E, A, A]

}