package monocle.std

import cats.data.Xor
import monocle.{Iso, PIso, PPrism, Prism}

object either extends EitherOptics

trait EitherOptics {

  final def pStdLeft[A, B, C]: PPrism[Either[A, B], Either[C, B], A, C] =
    PPrism[Either[A, B], Either[C, B], A, C]{
      case Left(a)  => Xor.right(a)
      case Right(b) => Xor.left(Right(b))
    }(Left.apply)

  final def stdLeft[A, B]: Prism[Either[A, B], A] =
    pStdLeft[A, B, A]

  final def pStdRight[A, B, C]: PPrism[Either[A, B], Either[A, C], B, C] =
    PPrism[Either[A, B], Either[A, C], B, C]{
      case Left(a)  => Xor.left(Left(a))
      case Right(b) => Xor.right(b)
    }(Right.apply)

  final def stdRight[A, B]: Prism[Either[A, B], B] =
    pStdRight[A, B, B]

  final def pEitherToXor[E1, E2, A1, A2]: PIso[Either[E1, A1], Either[E2, A2], E1 Xor A1, E2 Xor A2] =
    xor.pXorToEither[E2, E1, A2, A1].reverse

  final def eitherToXor[E, A]: Iso[Either[E, A], E Xor A] =
    pEitherToXor[E, E, A, A]
}
