package monocle.interopscalaz

import cats.data.Xor
import monocle.{PPrism, Prism}

import scalaz.{Either3, Left3, Middle3, Right3}

object either3 extends Either3Optics

trait Either3Optics {

  final def pLeft3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[D, B, C], A, D] =
    PPrism[Either3[A, B, C], Either3[D, B, C], A, D] {
      case Left3(a)   => Xor.right(a)
      case Middle3(b) => Xor.left(Middle3(b))
      case Right3(c)  => Xor.left(Right3(c))
    }(Left3.apply)

  final def left3[A, B, C]: Prism[Either3[A, B, C], A] =
    pLeft3[A, B, C, A]

  final def pMiddle3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[A, D, C], B, D] =
    PPrism[Either3[A, B, C], Either3[A, D, C], B, D] {
      case Left3(a)   => Xor.left(Left3(a))
      case Middle3(b) => Xor.right(b)
      case Right3(c)  => Xor.left(Right3(c))
    }(Middle3.apply)

  final def middle3[A, B, C]: Prism[Either3[A, B, C], B] =
    pMiddle3[A, B, C, B]

  final def pRight3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[A, B, D], C, D] =
    PPrism[Either3[A, B, C], Either3[A, B, D], C, D] {
      case Left3(a)   => Xor.left(Left3(a))
      case Middle3(b) => Xor.left(Middle3(b))
      case Right3(c)  => Xor.right(c)
    }(Right3.apply)

  final def right3[A, B, C]: Prism[Either3[A, B, C], C] =
    pRight3[A, B, C, C]
}
