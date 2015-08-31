package monocle.std

import cats.std.byte._
import monocle.Prism
import monocle.function.Index
import monocle.internal.{Bits, Bounded}

object byte extends ByteOptics

trait ByteOptics {

  implicit val byteBitIndex: Index[Byte, Int, Boolean] =
    Bits.bitsIndex[Byte]

  val byteToBoolean: Prism[Byte, Boolean] =
    Bounded.orderingBoundedSafeCast[Byte, Boolean]{
      case 0 => false
      case 1 => true
    }(if(_) 1 else 0)

}
