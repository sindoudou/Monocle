package monocle.internal

import cats.{MonoidK, Monoid}

final case class First[A](unwrap: Option[A]) extends AnyVal

object First {
  implicit val monoidK: MonoidK[First] = new MonoidK[First] {
    override def empty[A]: First[A] = First(None)
    override def combine[A](x: First[A], y: First[A]): First[A] =
      First(x.unwrap orElse y.unwrap)
  }

  implicit def monoid[A]: Monoid[First[A]] = monoidK.algebra[A]
}
