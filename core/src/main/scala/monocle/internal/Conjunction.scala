package monocle.internal

import cats.Monoid


final case class Conjunction(unwrap: Boolean) extends AnyVal

object Conjunction {
  implicit val monoid: Monoid[Conjunction] = new Monoid[Conjunction]{
    override def empty: Conjunction = Conjunction(true)
    override def combine(x: Conjunction, y: Conjunction): Conjunction =
      Conjunction(x.unwrap && y.unwrap)
  }
}