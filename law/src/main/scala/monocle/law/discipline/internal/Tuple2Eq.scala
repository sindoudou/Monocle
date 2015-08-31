package monocle.law.discipline.internal

import algebra.Eq

object Tuple2Eq {

  implicit def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] = new Eq[(A, B)] {
    override def eqv(x: (A, B), y: (A, B)): Boolean =
      A.eqv(x._1, y._1) && B.eqv(x._2, y._2)
  }

}
