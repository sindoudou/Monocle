package monocle.law.discipline.function

import cats.Eq
import monocle.function.Cons1._
import monocle.function._
import monocle.law.discipline.internal.Tuple2Eq._
import monocle.law.discipline.{IsoTests, LensTests}
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object Cons1Tests extends Laws {

  def apply[S, H, T](implicit hEq: Eq[H], hArb: Arbitrary[H],
                              tEq: Eq[T], tArb: Arbitrary[T],
                              sEq: Eq[S], sArb: Arbitrary[S],
                         evCons1: Cons1[S, H, T]): RuleSet =
    new SimpleRuleSet("Cons1",
      IsoTests(cons1[S, H, T]).props ++
      LensTests(head[S, H, T]).props ++
      LensTests(tail[S, H, T]).props: _*)
}