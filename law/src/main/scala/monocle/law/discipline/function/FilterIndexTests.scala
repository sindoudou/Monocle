package monocle.law.discipline.function

import cats.Eq
import monocle.function._
import monocle.law.discipline.TraversalTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws


object FilterIndexTests extends Laws {

  def apply[S, I, A](p: I => Boolean)(implicit aEq: Eq[A], aArb: Arbitrary[A],
                                               sEq: Eq[S], sArb: Arbitrary[S],
                                     evFilterIndex: FilterIndex[S, I, A]): RuleSet =
    new SimpleRuleSet("FilterIndex", TraversalTests(filterIndex(p)).props: _*)

  def evenIndex[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                               sEq: Eq[S], sArb: Arbitrary[S],
                      evFilterIndex: FilterIndex[S, Int, A]): RuleSet =
    apply[S, Int, A](_ % 2 == 0)


}