package monocle.law.discipline.function

import cats.Eq
import monocle.function._
import monocle.law.discipline.TraversalTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object EachTests extends Laws {

  def apply[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                           sEq: Eq[S], sArb: Arbitrary[S],
                        evEach: Each[S, A]): RuleSet =
    new SimpleRuleSet("Each", TraversalTests(each[S, A]).props: _*)

}