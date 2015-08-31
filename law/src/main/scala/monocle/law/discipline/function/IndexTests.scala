package monocle.law.discipline.function

import cats.Eq
import monocle.function._
import monocle.law.discipline.OptionalTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws


object IndexTests extends Laws {

  def apply[S, I, A](i: I)(implicit aEq: Eq[A], aArb: Arbitrary[A],
                                    sEq: Eq[S], sArb: Arbitrary[S],
                                evIndex: Index[S, I, A]): RuleSet =
    new SimpleRuleSet("Index", OptionalTests(index(i)).props: _*)

  def defaultIntIndex[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                                     sEq: Eq[S], sArb: Arbitrary[S],
                                 evIndex: Index[S, Int, A]): RuleSet =
    apply[S, Int, A](2)

}