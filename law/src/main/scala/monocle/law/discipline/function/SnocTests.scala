package monocle.law.discipline.function

import cats.Eq
import monocle.function.Snoc._
import monocle.function._
import monocle.law.discipline.internal.Tuple2Eq._
import monocle.law.discipline.{OptionalTests, PrismTests}
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object SnocTests extends Laws {

  def apply[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                           sEq: Eq[S], sArb: Arbitrary[S],
                           evSnoc: Snoc[S, A]): RuleSet =
    new SimpleRuleSet("Snoc",
      PrismTests(snoc[S, A]).props ++
      OptionalTests(lastOption[S, A]).props ++
      OptionalTests(initOption[S, A]).props: _*)
}