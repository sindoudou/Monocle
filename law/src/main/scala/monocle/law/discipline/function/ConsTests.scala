package monocle.law.discipline.function

import cats.Eq
import monocle.function.Cons._
import monocle.function._
import monocle.law.discipline.internal.Tuple2Eq._
import monocle.law.discipline.{OptionalTests, PrismTests}
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object ConsTests extends Laws {

  def apply[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                           sEq: Eq[S], sArb: Arbitrary[S],
                           evCons: Cons[S, A]): RuleSet =
    new SimpleRuleSet("Cons",
      PrismTests(cons[S, A]).props ++
      OptionalTests(headOption[S, A]).props ++
      OptionalTests(tailOption[S, A]).props: _*)

}