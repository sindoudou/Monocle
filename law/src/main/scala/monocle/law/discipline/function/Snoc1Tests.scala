package monocle.law.discipline.function

import cats.Eq
import monocle.function.Snoc1._
import monocle.function._
import monocle.law.discipline.internal.Tuple2Eq._
import monocle.law.discipline.{IsoTests, LensTests}
import monocle.law.discipline.{IsoTests, LensTests}
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object Snoc1Tests extends Laws {

  def apply[S, I, L](implicit iEq: Eq[I], iArb: Arbitrary[I],
                              lEq: Eq[L], lArb: Arbitrary[L],
                              sEq: Eq[S], sArb: Arbitrary[S],
                         evSnoc1: Snoc1[S, I, L]): RuleSet =
    new SimpleRuleSet("Snoc1",
      IsoTests(snoc1[S, I, L]).props ++
      LensTests(init[S, I, L]).props ++
      LensTests(last[S, I, L]).props: _*)
}