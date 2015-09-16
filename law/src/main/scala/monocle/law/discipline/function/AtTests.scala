package monocle.law.discipline.function

import cats.Eq
import cats.std.option._
import monocle.function.At._
import monocle.function._
import monocle.law.discipline.LensTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

object AtTests extends Laws {

  def apply[S, I, A](i: I)(implicit aEq: Eq[A], aArb: Arbitrary[A],
                                    sEq: Eq[S], sArb: Arbitrary[S],
                                   evAt: At[S, I, A]): RuleSet =
    new SimpleRuleSet("At", LensTests(at(i)).props: _*)

  def defaultIntIndex[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                                     sEq: Eq[S], sArb: Arbitrary[S],
                                    evAt: At[S, Int, A]): RuleSet =
    apply[S, Int, A](2)

}