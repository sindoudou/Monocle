package monocle.law.discipline.function

import cats.Eq
import monocle.function.Reverse._
import monocle.function._
import monocle.law.discipline.IsoTests
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object ReverseTests extends Laws {

  def apply[S, A](implicit aEq: Eq[A], aArb: Arbitrary[A],
                           sEq: Eq[S], sArb: Arbitrary[S],
                           evReverse: Reverse[S, A]): RuleSet = new RuleSet {
    override def name: String = "Reverse"
    override def bases: Seq[(String, RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] =
      IsoTests(reverse[S, A]).props
  }

  def apply[S](implicit sEq: Eq[S], sArb: Arbitrary[S], evReverse: Reverse[S, S]): RuleSet =
    apply[S, S]
}