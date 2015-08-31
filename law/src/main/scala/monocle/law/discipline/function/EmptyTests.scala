package monocle.law.discipline.function

import cats.Eq
import cats.std.unit._
import monocle.function._
import monocle.law.discipline.PrismTests
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object EmptyTests extends Laws {

  def apply[S: Arbitrary : Eq : Empty]: RuleSet = new RuleSet {
    override def name: String = "Empty"
    override def bases: Seq[(String, RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] =
      PrismTests(empty[S]).props
  }
}