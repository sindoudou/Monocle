package monocle.law.discipline

import cats.Eq
import cats.std.list._
import cats.std.option._
import monocle.Traversal
import monocle.law.TraversalLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object TraversalTests extends Laws {

  def apply[S: Arbitrary : Eq, A: Arbitrary : Eq](traversal: Traversal[S, A]): RuleSet = {
    val laws: TraversalLaws[S, A] = new TraversalLaws(traversal)
    new SimpleRuleSet("Traversal",
      "get what you set" -> forAll( (s: S, a: A) => laws.setGetAll(s, a)),
      "set idempotent"   -> forAll( (s: S, a: A) => laws.setIdempotent(s, a)),
      "modify id = id"   -> forAll( (s: S) => laws.modifyIdentity(s)),
      "modifyF Id = Id"  -> forAll( (s: S) => laws.modifyFId(s)),
      "headOption"       -> forAll( (s: S) => laws.headOption(s))
    )
  }

}
