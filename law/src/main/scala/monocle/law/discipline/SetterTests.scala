package monocle.law.discipline

import cats.Eq
import monocle.Setter
import monocle.law.SetterLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object SetterTests extends Laws {

  def apply[S: Arbitrary : Eq, A: Arbitrary : Eq](setter: Setter[S, A]): RuleSet = {
    val laws: SetterLaws[S, A] = new SetterLaws(setter)
    new SimpleRuleSet("Setter",
      "set idempotent" -> forAll( (s: S, a: A) => laws.setIdempotent(s, a)),
      "modify id = id" -> forAll( (s: S) => laws.modifyIdentity(s))
    )
  }

}
