package monocle

import cats.arrow.{Category, Compose}
import cats.data.Xor
import cats.laws.discipline.arbitrary._
import cats.std.int._
import cats.std.string._
import monocle.law.discipline.{OptionalTests, PrismTests, SetterTests, TraversalTests}

class PrismSpec extends MonocleSuite {

  def right[E, A]: Prism[E Xor A, A] = Prism[E Xor A, A](_.toOption)(Xor.right)

  checkAll("apply Prism", PrismTests(right[String, Int]))

  checkAll("prism.asTraversal", OptionalTests(right[String, Int].asOptional))
  checkAll("prism.asTraversal", TraversalTests(right[String, Int].asTraversal))
  checkAll("prism.asSetter"   , SetterTests(right[String, Int].asSetter))

  // test implicit resolution of type classes

  test("Prism has a Compose instance") {
    Compose[Prism].compose(right[String, Int], right[String, String Xor Int]).getOption(Xor.right(Xor.right(3))) shouldEqual Some(3)
  }

  test("Prism has a Category instance") {
    Category[Prism].id[Int].getOption(3) shouldEqual Some(3)
  }


}