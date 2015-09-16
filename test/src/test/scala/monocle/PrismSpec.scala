package monocle

import cats.arrow.{Category, Compose}
import cats.data.Xor
import cats.laws.discipline.arbitrary._
import cats.std.int._
import cats.std.list._
import cats.std.string._
import monocle.law.discipline.{OptionalTests, PrismTests, SetterTests, TraversalTests}

class PrismSpec extends MonocleSuite {

  def _right[E, A]: Prism[E Xor A, A] = Prism[E Xor A, A](_.toOption)(Xor.right)

  checkAll("apply Prism", PrismTests(_right[String, Int]))

  checkAll("prism.asTraversal", OptionalTests(_right[String, Int].asOptional))
  checkAll("prism.asTraversal", TraversalTests(_right[String, Int].asTraversal))
  checkAll("prism.asSetter"   , SetterTests(_right[String, Int].asSetter))

  // test implicit resolution of type classes

  test("Prism has a Compose instance") {
    Compose[Prism].compose(_right[String, Int], _right[String, String Xor Int]).getOption(Xor.right(Xor.right(3))) shouldEqual Some(3)
  }

  test("Prism has a Category instance") {
    Category[Prism].id[Int].getOption(3) shouldEqual Some(3)
  }

  test("only") {
    Prism.only(5).getOption(5) shouldEqual Some(())
  }

  test("below") {
    val _5s = Prism.only(5).below[List]
    _5s.getOption(List(1,2,3,4,5)) shouldEqual None
    _5s.getOption(List(5,5,5))     shouldEqual Some(List((), (), ()))
  }

}