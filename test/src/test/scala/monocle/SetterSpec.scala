package monocle

import cats.arrow.{Category, Choice, Compose}
import cats.data.Xor
import cats.std.list._

class SetterSpec extends MonocleSuite {

  def all[A]: Setter[List[A], A] = PSetter.fromFunctor[List, A, A]
  def even[A]: Setter[List[A], A] = filterIndex[List[A], Int, A](_ % 2 == 0).asSetter

  // test implicit resolution of type classes

  test("Setter has a Compose instance") {
    Compose[Setter].compose(all[Int], all[List[Int]]).set(3)(List(List(1,2,3), List(4))) shouldEqual List(List(3,3,3), List(3))
  }

  test("Setter has a Category instance") {
    Category[Setter].id[Int].modify(_ + 1)(3) shouldEqual 4
  }

  test("Setter has a Choice instance") {
    Choice[Setter].choice(all[Int], even[Int]).modify(_ + 1)(Xor.right(List(1,2,3,4))) shouldEqual Xor.right(List(2,2,4,4))
  }

}