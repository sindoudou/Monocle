package monocle

import cats.arrow.{Category, Choice, Compose}
import cats.data.Xor
import cats.std.list._
import cats.std.string._
import cats.std.int._

class FoldSpec extends MonocleSuite {

  val listFold = Fold.fromFoldable[List, Int]

  test("foldMap") {
    listFold.foldMap(_.toString)(List(1,2,3,4,5)) shouldEqual "12345"
  }

  test("headMaybe") {
    listFold.headOption(List(1,2,3,4,5)) shouldEqual Some(1)
    listFold.headOption(Nil)           shouldEqual None
  }

  test("exist") {
    listFold.exist(_ % 2 == 0)(List(1,2,3)) shouldEqual true
    listFold.exist(_ == 7)(List(1,2,3))     shouldEqual false
  }

  test("all") {
    listFold.all(_ % 2 == 0)(List(1,2,3)) shouldEqual false
    listFold.all(_ <= 7)(List(1,2,3))     shouldEqual true
  }

  test("length") {
    listFold.length(List(1,2,3,4,5)) shouldEqual 5
    listFold.length(Nil)             shouldEqual 0
  }

  def nestedListFold[A] = Fold.fromFoldable[List, List[A]]

  // test implicit resolution of type classes

  test("Fold has a Compose instance") {
    Compose[Fold].compose(listFold, nestedListFold[Int]).fold(List(List(1,2,3), List(4,5), List(6))) shouldEqual 21
  }

  test("Fold has a Category instance") {
    Category[Fold].id[Int].fold(3) shouldEqual 3
  }

  test("Fold has a Choice instance") {
    Choice[Fold].choice(listFold, Choice[Fold].id[Int]).fold(Xor.left(List(1,2,3))) shouldEqual 6
  }

}