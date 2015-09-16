package monocle

import cats.Eq
import cats.arrow.{Category, Choice, Compose}
import cats.data.Xor
import cats.std.list._
import cats.std.int._
import monocle.law.discipline.{SetterTests, TraversalTests}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class TraversalSpec extends MonocleSuite {

  case class Location(latitude: Int, longitude: Int, name: String)

  val coordinates: Traversal[Location, Int] = Traversal.apply2[Location, Int](_.latitude, _.longitude) {
    case (newLat, newLong, oldLoc) =>
      oldLoc.copy(latitude = newLat, longitude = newLong)
  }

  def all[A]: Traversal[List[A], A] = PTraversal.fromTraverse[List, A, A]

  implicit val locationGen: Arbitrary[Location] = Arbitrary(for {
    x <- arbitrary[Int]
    y <- arbitrary[Int]
    n <- arbitrary[String]
  } yield Location(x, y, n))

  implicit val exampleEq = Eq.fromUniversalEquals[Location]


  checkAll("apply2 Traversal", TraversalTests(coordinates))
  checkAll("fromTraverse Traversal" , TraversalTests(all[Int]))

  checkAll("traversal.asSetter", SetterTests(coordinates.asSetter))

  test("length") {
    all[Location].length(List(Location(1,2,""), Location(3,4,""))) shouldEqual 2
    all[Location].length(Nil) shouldEqual 0
  }

  // test implicit resolution of type classes

  test("Traversal has a Compose instance") {
    Compose[Traversal].compose(coordinates, all[Location])
      .modify(_ + 1)(List(Location(1,2,""), Location(3,4,""))) shouldEqual List(Location(2,3,""), Location(4,5,""))
  }

  test("Traversal has a Category instance") {
    Category[Traversal].id[Int].getAll(3) shouldEqual List(3)
  }

  test("Traversal has a Choice instance") {
    Choice[Traversal].choice(all[Int], coordinates).modify(_ + 1)(Xor.left(List(1,2,3))) shouldEqual Xor.left(List(2,3,4))
  }

}
