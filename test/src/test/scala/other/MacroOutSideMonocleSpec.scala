package other

import cats.Eq
import cats.std.all._
import monocle.MonocleSuite
import monocle.law.discipline.{IsoTests, LensTests, PrismTests}
import monocle.macros.{GenIso, GenLens, GenPrism}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

class MacroOutSideMonocleSpec extends MonocleSuite {

  case class Example(i: Int)
  case object ExampleObject
  case class EmptyCase()
  case class EmptyCaseType[A]()

  sealed trait Foo
  case class Bar1(s: String) extends Foo
  case class Bar2(i: Int) extends Foo

  implicit val exampleArb: Arbitrary[Example] = Arbitrary(arbitrary[Int].map(Example.apply))
  implicit val exampleObjArb: Arbitrary[ExampleObject.type] = Arbitrary(Gen.const(ExampleObject))
  implicit val emptyCaseArb: Arbitrary[EmptyCase] = Arbitrary(Gen.const(EmptyCase()))
  implicit def emptyCaseTypeArb[A]: Arbitrary[EmptyCaseType[A]] = Arbitrary(Gen.const(EmptyCaseType()))
  implicit val bar1Arb: Arbitrary[Bar1] = Arbitrary(arbitrary[String].map(Bar1.apply))
  implicit val bar2Arb: Arbitrary[Bar2] = Arbitrary(arbitrary[Int].map(Bar2.apply))
  implicit val fooArb: Arbitrary[Foo] = Arbitrary(Gen.oneOf(arbitrary[Bar1], arbitrary[Bar2]))

  implicit val exampleEq: Eq[Example] = Eq.fromUniversalEquals[Example]
  implicit val exampleObjEq: Eq[ExampleObject.type] = Eq.fromUniversalEquals[ExampleObject.type]
  implicit val emptyCaseEq: Eq[EmptyCase] = Eq.fromUniversalEquals[EmptyCase]
  implicit def emptyCaseTypeEq[A]: Eq[EmptyCaseType[A]] = Eq.fromUniversalEquals[EmptyCaseType[A]]
  implicit val bar1Eq: Eq[Bar1] = Eq.fromUniversalEquals[Bar1]
  implicit val fooEq: Eq[Foo] = Eq.fromUniversalEquals[Foo]

  checkAll("GenIso"                                       , IsoTests(GenIso[Example, Int]))
  checkAll("GenIso.unit object"                           , IsoTests(GenIso.unit[ExampleObject.type]))
  checkAll("GenIso.unit empty case class"                 , IsoTests(GenIso.unit[EmptyCase]))
  checkAll("GenIso.unit empty case class with type param" , IsoTests(GenIso.unit[EmptyCaseType[Int]]))
  checkAll("GenLens"                                      , LensTests(GenLens[Example](_.i)))
  checkAll("GenPrism"                                     , PrismTests(GenPrism[Foo, Bar1]))
}
