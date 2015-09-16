package monocle

import cats.std.list._
import cats.syntax.eq._
import cats.syntax.traverse._
import cats.{Applicative, Eq}
import monocle.interopscalaz.typeclass
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.Equality

import scalaz.\&/.{Both, That, This}
import scalaz.{Either3, IList, IMap, ISet, Maybe, NonEmptyList, OneAnd, Tree, Validation, \&/, \/}

trait TestInstances {

  implicit def equality[A](implicit A: Eq[A]): Equality[A] =
    new Equality[A]{
      override def areEqual(a: A, b: Any): Boolean =
        A.eqv(a, b.asInstanceOf[A])
    }

  implicit val genApplicative: Applicative[Gen] = new Applicative[Gen] {
    override def ap[A, B](fa: Gen[A])(f: Gen[A => B]): Gen[B] = fa.flatMap(a => f.map(_(a)))
    override def pure[A](a: A): Gen[A] = Gen.const(a)
  }

  // Equal instances

  implicit def tuple2Eq[A1: Eq, A2: Eq] = new Eq[(A1, A2)] {
    override def eqv(x: (A1, A2), y: (A1, A2)): Boolean =
      x._1 === y._1 && x._2 === y._2
  }
  implicit def tuple3Eq[A1: Eq, A2: Eq, A3: Eq] = new Eq[(A1, A2, A3)] {
    override def eqv(x: (A1, A2, A3), y: (A1, A2, A3)): Boolean =
      x._1 === y._1 && x._2 === y._2 && x._3 === y._3
  }
  implicit def tuple4Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq] = new Eq[(A1, A2, A3, A4)] {
    override def eqv(x: (A1, A2, A3, A4), y: (A1, A2, A3, A4)): Boolean =
      x._1 === y._1 && x._2 === y._2 && x._3 === y._3 && x._4 === y._4
  }
  implicit def tuple5Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq, A5: Eq] = new Eq[(A1, A2, A3, A4, A5)] {
    override def eqv(x: (A1, A2, A3, A4, A5), y: (A1, A2, A3, A4, A5)): Boolean =
      x._1 === y._1 && x._2 === y._2 && x._3 === y._3 && x._4 === y._4 && x._5 === y._5
  }
  implicit def tuple6Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq, A5: Eq, A6: Eq] = new Eq[(A1, A2, A3, A4, A5, A6)] {
    override def eqv(x: (A1, A2, A3, A4, A5, A6), y: (A1, A2, A3, A4, A5, A6)): Boolean =
      x._1 === y._1 && x._2 === y._2 && x._3 === y._3 && x._4 === y._4 && x._5 === y._5 && x._6 === y._6
  }

  implicit def disjunctionEq[E, A](implicit E: Eq[E], A: Eq[A]): Eq[E \/ A] =
    typeclass.eq.get(scalaz.\/.DisjunctionEqual(typeclass.eq.reverseGet(E), typeclass.eq.reverseGet(A)))
  implicit def validationEq[E, A](implicit E: Eq[E], A: Eq[A]): Eq[Validation[E, A]] =
    typeclass.eq.get(scalaz.Validation.ValidationEqual(typeclass.eq.reverseGet(E), typeclass.eq.reverseGet(A)))
  implicit def theseEq[E, A](implicit E: Eq[E], A: Eq[A]): Eq[E \&/ A] =
    typeclass.eq.get(scalaz.\&/.TheseEqual(typeclass.eq.reverseGet(E), typeclass.eq.reverseGet(A)))
  implicit def maybeEq[A](implicit A: Eq[A]): Eq[scalaz.Maybe[A]] =
    typeclass.eq.get(scalaz.Maybe.maybeEqual(typeclass.eq.reverseGet(A)))
  implicit def iListEq[A](implicit A: Eq[A]): Eq[scalaz.IList[A]] =
    typeclass.eq.get(scalaz.IList.equal(typeclass.eq.reverseGet(A)))
  implicit def netEq[A](implicit A: Eq[A]): Eq[scalaz.NonEmptyList[A]] =
    typeclass.eq.get(scalaz.NonEmptyList.nonEmptyListEqual(typeclass.eq.reverseGet(A)))
  implicit def iSetEq[A](implicit A: Eq[A]): Eq[scalaz.ISet[A]] =
    typeclass.eq.get(scalaz.ISet.setEqual(typeclass.eq.reverseGet(A)))
  implicit def oneAndEq[F[_], A](implicit A: Eq[A], FA: Eq[F[A]]): Eq[scalaz.OneAnd[F, A]] =
    typeclass.eq.get(scalaz.OneAnd.oneAndEqual(typeclass.eq.reverseGet(A), typeclass.eq.reverseGet(FA)))
  implicit def imapEq[K, V](implicit K: Eq[K], V: Eq[V]): Eq[scalaz.IMap[K, V]] =
    typeclass.eq.get(scalaz.IMap.mapEqual(typeclass.eq.reverseGet(K), typeclass.eq.reverseGet(V)))
  implicit def treeEq[A](implicit A: Eq[A]): Eq[scalaz.Tree[A]] =
    typeclass.eq.get(scalaz.Tree.treeEqual(typeclass.eq.reverseGet(A)))
  implicit def either3Eq[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[Either3[A, B, C]] =
    typeclass.eq.get(scalaz.Either3.equal(typeclass.eq.reverseGet(A), typeclass.eq.reverseGet(B), typeclass.eq.reverseGet(C)))

  // Arbitrary instances

  implicit def treeArbitrary[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary {
      def genPartition(sum: Int): Gen[List[Int]] =
        if(sum <= 0) Gen.const(Nil)
        else for {
          n    <- Gen.choose(1, sum)
          tail <- genPartition(sum - n)
        } yield n :: tail

      def sizedTree(size: Int): Gen[Tree[A]] =
        for {
          value      <- Arbitrary.arbitrary[A]
          partitions <- genPartition(size - 1)
          children   <- partitions.traverseU(sizedTree)
        } yield Tree.node[A](value, children.toStream)

      Gen.sized(sz => sizedTree(sz))
    }

  implicit def optionArbitrary[A: Arbitrary]: Arbitrary[Option[A]] = Arbitrary(Gen.frequency(
    1 -> None,
    3 -> Arbitrary.arbitrary[A].map(Option(_))
  ))

  implicit def maybeArbitrary[A: Arbitrary]: Arbitrary[Maybe[A]] = Arbitrary(Gen.frequency(
    1 -> Maybe.empty[A],
    3 -> Arbitrary.arbitrary[A].map(Maybe.just(_))
  ))

  implicit def someArbitrary[A: Arbitrary]: Arbitrary[Some[A]] = Arbitrary(Arbitrary.arbitrary[A].map(Some(_)))

  implicit def disjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Arbitrary(arbitrary[Either[A, B]] map \/.fromEither)

  implicit def validationArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Validation[A, B]] =
    Arbitrary(arbitrary[A \/ B].map(_.validation))

  implicit def theseArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \&/ B] =
    Arbitrary(Gen.oneOf(
      arbitrary[A].map(This(_)),
      arbitrary[B].map(That(_)),
      for {
        a <- arbitrary[A]
        b <- arbitrary[B]
      } yield Both(a, b)))

  implicit def oneAndArbitrary[T[_], A](implicit a: Arbitrary[A], ta: Arbitrary[T[A]]): Arbitrary[OneAnd[T, A]] = Arbitrary(for {
    head <- Arbitrary.arbitrary[A]
    tail <- Arbitrary.arbitrary[T[A]]
  } yield OneAnd(head, tail))

  implicit def vectorArbitrary[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toVector))

  implicit def iListArbitrary[A: Arbitrary]: Arbitrary[IList[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(IList.fromList))

  implicit def mapArbitrary[K: Arbitrary, V: Arbitrary] =
    Arbitrary(Arbitrary.arbitrary[List[(K,V)]].map(_.toMap))

  implicit def iMapArbitrary[K: Arbitrary: scalaz.Order, V: Arbitrary] =
    Arbitrary(Arbitrary.arbitrary[List[(K,V)]].map(IMap.fromList(_)))

  implicit def setArbitrary[A: Arbitrary]: Arbitrary[Set[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toSet))

  implicit def iSetArbitrary[A: Arbitrary: scalaz.Order]: Arbitrary[ISet[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(l => ISet.fromList(l)))

  implicit def nelArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(oneAndArbitrary[List,A].arbitrary.map(o => NonEmptyList(o.head, o.tail:_*)))

  implicit def either3Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Arbitrary[Either3[A, B, C]] =
    Arbitrary(Gen.oneOf(
      Arbitrary.arbitrary[A].map(Either3.left3),
      Arbitrary.arbitrary[B].map(Either3.middle3),
      Arbitrary.arbitrary[C].map(Either3.right3)
    ))

}
