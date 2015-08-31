package monocle.interopscalaz

import monocle.function._
import monocle.{Iso, Lens, Traversal}

import cats.Applicative
import scalaz.OneAnd

object oneand extends OneAndOptics

trait OneAndOptics {

  implicit def oneAndEach[T[_], A](implicit ev: Each[T[A], A]): Each[OneAnd[T, A], A] =
    new Each[OneAnd[T, A], A]{
      def each = new Traversal[OneAnd[T, A], A]{
        def modifyF[F[_]](f: (A) => F[A])(s: OneAnd[T, A])(implicit F: Applicative[F]): F[OneAnd[T, A]] =
          typeclass.applicative.reverseGet(F)
            .apply2(f(s.head), ev.each.modifyF(f)(s.tail))((head, tail) => new OneAnd(head, tail))
      }
    }

  implicit def oneAndIndex[T[_], A](implicit ev: Index[T[A], Int, A]): Index[OneAnd[T, A], Int, A] =
    new Index[OneAnd[T, A], Int, A]{
      def index(i: Int) = i match {
        case 0 => oneAndCons1[T, A].head.asOptional
        case _ => oneAndCons1[T, A].tail composeOptional ev.index(i - 1)
      }
    }

  implicit def oneAndField1[T[_], A]: Field1[OneAnd[T, A], A] = new Field1[OneAnd[T, A], A]{
    def first = Lens[OneAnd[T, A], A](_.head)(a => oneAnd => oneAnd.copy(head = a))
  }

  implicit def oneAndCons1[T[_], A]: Cons1[OneAnd[T, A], A, T[A]] = new Cons1[OneAnd[T, A], A, T[A]] {
    def cons1 = Iso[OneAnd[T, A], (A, T[A])](o => (o.head, o.tail)){ case (h, t) => OneAnd(h, t)}
  }

}
