package monocle.interopscalaz

import cats.Eval
import monocle.Iso

import scalaz.{Ordering, Monoid}

object typeclass extends TypeclassOptics

trait TypeclassOptics {

  def eq[A]: Iso[scalaz.Equal[A], cats.Eq[A]] =
    Iso[scalaz.Equal[A], cats.Eq[A]](other =>
      new cats.Eq[A]{
        override def eqv(x: A, y: A): Boolean = other.equal(x, y)
    })(other =>
      new scalaz.Equal[A] {
        override def equal(x: A, y: A): Boolean = other.eqv(x, y)
    })

  def order[A]: Iso[scalaz.Order[A], cats.Order[A]] =
    Iso[scalaz.Order[A], cats.Order[A]](other =>
      new cats.Order[A]{
        override def compare(x: A, y: A): Int =
          other.apply(x, y).toInt
    })(other =>
      new scalaz.Order[A] {
        override def order(x: A, y: A): Ordering =
          Ordering.fromInt(other.compare(x, y))
    })

  def monoid[A]: Iso[scalaz.Monoid[A], cats.Monoid[A]] =
    Iso[scalaz.Monoid[A], cats.Monoid[A]](other =>
      new cats.Monoid[A]{
        override def empty: A = other.zero
        override def combine(x: A, y: A): A = other.append(x, y)
    })(other =>
      new scalaz.Monoid[A] {
        override def zero: A = other.empty
        override def append(f1: A, f2: => A): A = other.combine(f1, f2)
      })

  def applicative[F[_]]: Iso[scalaz.Applicative[F], cats.Applicative[F]] =
    Iso[scalaz.Applicative[F], cats.Applicative[F]](other =>
      new cats.Applicative[F]{
        override def pure[A](x: A): F[A] = other.pure(x)
        override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] = other.ap(fa)(f)
    })(other =>
      new scalaz.Applicative[F] {
        override def point[A](a: => A): F[A] = other.pure(a)
        override def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B] = other.ap(fa)(f)
    })

  def foldable[F[_]]: Iso[scalaz.Foldable[F], cats.Foldable[F]] =
    Iso[scalaz.Foldable[F], cats.Foldable[F]](other =>
      new cats.Foldable[F]{
        override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
          other.foldLeft(fa, b)(f)
        override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          Eval.now(other.foldRight(fa, lb.value)((a, b) => f(a, Eval.now(b)).value))
    })(other =>
      new scalaz.Foldable[F] {
        override def foldMap[A, B](fa: F[A])(f: (A) => B)(implicit F: Monoid[B]): B =
          other.foldMap(fa)(f)(monoid.get(F))

        override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B =
          other.foldRight(fa, Eval.later(z))((a, b) => b.map(f(a, _))).value
    })

  def traverse[F[_]]: Iso[scalaz.Traverse[F], cats.Traverse[F]] =
    Iso[scalaz.Traverse[F], cats.Traverse[F]](other =>
      new cats.Traverse[F]{
        override def traverse[G[_], A, B](fa: F[A])(f: (A) => G[B])(implicit G: cats.Applicative[G]): G[F[B]] =
          other.traverseImpl(fa)(f)(applicative.reverseGet(G))
        override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
          other.foldLeft(fa, b)(f)
        override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          Eval.now(other.foldRight(fa, lb.value)((a, b) => f(a, Eval.now(b)).value))
    })(other =>
      new scalaz.Traverse[F] {
        override def traverseImpl[G[_], A, B](fa: F[A])(f: (A) => G[B])(implicit G: scalaz.Applicative[G]): G[F[B]] =
          other.traverse(fa)(f)(applicative.get(G))
    })

}
