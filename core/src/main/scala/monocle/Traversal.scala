package monocle

import cats.arrow.Choice
import cats.{Applicative, Functor, Id, Monoid, Traverse}
import cats.data.{Const, Xor}
import cats.std.list._
import monocle.internal.{First, Conjunction}

/**
 * A [[PTraversal]] can be seen as a [[POptional]] generalised to 0 to n targets
 * where n can be infinite.
 *
 * [[PTraversal]] stands for Polymorphic Traversal as it set and modify methods change
 * a type `A` to `B` and `S` to `T`.
 * [[Traversal]] is a type alias for [[PTraversal]] restricted to monomorphic updates:
 * {{{
 * type Traversal[S, A] = PTraversal[S, S, A, A]
 * }}}
 *
 * @see [[monocle.law.TraversalLaws]]
 *
 * @tparam S the source of a [[PTraversal]]
 * @tparam T the modified source of a [[PTraversal]]
 * @tparam A the target of a [[PTraversal]]
 * @tparam B the modified target of a [[PTraversal]]
 */
abstract class PTraversal[S, T, A, B] extends Serializable { self =>

  /**
   * modify polymorphically the target of a [[PTraversal]] with an Applicative function
   * all traversal methods are written in terms of modifyF
   */
  def modifyF[F[_]](f: A => F[B])(s: S)(implicit F: Applicative[F]): F[T]

  /** map each target to a Monoid and combine the results */
  final def foldMap[M: Monoid](f: A => M)(s: S): M =
    modifyF[Const[M, ?]](a => Const(f(a)))(s).getConst

  /** combine all targets using a target's Monoid */
  final def fold(s: S)(implicit A: Monoid[A]): A =
    foldMap(identity)(s)

  /** get all the targets of a [[PTraversal]] */
  final def getAll(s: S): List[A] =
    foldMap(List(_))(s)

  /** find the first target of a [[PTraversal]] matching the predicate  */
  final def find(p: A => Boolean)(s: S): Option[A] =
    foldMap(a => First(if(p(a)) Some(a) else None))(s).unwrap

  /** get the first target of a [[PTraversal]] */
  final def headOption(s: S): Option[A] =
    find(_ => true)(s)

  /** check if at least one target satisfies the predicate */
  final def exist(p: A => Boolean)(s: S): Boolean =
    find(p)(s).isDefined

  /** check if all targets satisfy the predicate */
  final def all(p: A => Boolean)(s: S): Boolean =
    foldMap(a => Conjunction(p(a)))(s).unwrap

  /** modify polymorphically the target of a [[PTraversal]] with a function */
  final def modify(f: A => B): S => T =
    modifyF[Id](f)

  /** set polymorphically the target of a [[PTraversal]] with a value */
  final def set(b: B): S => T =
    modify(_ => b)

  /** join two [[PTraversal]] with the same target */
  final def sum[S1, T1](other: PTraversal[S1, T1, A, B]): PTraversal[S Xor S1, T Xor T1, A, B] =
    new PTraversal[S Xor S1, T Xor T1, A, B]{
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S Xor S1): F[T Xor T1] =
        s.fold(
          s  => Functor[F].map(self.modifyF(f)(s))(Xor.left),
          s1 => Functor[F].map(other.modifyF(f)(s1))(Xor.right)
        )
    }

  /****************************************************************/
  /** Compose methods between a [[PTraversal]] and another Optics */
  /****************************************************************/

  /** compose a [[PTraversal]] with a [[Fold]] */
  final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asFold composeFold other

  /** compose a [[PTraversal]] with a [[Getter]] */
  final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asFold composeGetter other

  /** compose a [[PTraversal]] with a [[PSetter]] */
  final def composeSetter[C, D](other: PSetter[A, B, C, D]): PSetter[S, T, C, D] =
    asSetter composeSetter other

  /** compose a [[PTraversal]] with a [[PTraversal]] */
  final def composeTraversal[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    new PTraversal[S, T, C, D] {
      def modifyF[F[_]: Applicative](f: C => F[D])(s: S): F[T] =
        self.modifyF(other.modifyF(f)(_))(s)
    }

  /** compose a [[PTraversal]] with a [[POptional]] */
  final def composeOptional[C, D](other: POptional[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other.asTraversal)

  /** compose a [[PTraversal]] with a [[PPrism]] */
  final def composePrism[C, D](other: PPrism[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other.asTraversal)

  /** compose a [[PTraversal]] with a [[PLens]] */
  final def composeLens[C, D](other: PLens[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other.asTraversal)

  /** compose a [[PTraversal]] with a [[PIso]] */
  final def composeIso[C, D](other: PIso[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other.asTraversal)

  /********************************************/
  /** Experimental aliases of compose methods */
  /********************************************/

  /** alias to composeTraversal */
  final def ^|->>[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other)

  /** alias to composeOptional */
  final def ^|-?[C, D](other: POptional[A, B, C, D]): PTraversal[S, T, C, D] =
    composeOptional(other)

  /** alias to composePrism */
  final def ^<-?[C, D](other: PPrism[A, B, C, D]): PTraversal[S, T, C, D] =
    composePrism(other)

  /** alias to composeLens */
  final def ^|->[C, D](other: PLens[A, B, C, D]): PTraversal[S, T, C, D] =
    composeLens(other)

  /** alias to composeIso */
  final def ^<->[C, D](other: PIso[A, B, C, D]): PTraversal[S, T, C, D] =
    composeIso(other)

  /**********************************************************************/
  /** Transformation methods to view a [[PTraversal]] as another Optics */
  /**********************************************************************/

  /** view a [[PTraversal]] as a [[Fold]] */
  final def asFold: Fold[S, A] =
    new Fold[S, A]{
      def foldMap[M: Monoid](f: A => M)(s: S): M =
        self.foldMap(f)(s)
    }

  /** view a [[PTraversal]] as a [[PSetter]] */
  final def asSetter: PSetter[S, T, A, B] =
    PSetter(modify)

}

object PTraversal extends TraversalInstances {
  def id[S, T]: PTraversal[S, T, S, T] =
    PIso.id[S, T].asTraversal

  def codiagonal[S, T]: PTraversal[S Xor S, T Xor T, S, T] =
    new PTraversal[S Xor S, T Xor T, S, T]{
      def modifyF[F[_]: Applicative](f: S => F[T])(s: S Xor S): F[T Xor T] =
        s.bimap(f,f).fold(Applicative[F].map(_)(Xor.left), Applicative[F].map(_)(Xor.right))
    }

  /** create a [[PTraversal]] from a Traverse */
  def fromTraverse[T[_]: Traverse, A, B]: PTraversal[T[A], T[B], A, B] =
    new PTraversal[T[A], T[B], A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: T[A]): F[T[B]] =
        Traverse[T].traverse(s)(f)
    }

  def apply2[S, T, A, B](get1: S => A, get2: S => A)(_set: (B, B, S) => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        Applicative[F].map2(f(get1(s)), f(get2(s)))(_set(_, _, s))
    }

  def apply3[S, T, A, B](get1: S => A, get2: S => A, get3: S => A)(_set: (B, B, B, S) => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        Applicative[F].map3(f(get1(s)), f(get2(s)), f(get3(s)))(_set(_, _, _, s))
    }

  def apply4[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A)(_set: (B, B, B, B, S) => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        Applicative[F].map4(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)))(_set(_, _, _, _, s))
    }

  def apply5[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A)(_set: (B, B, B, B, B, S) => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        Applicative[F].map5(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)))(_set(_, _, _, _, _, s))
    }

  def apply6[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A, get6: S => A)(_set: (B, B, B, B, B, B, S) => T): PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        Applicative[F].map6(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)), f(get6(s)))(_set(_, _, _, _, _, _, s))
    }

}

object Traversal {
  def id[A]: Traversal[A, A] =
    Iso.id[A].asTraversal

  def codiagonal[S, T]: Traversal[S Xor S, S] =
    PTraversal.codiagonal

  /** [[Traversal]] that points to nothing */
  def void[S, A]: Traversal[S, A] =
    Optional.void.asTraversal

  def apply2[S, A](get1: S => A, get2: S => A)(set: (A, A, S) => S): Traversal[S, A] =
    PTraversal.apply2(get1, get2)(set)

  def apply3[S, A](get1: S => A, get2: S => A, get3: S => A)(set: (A, A, A, S) => S): Traversal[S, A] =
    PTraversal.apply3(get1, get2, get3)(set)

  def apply4[S, A](get1: S => A, get2: S => A, get3: S => A, get4: S => A)(set: (A, A, A, A, S) => S): Traversal[S, A] =
    PTraversal.apply4(get1, get2, get3, get4)(set)

  def apply5[S, A](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A)(set: (A, A, A, A, A, S) => S): Traversal[S, A] =
    PTraversal.apply5(get1, get2, get3, get4, get5)(set)

  def apply6[S, A](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A, get6: S => A)(set: (A, A, A, A, A, A, S) => S): Traversal[S, A] =
    PTraversal.apply6(get1, get2, get3, get4, get5, get6)(set)
}

sealed abstract class TraversalInstances {
  implicit val traversalChoice: Choice[Traversal] = new Choice[Traversal] {
    def compose[A, B, C](f: Traversal[B, C], g: Traversal[A, B]): Traversal[A, C] =
      g composeTraversal f

    def id[A]: Traversal[A, A] =
      Traversal.id

    override def choice[A, B, C](f: Traversal[A, C], g: Traversal[B, C]): Traversal[A Xor B, C] =
      f sum g
  }
}

