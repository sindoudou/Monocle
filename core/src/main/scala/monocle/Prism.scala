package monocle

import cats.{Applicative, Eq, Monoid, Traverse}
import cats.arrow.Category
import cats.data.Xor
import cats.std.option._

/**
 * A [[PPrism]] can be seen as a pair of functions:
 *  - `getOrModify: S => T Xor A`
 *  - `reverseGet : B => T`
 *
 * A [[PPrism]] could also be defined as a weaker [[PIso]] where get can fail.
 *
 * Typically a [[PPrism]] or [[Prism]] encodes the relation between a Sum or
 * CoProduct type (e.g. sealed trait) and one of it is element.
 *
 * [[PPrism]] stands for Polymorphic Prism as it set and modify methods change
 * a type `A` to `B` and `S` to `T`.
 * [[Prism]] is a type alias for [[PPrism]] where the type of target cannot be modified:
 * {{{
 * type Prism[S, A] = PPrism[S, S, A, A]
 * }}}
 *
 * A [[PPrism]] is also a valid  [[Fold]], [[POptional]], [[PTraversal]] and [[PSetter]]
 *
 * @see [[monocle.law.PrismLaws]]
 *
 * @tparam S the source of a [[PPrism]]
 * @tparam T the modified source of a [[PPrism]]
 * @tparam A the target of a [[PPrism]]
 * @tparam B the modified target of a [[PPrism]]
 */
abstract class PPrism[S, T, A, B] extends Serializable { self =>

  /** get the target of a [[PPrism]] or modify the source in case there is no target */
  def getOrModify(s: S): T Xor A

  /** get the modified source of a [[PPrism]] */
  def reverseGet(b: B): T

  /** get the target of a [[PPrism]] or nothing if there is no target */
  def getOption(s: S): Option[A]

  /** modify polymorphically the target of a [[PPrism]] with an Applicative function */
  final def modifyF[F[_]](f: A => F[B])(s: S)(implicit F: Applicative[F]): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].pure(t),
      a => Applicative[F].map(f(a))(reverseGet)
    )

  /** modify polymorphically the target of a [[PPrism]] with a function */
  final def modify(f: A => B): S => T =
    getOrModify(_).fold(identity,a => reverseGet(f(a)))

  /**
   * modify polymorphically the target of a [[PPrism]] with a function.
   * return empty if the [[PPrism]] is not getOrModify
   */
  final def modifyOption(f: A => B): S => Option[T] =
    s => getOption(s).map(a => reverseGet(f(a)))

  /** set polymorphically the target of a [[PPrism]] with a value */
  final def set(b: B): S => T =
    modify(_ => b)

  /**
   * set polymorphically the target of a [[PPrism]] with a value.
   * return empty if the [[PPrism]] is not getOrModify
   */
  final def setOption(b: B): S => Option[T] =
    modifyOption(_ => b)

  /** check if a [[PPrism]] has a target */
  final def isMatching(s: S): Boolean =
    getOption(s).isDefined

  /** create a [[Getter]] from the modified target to the modified source of a [[PPrism]] */
  final def re: Getter[B, T] =
    Getter(reverseGet)

  final def first[C]: PPrism[(S, C), (T, C), (A, C), (B, C)] =
    PPrism[(S, C), (T, C), (A, C), (B, C)]{
      case (s, c) => getOrModify(s).bimap(_ -> c, _ -> c)
    }{
      case (b, c) => (reverseGet(b), c)
    }

  final def second[C]: PPrism[(C, S), (C, T), (C, A), (C, B)] =
    PPrism[(C, S), (C, T), (C, A), (C, B)]{
      case (c, s) => getOrModify(s).bimap(c -> _, c -> _)
    }{
      case (c, b) => (c, reverseGet(b))
    }

  /************************************************************/
  /** Compose methods between a [[PPrism]] and another Optics */
  /************************************************************/

  /** compose a [[PPrism]] with a [[Fold]] */
  final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asFold composeFold other

  /** compose a [[PPrism]] with a [[Getter]] */
  final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asFold composeGetter other

  /** compose a [[PPrism]] with a [[PSetter]] */
  final def composeSetter[C, D](other: PSetter[A, B, C, D]): PSetter[S, T, C, D] =
    asSetter composeSetter other

  /** compose a [[PPrism]] with a [[PTraversal]] */
  final def composeTraversal[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    asTraversal composeTraversal other

  /** compose a [[PPrism]] with a [[POptional]] */
  final def composeOptional[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] =
    asOptional composeOptional other

  /** compose a [[PPrism]] with a [[PLens]] */
  final def composeLens[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] =
    asOptional composeOptional other.asOptional

  /** compose a [[PPrism]] with a [[PPrism]] */
  final def composePrism[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] =
    new PPrism[S, T, C, D]{
      def getOrModify(s: S): T Xor C =
        self.getOrModify(s).flatMap(a => other.getOrModify(a).bimap(self.set(_)(s), identity))

      def reverseGet(d: D): T =
        self.reverseGet(other.reverseGet(d))

      def getOption(s: S): Option[C] =
        self.getOption(s) flatMap other.getOption
    }

  /** compose a [[PPrism]] with a [[PIso]] */
  final def composeIso[C, D](other: PIso[A, B, C, D]): PPrism[S, T, C, D] =
    composePrism(other.asPrism)

  /********************************************/
  /** Experimental aliases of compose methods */
  /********************************************/

  /** alias to composeTraversal */
  final def ^|->>[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    composeTraversal(other)

  /** alias to composeOptional */
  final def ^|-?[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] =
    composeOptional(other)

  /** alias to composePrism */
  final def ^<-?[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] =
    composePrism(other)

  /** alias to composeLens */
  final def ^|->[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] =
    composeLens(other)

  /** alias to composeIso */
  final def ^<->[C, D](other: PIso[A, B, C, D]): PPrism[S, T, C, D] =
    composeIso(other)

  /******************************************************************/
  /** Transformation methods to view a [[PPrism]] as another Optics */
  /******************************************************************/

  /** view a [[PPrism]] as a [[Fold]] */
  final def asFold: Fold[S, A] = new Fold[S, A]{
    def foldMap[M: Monoid](f: A => M)(s: S): M =
      getOption(s) map f getOrElse Monoid[M].empty
  }

  /** view a [[PPrism]] as a [[Setter]] */
  final def asSetter: PSetter[S, T, A, B] =
    new PSetter[S, T, A, B]{
      def modify(f: A => B): S => T =
        self.modify(f)

      def set(b: B): S => T =
        self.set(b)
    }

  /** view a [[PPrism]] as a [[PTraversal]] */
  final def asTraversal: PTraversal[S, T, A, B] =
    new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        self.modifyF(f)(s)
    }

  /** view a [[PPrism]] as a [[POptional]] */
  final def asOptional: POptional[S, T, A, B] =
    new POptional[S, T, A, B]{
      def getOrModify(s: S): T Xor A =
        self.getOrModify(s)

      def set(b: B): S => T =
        self.set(b)

      def getOption(s: S): Option[A] =
        self.getOption(s)

      def modify(f: A => B): S => T =
        self.modify(f)

      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        self.modifyF(f)(s)
    }
}

object PPrism extends PrismInstances {
  def id[S, T]: PPrism[S, T, S, T] =
    PIso.id[S, T].asPrism

  /** create a [[PPrism]] using the canonical functions: getOrModify and reverseGet */
  def apply[S, T, A, B](_getOrModify: S => T Xor A)(_reverseGet: B => T): PPrism[S, T, A, B] =
    new PPrism[S, T, A, B]{
      def getOrModify(s: S): T Xor A =
        _getOrModify(s)

      def reverseGet(b: B): T =
        _reverseGet(b)

      def getOption(s: S): Option[A] =
        _getOrModify(s).toOption
    }

  implicit def prismSyntax[S, A](self: Prism[S, A]): PrismSyntax[S, A] =
    new PrismSyntax(self)
}

object Prism {
  def id[A]: Prism[A, A] =
    Iso.id[A].asPrism

  /** alias for [[PPrism]] apply restricted to monomorphic update */
  def apply[S, A](_getOption: S => Option[A])(_reverseGet: A => S): Prism[S, A] =
    new Prism[S, A]{
      def getOrModify(s: S): S Xor A =
        _getOption(s).fold[S Xor A](Xor.left(s))(Xor.right)

      def reverseGet(b: A): S =
        _reverseGet(b)

      def getOption(s: S): Option[A] =
        _getOption(s)
    }

  /** a [[Prism]] that checks for equality with a given value */
  def only[A](a: A)(implicit A: Eq[A]): Prism[A, Unit] =
    Prism[A, Unit](a2 => if(A.eqv(a, a2)) Some(()) else None)(_ => a)
}

sealed abstract class PrismInstances {
  implicit val prismCategory: Category[Prism] = new Category[Prism] {
    def id[A]: Prism[A, A] =
      Prism.id

    def compose[A, B, C](f: Prism[B, C], g: Prism[A, B]): Prism[A, C] =
      g composePrism f
  }
}

final case class PrismSyntax[S, A](self: Prism[S, A]) extends AnyVal {

  /** lift a [[Prism]] such as it only matches if all elements of `F[S]` are getOrModify */
  def below[F[_]](implicit F: Traverse[F]): Prism[F[S], F[A]] =
    Prism[F[S], F[A]](F.traverse(_)(self.getOption))(F.map(_)(self.reverseGet))
}