package monocle

import cats.arrow.Choice
import cats.{Applicative, Monoid}
import cats.data.Xor

/**
 * A [[POptional]] can be seen as a pair of functions:
 *  - `getOrModify: S      => T Xor A`
 *  - `set        : (B, S) => T`
 *
 * A [[POptional]] could also be defined as a weaker [[PLens]] and
 * weaker [[PPrism]]
 *
 * [[POptional]] stands for Polymorphic Optional as it set and modify methods change
 * a type `A` to `B` and `S` to `T`.
 * [[Optional]] is a type alias for [[POptional]] restricted to monomorphic updates:
 * {{{
 * type Optional[S, A] = POptional[S, S, A, A]
 * }}}
 *
 * @see [[monocle.law.OptionalLaws]]
 *
 * @tparam S the source of a [[POptional]]
 * @tparam T the modified source of a [[POptional]]
 * @tparam A the target of a [[POptional]]
 * @tparam B the modified target of a [[POptional]]
 */
abstract class POptional[S, T, A, B] extends Serializable { self =>

  /** get the target of a [[POptional]] or return the original value while allowing the type to change if it does not match */
  def getOrModify(s: S): T Xor A

  /** get the modified source of a [[POptional]] */
  def set(b: B): S => T

  /** get the target of a [[POptional]] or nothing if there is no target */
  def getOption(s: S): Option[A]

  /** modify polymorphically the target of a [[POptional]] with an Applicative function */
  def modifyF[F[_]](f: A => F[B])(s: S)(implicit F: Applicative[F]): F[T]

  /** modify polymorphically the target of a [[POptional]] with a function */
  def modify(f: A => B): S => T

  /**
   * modify polymorphically the target of a [[POptional]] with a function.
   * return empty if the [[POptional]] is not getOrModify
   */
  final def modifyOption(f: A => B): S => Option[T] =
    s => getOption(s).map(a => set(f(a))(s))

  /**
   * set polymorphically the target of a [[POptional]] with a value.
   * return empty if the [[POptional]] is not getOrModify
   */
  final def setOption(b: B): S => Option[T] =
    modifyOption(_ => b)

  /** check if a [[POptional]] has a target */
  final def isMatching(s: S): Boolean =
    getOption(s).isDefined

  /** join two [[POptional]] with the same target */
  final def sum[S1, T1](other: POptional[S1, T1, A, B]): POptional[S Xor S1, T Xor T1, A, B] =
    POptional[S Xor S1, T Xor T1, A, B](_.fold(self.getOrModify(_).leftMap(Xor.left), other.getOrModify(_).leftMap(Xor.right))){
      b => _.bimap(self.set(b), other.set(b))
    }

  final def first[C]: POptional[(S, C), (T, C), (A, C), (B, C)] =
    POptional[(S, C), (T, C), (A, C), (B, C)]{
      case (s, c) => getOrModify(s).bimap(_ -> c, _ -> c)
    }{ case (b, c) => {
        case (s, _) => (set(b)(s), c)
      }
    }

  final def second[C]: POptional[(C, S), (C, T), (C, A), (C, B)] =
    POptional[(C, S), (C, T), (C, A), (C, B)]{
      case (c, s) => getOrModify(s).bimap(c -> _, c -> _)
    }{ case (c, b) => {
        case (_, s) => (c, set(b)(s))
      }
    }

  /***************************************************************/
  /** Compose methods between a [[POptional]] and another Optics */
  /***************************************************************/

  /** compose a [[POptional]] with a [[Fold]] */
  final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asFold composeFold other

  /** compose a [[POptional]] with a [[Getter]] */
  final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asFold composeGetter other

  /** compose a [[POptional]] with a [[PSetter]] */
  final def composeSetter[C, D](other: PSetter[A, B, C, D]): PSetter[S, T, C, D] =
    asSetter composeSetter other

  /** compose a [[POptional]] with a [[PTraversal]] */
  final def composeTraversal[C, D](other: PTraversal[A, B, C, D]): PTraversal[S, T, C, D] =
    asTraversal composeTraversal other

  /** compose a [[POptional]] with a [[POptional]] */
  final def composeOptional[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] =
    new POptional[S, T, C, D]{
      def getOrModify(s: S): T Xor C =
        self.getOrModify(s).flatMap(a => other.getOrModify(a).bimap(self.set(_)(s), identity))

      def set(d: D): S => T =
        self.modify(other.set(d))

      def getOption(s: S): Option[C] =
        self.getOption(s) flatMap other.getOption

      def modifyF[F[_]: Applicative](f: C => F[D])(s: S): F[T] =
        self.modifyF(other.modifyF(f))(s)

      def modify(f: C => D): S => T =
        self.modify(other.modify(f))
    }

  /** compose a [[POptional]] with a [[PPrism]] */
  final def composePrism[C, D](other: PPrism[A, B, C, D]): POptional[S, T, C, D] =
    composeOptional(other.asOptional)

  /** compose a [[POptional]] with a [[PLens]] */
  final def composeLens[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] =
    composeOptional(other.asOptional)

  /** compose a [[POptional]] with a [[PIso]] */
  final def composeIso[C, D](other: PIso[A, B, C, D]): POptional[S, T, C, D] =
    composeOptional(other.asOptional)

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
  final def ^<-?[C, D](other: PPrism[A, B, C, D]): POptional[S, T, C, D] =
    composePrism(other)

  /** alias to composeLens */
  final def ^|->[C, D](other: PLens[A, B, C, D]): POptional[S, T, C, D] =
    composeLens(other)

  /** alias to composeIso */
  final def ^<->[C, D](other: PIso[A, B, C, D]): POptional[S, T, C, D] =
    composeIso(other)

  /*********************************************************************/
  /** Transformation methods to view a [[POptional]] as another Optics */
  /*********************************************************************/

  /** view a [[POptional]] as a [[Fold]] */
  final def asFold: Fold[S, A] = new Fold[S, A]{
    def foldMap[M: Monoid](f: A => M)(s: S): M =
      self.getOption(s) map f getOrElse Monoid[M].empty
  }

  /** view a [[POptional]] as a [[PSetter]] */
  final def asSetter: PSetter[S, T, A, B] =
    new PSetter[S, T, A, B]{
      def modify(f: A => B): S => T =
        self.modify(f)

      def set(b: B): S => T =
        self.set(b)
    }

  /** view a [[POptional]] as a [[PTraversal]] */
  final def asTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
    def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
      self.modifyF(f)(s)
  }

}

object POptional extends OptionalInstances {
  def id[S, T]: POptional[S, T, S, T] =
    PIso.id[S, T].asOptional

  def codiagonal[S, T]: POptional[S Xor S, T Xor T, S, T] =
    POptional[S Xor S, T Xor T, S, T](
      _.fold(Xor.right, Xor.right)
    )(t => _.bimap(_ => t, _ => t))

  /** create a [[POptional]] using the canonical functions: getOrModify and set */
  def apply[S, T, A, B](_getOrModify: S => T Xor A)(_set: B => S => T): POptional[S, T, A, B] =
    new POptional[S, T, A, B]{
      def getOrModify(s: S): T Xor A =
        _getOrModify(s)

      def set(b: B): S => T =
        _set(b)

      def getOption(s: S): Option[A] =
        _getOrModify(s).toOption

      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
        _getOrModify(s).fold(
          t => Applicative[F].pure(t),
          a => Applicative[F].map(f(a))(_set(_)(s))
        )

      def modify(f: A => B): S => T =
        s => _getOrModify(s).fold(identity, a => _set(f(a))(s))
    }
}

object Optional {
  def id[A]: Optional[A, A] =
    Iso.id[A].asOptional

  def codiagonal[S]: Optional[S Xor S, S] =
    POptional.codiagonal

  /** [[Optional]] that points to nothing */
  def void[S, A]: Optional[S, A] =
    Optional[S, A](_ => None)(_ => identity)

  /** alias for [[POptional]] apply restricted to monomorphic update */
  def apply[S, A](_getOption: S => Option[A])(_set: A => S => S): Optional[S, A] =
    new Optional[S, A]{
      def getOrModify(s: S): S Xor A =
        _getOption(s).fold[S Xor A](Xor.left(s))(Xor.right)

      def set(a: A): S => S =
        _set(a)

      def getOption(s: S): Option[A] =
        _getOption(s)

      def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
        _getOption(s).fold(
          Applicative[F].pure(s))(
          a => Applicative[F].map(f(a))(_set(_)(s))
        )

      def modify(f: A => A): S => S =
        s => _getOption(s).fold(s)(a => _set(f(a))(s))
    }
}

sealed abstract class OptionalInstances {
  implicit val optionalChoice: Choice[Optional] = new Choice[Optional] {
    override def choice[A, B, C](f: Optional[A, C], g: Optional[B, C]): Optional[A Xor B, C] =
      f sum g

    def id[A]: Optional[A, A] =
      Optional.id[A]

    def compose[A, B, C](f: Optional[B, C], g: Optional[A, B]): Optional[A, C] =
      g composeOptional f
  }
}