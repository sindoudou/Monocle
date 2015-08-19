package monocle

import cats.data.Xor
import cats.{Foldable, Monoid}
import monocle.internal.{Conjunction, First}

/**
 * A [[Fold]] can be seen as a [[Getter]] with many targets or
 * a weaker [[PTraversal]] which cannot modify its target.
 *
 * [[Fold]] is on the top of the Optic hierarchy which means that
 * [[Getter]], [[PTraversal]], [[POptional]], [[PLens]], [[PPrism]]
 * and [[PIso]] are valid [[Fold]]
 *
 * @tparam S the source of a [[Fold]]
 * @tparam A the target of a [[Fold]]
 */
abstract class Fold[S, A] extends Serializable { self =>

  /**
   * map each target to a Monoid and combine the results
   * underlying representation of [[Fold]], all [[Fold]] methods are defined in terms of foldMap
   */
  def foldMap[M: Monoid](f: A => M)(s: S): M

  /** combine all targets using a target's Monoid */
  final def fold(s: S)(implicit ev: Monoid[A]): A =
    foldMap(identity)(s)

  /**
   * get all the targets of a [[Fold]]
   */
  final def getAll(s: S): List[A] =
    foldMap(List(_))(s)

  /** find the first target of a [[Fold]] matching the predicate  */
  final def find(p: A => Boolean)(s: S): Option[A] =
    foldMap(a => First(if(p(a)) Some(a) else None))(s).unwrap

  /** get the first target of a [[Fold]] */
  final def headOption(s: S): Option[A] =
    find(_ => true)(s)

  /** check if at least one target satisfies the predicate */
  final def exist(p: A => Boolean)(s: S): Boolean =
    find(p)(s).isDefined

  /** check if all targets satisfy the predicate */
  final def all(p: A => Boolean)(s: S): Boolean =
    foldMap(a => Conjunction(p(a)))(s).unwrap

  /** join two [[Fold]] with the same target */
  final def sum[S1](other: Fold[S1, A]): Fold[S Xor S1, A] =
    new Fold[S Xor S1, A]{
      def foldMap[M: Monoid](f: A => M)(s: S Xor S1): M =
        s.fold(self.foldMap(f), other.foldMap(f))
    }

  /**********************************************************/
  /** Compose methods between a [[Fold]] and another Optics */
  /**********************************************************/

  /** compose a [[Fold]] with a [[Fold]] */
  final def composeFold[B](other: Fold[A, B]): Fold[S, B] =
    new Fold[S, B] {
      def foldMap[M: Monoid](f: B => M)(s: S): M =
        self.foldMap(other.foldMap(f)(_))(s)
    }

  /** compose a [[Fold]] with a [[Getter]] */
  final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    composeFold(other.asFold)

  /** compose a [[Fold]] with a [[PTraversal]] */
  final def composeTraversal[B, C, D](other: PTraversal[A, B, C, D]): Fold[S, C] =
    composeFold(other.asFold)

  /** compose a [[Fold]] with a [[POptional]] */
  final def composeOptional[B, C, D](other: POptional[A, B, C, D]): Fold[S, C] =
    composeFold(other.asFold)

  /** compose a [[Fold]] with a [[PPrism]] */
  final def composePrism[B, C, D](other: PPrism[A, B, C, D]): Fold[S, C] =
    composeFold(other.asFold)

  /** compose a [[Fold]] with a [[PLens]] */
  final def composeLens[B, C, D](other: PLens[A, B, C, D]): Fold[S, C] =
    composeFold(other.asFold)

  /** compose a [[Fold]] with a [[PIso]] */
  final def composeIso[B, C, D](other: PIso[A, B, C, D]): Fold[S, C] =
    composeFold(other.asFold)

  /********************************************/
  /** Experimental aliases of compose methods */
  /********************************************/

  /** alias to composeTraversal */
  final def ^|->>[B, C, D](other: PTraversal[A, B, C, D]): Fold[S, C] =
    composeTraversal(other)

  /** alias to composeOptional */
  final def ^|-?[B, C, D](other: POptional[A, B, C, D]): Fold[S, C] =
    composeOptional(other)

  /** alias to composePrism */
  final def ^<-?[B, C, D](other: PPrism[A, B, C, D]): Fold[S, C] =
    composePrism(other)

  /** alias to composeLens */
  final def ^|->[B, C, D](other: PLens[A, B, C, D]): Fold[S, C] =
    composeLens(other)

  /** alias to composeIso */
  final def ^<->[B, C, D](other: PIso[A, B, C, D]): Fold[S, C] =
    composeIso(other)

}

object Fold extends FoldInstances {
  def id[A]: Fold[A, A] =
    Iso.id[A].asFold

  def codiagonal[A]: Fold[A Xor A, A] =
    new Fold[A Xor A, A]{
      def foldMap[M: Monoid](f: A => M)(s: A Xor A): M =
        s.fold(f,f)
    }

  /** [[Fold]] that points to nothing */
  def void[S, A]: Fold[S, A] =
    Optional.void.asFold

  /** create a [[Fold]] from a Foldable */
  def fromFoldable[F[_]: Foldable, A]: Fold[F[A], A] =
    new Fold[F[A], A] {
      def foldMap[M: Monoid](f: A => M)(s: F[A]): M =
        Foldable[F].foldMap(s)(f)
    }
}


sealed abstract class FoldInstances {
  implicit val foldChoice: Choice[Fold] = new Choice[Fold]{
    def choice[A, B, C](f: => Fold[A, C], g: => Fold[B, C]): Fold[A Xor B, C] =
      f sum g

    def id[A]: Fold[A, A] =
      Fold.id[A]

    def compose[A, B, C](f: Fold[B, C], g: Fold[A, B]): Fold[A, C] =
      g composeFold f
  }
}
