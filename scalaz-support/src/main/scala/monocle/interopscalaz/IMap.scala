package monocle.interopscalaz

import cats.Applicative
import monocle.function._
import monocle.{Lens, Prism, Traversal}

import cats.std.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import scalaz.{Traverse, IMap, Order}

object imap extends IMapOptics

trait IMapOptics {

  implicit def iMapEmpty[K, V]: Empty[IMap[K, V]] = new Empty[IMap[K, V]] {
    def empty = Prism[K IMap V, Unit](m => if(m.isEmpty) Some(()) else None)(_ => IMap.empty)
  }

  implicit def atIMap[K: Order, V]: At[IMap[K, V], K, V] = new At[IMap[K, V], K, V]{
    def at(i: K) = Lens{m: IMap[K, V] => m.lookup(i)}(optV => map => optV.fold(map - i)(v => map + (i -> v)))
  }

  implicit def iMapEach[K, V]: Each[IMap[K, V], V] =
    Each.traverseEach[IMap[K, ?], V](typeclass.traverse[IMap[K, ?]].get(IMap.mapCovariant[K]))

  implicit def iMapIndex[K: Order, V]: Index[IMap[K, V], K, V] = Index.atIndex

  implicit def iMapFilterIndex[K: Order, V]: FilterIndex[IMap[K, V], K, V] = new FilterIndex[IMap[K, V], K, V] {
    def filterIndex(predicate: K => Boolean) = new Traversal[IMap[K, V], V] {
      def modifyF[F[_]](f: V => F[V])(s: IMap[K, V])(implicit F: Applicative[F]): F[IMap[K, V]] =
        s.toList.traverse{ case (k, v) =>
          (if(predicate(k)) f(v) else F.pure(v)).map(k -> _)
        }.map(IMap.fromList(_))
    }
  }

}
