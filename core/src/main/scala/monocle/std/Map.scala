package monocle.std

import cats.Applicative
import cats.std.list._
import cats.std.map._
import cats.syntax.functor._
import cats.syntax.traverse._
import monocle.function._
import monocle.{Lens, Prism, Traversal}

object map extends MapOptics

trait MapOptics {

  implicit def mapEmpty[K, V]: Empty[Map[K, V]] = new Empty[Map[K, V]] {
    def empty = Prism[Map[K, V], Unit](m => if(m.isEmpty) Some(()) else None)(_ => Map.empty)
  }

  implicit def atMap[K, V]: At[Map[K, V], K, V] = new At[Map[K, V], K, V]{
    def at(i: K) = Lens{m: Map[K, V] => m.get(i)}(optV => map => optV.fold(map - i)(v => map + (i -> v)))
  }

  implicit def mapEach[K, V]: Each[Map[K, V], V] = Each.traverseEach[Map[K, ?], V]

  implicit def mapIndex[K, V]: Index[Map[K, V], K  , V] = Index.atIndex

  implicit def mapFilterIndex[K, V]: FilterIndex[Map[K,V], K, V] = new FilterIndex[Map[K, V], K, V] {
    def filterIndex(predicate: K => Boolean) = new Traversal[Map[K, V], V] {
      def modifyF[F[_]](f: V => F[V])(s: Map[K, V])(implicit F: Applicative[F]): F[Map[K, V]] =
        s.toList.traverse{ case (k, v) =>
          (if(predicate(k)) f(v) else F.pure(v)).map(k -> _)
        }.map(_.toMap)
    }
  }

}
