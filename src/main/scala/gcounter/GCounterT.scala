package gcounter

import cats.instances.list.*
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.instances.int.*
import cats.kernel.CommutativeMonoid

trait GCounterT[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounterT {
  def apply[F[_, _], K, V](implicit counter: GCounterT[F, K, V]): GCounterT[F, K, V] = counter

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F, K, V]): F[K, V] = kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F, K, V]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F, K, V]): V = kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F, K, V]): List[V] = kvs.values(f)

    def keys(implicit kvs: KeyValueStore[F, K, V]): List[K] = kvs.keys(f)
  }

  implicit def instanceGCounterT[K, V, F[_, _]](implicit
                                                kvs: KeyValueStore[F, K, V],
                                                b: BoundedSemiLattice[V]): GCounterT[F, K, V] =
    new GCounterT[F, K, V] {

      def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val value = v |+| f.getOrElse(k, m.empty)
        f.put(k, value)
      }

      def merge(f1: F[K, V], f2: F[K, V]): F[K, V] = {
        val allKeys = f1.keys ++ f2.keys
        allKeys.foldLeft(f1) {
          (acc, k) => acc.put(k, f1.getOrElse(k, b.empty) |+| f2.getOrElse(k, b.empty))
        }
      }

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
    }

  def main(args: Array[String]): Unit = {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)
    val counter = GCounterT[Map, String, Int]
    val merge = counter.merge(g1, g2)
    println(merge)
    println(counter.total(merge))
  }
}