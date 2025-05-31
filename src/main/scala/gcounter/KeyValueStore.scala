package gcounter

trait KeyValueStore[F[_, _], K, V] {
  def put(f: F[K, V])(k: K, v: V): F[K, V]

  def get(f: F[K, V])(k: K): Option[V]

  def getOrElse(f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values(f: F[K, V]): List[V]

  def keys(f: F[K, V]): List[K]
}

object KeyValueStore {
  given [K, V]: KeyValueStore[Map, K, V] with {
    def put(f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    def get(f: Map[K, V])(k: K): Option[V] = f.get(k)

    def values(f: Map[K, V]): List[V] = f.values.toList

    def keys(f: Map[K, V]): List[K] = f.keys.toList
  }
}
