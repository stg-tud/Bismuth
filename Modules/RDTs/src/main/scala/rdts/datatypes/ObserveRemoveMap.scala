package rdts.datatypes

import rdts.base.*
import rdts.time.{Dot, Dots}

/** Observed-remove map.
  *
  * Each key maps to a set of dots, each dot carrying its own value. Values of a key are merged on read.
  *  - Every update (including the first insert) stores `v` under a fresh dot, existing dots are kept.
  *  - Removing a key removes exactly the observed dots of that key, and with them the values written under those dots.
  *
  * Consequently, updates win against concurrent removes (add-wins), but only the concurrent updates survive a remove,
  * not the values that were observed by the remove.
  * Note that the number of dots of a key grows with every update until the key is removed.
  */
case class ObserveRemoveMap[K, V](inner: Map[K, Map[Dot, V]], removed: Dots) {

  lazy val observed: Dots = removed.union:
      inner.values.foldLeft(Dots.empty) {
        case (set, v) => set `union` Dots.from(v.keys)
      }

  type Delta = ObserveRemoveMap[K, V]

  def contains(k: K): Boolean = inner.get(k).exists(_.nonEmpty)

  def dotsOf(k: K): Dots = inner.get(k).fold(Dots.empty)(m => Dots.from(m.keys))

  def get(k: K)(using Lattice[V]): Option[V] = inner.get(k).flatMap(ObserveRemoveMap.read)

  def queryAllEntries(using Lattice[V]): Iterable[V] = inner.values.flatMap(ObserveRemoveMap.read)

  def entries(using Lattice[V]): Iterable[(K, V)] = inner.view.flatMap((k, m) => ObserveRemoveMap.read(m).map(k -> _))

  def keySet: Set[K] = inner.keySet

  def size: Int = inner.size

  /** merges `v` into the current value stored in the map */
  def update(k: K, v: V)(using replicaId: ReplicaId): Delta =
    ObserveRemoveMap(Map(k -> Map(observed.nextDot(replicaId.uid) -> v)), Dots.empty)

  def transform(k: K)(m: Option[V] => Option[V])(using ReplicaId, Lattice[V]): Delta =
    m(get(k)) match {
      case Some(value) => update(k, value)
      case None        => remove(k)
    }

  def remove(k: K): Delta =
    ObserveRemoveMap(Map.empty, dotsOf(k))

  def removeAll(keys: Iterable[K]): Delta =
    ObserveRemoveMap(Map.empty, keys.map(dotsOf).foldLeft(Dots.empty)(_ `union` _))

  def removeBy(cond: K => Boolean): Delta =
    removeAll(inner.keys.filter(cond))

  def removeByValue(cond: V => Boolean)(using Lattice[V]): Delta =
    removeAll(entries.collect { case (k, v) if cond(v) => k })

  def clear(): Delta =
    removeAll(inner.keys)
}

object ObserveRemoveMap {

  private def read[V: Lattice](versions: Map[Dot, V]): Option[V] = versions.values.reduceOption(_ `merge` _)

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Map.empty, Dots.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given lattice[K, V: {Lattice}]: Lattice[ObserveRemoveMap[K, V]] =
    DecoratedLattice.filter[ObserveRemoveMap[K, V]](Lattice.derived) { (base, other) =>
      if other.removed.isEmpty then base
      else
          base.copy(inner =
            base.inner.flatMap { (k, versions) =>
              val remaining = versions.filter((d, _) => !other.removed.contains(d))
              Option.when(remaining.nonEmpty)(k -> remaining)
            }
          )
    }

  given decompose[K, V: Decompose]: Decompose[ObserveRemoveMap[K, V]] = Decompose.derived

  /** a buffered value is redundant if its dot is removed, or if the delta has a value for the same key that subsumes it */
  given redundancy[K, V: Redundancy]: Redundancy[ObserveRemoveMap[K, V]] = (delta, bufferedDelta) =>
    bufferedDelta.inner.forall((key, versions) =>
      versions.forall((dot, value) =>
        delta.removed.contains(dot)
        || delta.inner.get(key).exists(_.values.exists(Redundancy[V].isRedundant(_, value)))
      )
    ) && delta.removed.contains(bufferedDelta.removed)

}
