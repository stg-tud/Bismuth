package rdts.datatypes

import rdts.base.*
import rdts.time.{Dot, Dots}

/** Observed-remove map.
  *
  * Each key maps to one or more dots, each dot carrying its own value. Values of a key are merged on read.
  *  - A key normally has a single dot. Multiple dots only exist after concurrent inserts of the same key.
  *  - Updating a present key reuses its newest dot (by [[Dot.ordering]]). If the key has multiple dots,
  *    the update collapses them: the merged value is written under the newest dot and the other observed dots are removed.
  *  - Inserting an absent key generates a fresh dot.
  *  - Removing a key removes exactly the observed dots of that key.
  *
  * Consequently, a concurrent insert of an absent key wins against a remove (add-wins),
  * but a concurrent update of a present key loses against a remove (remove-wins), as the update reuses an observed dot.
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
  def update(k: K, v: V)(using replicaId: ReplicaId, lattice: Lattice[V]): Delta =
    inner.get(k).filter(_.nonEmpty) match
        case None =>
          ObserveRemoveMap(Map(k -> Map(observed.nextDot(replicaId.uid) -> v)), Dots.empty)
        case Some(versions) if versions.sizeIs == 1 =>
          ObserveRemoveMap(Map(k -> Map(versions.head._1 -> v)), Dots.empty)
        case Some(versions) =>
          val newest = versions.keys.max(using Dot.ordering)
          val value  = versions.values.reduce(_ `merge` _) `merge` v
          ObserveRemoveMap(Map(k -> Map(newest -> value)), Dots.from(versions.keys.filter(_ != newest)))

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

  given redundancy[K, V: Redundancy]: Redundancy[ObserveRemoveMap[K, V]] = (delta, bufferedDelta) =>
    bufferedDelta.inner.forall((key, versions) =>
      versions.forall((dot, value) =>
        delta.removed.contains(dot)
        || delta.inner.get(key).flatMap(_.get(dot)).exists(Redundancy[V].isRedundant(_, value))
      )
    ) && delta.removed.contains(bufferedDelta.removed)

}
