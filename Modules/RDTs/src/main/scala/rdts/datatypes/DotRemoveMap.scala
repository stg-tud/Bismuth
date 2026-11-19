package rdts.datatypes

import rdts.base.*
import rdts.time.{Dot, Dots}

/** A collection of values identified by dots.
  *
  * Inserting a value generates a fresh dot that acts as the unique id of that value.
  * Updates merge into the value of an existing id, removes add the id to `removed`, a removed id can never be used again.
  *
  * Concurrent inserts produce distinct elements, a remove wins against a concurrent update of the same element.
  */
case class DotRemoveMap[V](elements: Map[Dot, V], removed: Dots) {

  type Delta = DotRemoveMap[V]

  lazy val observed: Dots = removed `union` Dots.from(elements.keys)

  def contains(id: Dot): Boolean = elements.contains(id)

  def get(id: Dot): Option[V] = elements.get(id)

  def ids: Set[Dot] = elements.keySet

  def values: Iterable[V] = elements.values

  def size: Int = elements.size

  /** Inserts `value` under a fresh id. Use [[nextId]] to know the id in advance. */
  def insert(value: V)(using ReplicaId): Delta = DotRemoveMap(Map(nextId -> value), Dots.empty)

  /** Returns the id the next [[insert]] by the local replica will use. */
  def nextId(using replicaId: ReplicaId): Dot = observed.nextDot(replicaId.uid)

  /** merges `value` into the value stored under `id`, does nothing if `id` is not present */
  def update(id: Dot, value: V): Delta =
    if contains(id) then DotRemoveMap(Map(id -> value), Dots.empty)
    else DotRemoveMap.empty

  /** applies `f` to the value stored under `id` and stores the result as a delta, does nothing if `id` is not present */
  def transform(id: Dot)(f: V => V): Delta =
    get(id).fold(DotRemoveMap.empty)(v => update(id, f(v)))

  def remove(id: Dot): Delta = removeAll(List(id))

  def removeAll(ids: Iterable[Dot]): Delta = DotRemoveMap(Map.empty, Dots.from(ids))

  def removeBy(cond: V => Boolean): Delta = removeAll(elements.collect { case (id, v) if cond(v) => id })

  def clear(): Delta = removeAll(elements.keys)
}

object DotRemoveMap {

  def empty[V]: DotRemoveMap[V] = DotRemoveMap(Map.empty, Dots.empty)

  given bottom[V]: Bottom[DotRemoveMap[V]] = Bottom.derived

  given lattice[V: Lattice]: Lattice[DotRemoveMap[V]] =
    DecoratedLattice.filter[DotRemoveMap[V]](Lattice.derived) { (base, other) =>
      if other.removed.isEmpty then base
      else base.copy(elements = base.elements.filter((id, _) => !other.removed.contains(id)))
    }

  given decompose[V: Decompose]: Decompose[DotRemoveMap[V]] = Decompose.derived

  /** `delta` makes `bufferedDelta` redundant if it removes or subsumes all of its elements and all of its removes */
  given redundancy[V: Redundancy]: Redundancy[DotRemoveMap[V]] = (delta, bufferedDelta) =>
    bufferedDelta.elements.forall((id, value) =>
      delta.removed.contains(id) || delta.elements.get(id).exists(Redundancy[V].isRedundant(_, value))
    ) && delta.removed.contains(bufferedDelta.removed)
}
