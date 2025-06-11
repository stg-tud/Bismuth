package rdts.datatypes

import rdts.base.{Bottom, DecoratedLattice, Lattice, LocalUid}
import rdts.datatypes.ObserveRemoveMap.Entry
import rdts.time.Dots

case class ObserveRemoveMap[K, V](inner: Map[K, Entry[V]], removed: Dots) {

  lazy val observed: Dots = inner.values.foldLeft(Dots.empty) {
    case (set, v) => set `union` v.dots
  }

  type Delta = ObserveRemoveMap[K, V]

  def contains(k: K): Boolean = inner.contains(k)

  def get(k: K): Option[V] = inner.get(k).map(_.value)

  def queryAllEntries: Iterable[V] = inner.values.map(_.value)

  def entries: Iterable[(K, V)] = inner.view.mapValues(_.value)

  /** merges `v` into the current value stored in the map */
  def update(using LocalUid)(k: K, v: V): Delta = {
    val next = Dots.single(observed.nextDot(LocalUid.replicaId))
    ObserveRemoveMap(
      Map(k -> Entry(next, v)),
      Dots.empty
    )
  }

  def transformPlain(using LocalUid)(k: K)(m: Option[V] => Option[V]): Delta = {
    m(inner.get(k).map(_.value)) match {
      case Some(value) => update(k, value)
      case None        => remove(k)
    }
  }

  def remove(k: K): Delta = {
    ObserveRemoveMap(Map.empty, inner.get(k).map(_.dots).getOrElse(Dots.empty))
  }

  def removeAll(keys: Iterable[K]): Delta = {
    val rem = keys.flatMap(inner.get).map(_.dots).foldLeft(Dots.empty)(_ `union` _)
    ObserveRemoveMap(Map.empty, rem)
  }

  def removeByValue(cond: V => Boolean): Delta = {
    val toRemove = inner.values.collect {
      case v if cond(v.value) => v.dots
    }.fold(Dots.empty)(_ `union` _)

    ObserveRemoveMap(
      Map.empty,
      toRemove
    )
  }

  def clear(): Delta = {
    ObserveRemoveMap(Map.empty, inner.values.map(_.dots).foldLeft(Dots.empty)(_ `union` _))
  }
}

/** An ObserveRemoveMap (Observed-Remove Map) is a Delta CRDT that models a map from an arbitrary key type to nested causal Delta CRDTs.
  * In contrast to [[GrowOnlyMap]], ObserveRemoveMap allows the removal of key/value pairs from the map.
  *
  * The nested CRDTs can be queried/mutated by calling the queryKey/mutateKey methods with a DeltaQuery/DeltaMutator generated
  * by a CRDT Interface method of the nested CRDT. For example, to enable a nested EWFlag, one would pass `EWFlagInterface.enable()`
  * as the DeltaMutator to mutateKey.
  */
object ObserveRemoveMap {

  case class Entry[V](dots: Dots, value: V)
  object Entry {
    given bottom[V: Bottom]: Bottom[Entry[V]]    = Bottom.derived
    given lattice[V: Lattice]: Lattice[Entry[V]] = Lattice.derived
  }

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Map.empty, Dots.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given lattice[K, V: {Lattice}]: Lattice[ObserveRemoveMap[K, V]] =
    DecoratedLattice.filter[ObserveRemoveMap[K, V]](Lattice.derived) { (base, other) =>
      base.copy(inner = base.inner.filter((_, e) => !other.removed.subsumes(e.dots)))
    }
}
