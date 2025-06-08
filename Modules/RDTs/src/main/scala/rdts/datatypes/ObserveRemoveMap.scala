package rdts.datatypes

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{Dotted, HasDots, Obrem}
import rdts.time.Dots

case class ObserveRemoveMap[K, V](repr: Obrem[Map[K, V]]) {

  val inner: Map[K, V] = repr.data

  export inner.get

  type Delta = ObserveRemoveMap[K, V]

  def contains(k: K): Boolean = inner.contains(k)

  def queryKey[A](using Bottom[V])(k: K): V = {
    inner.getOrElse(k, Bottom[V].empty)
  }

  def queryAllEntries: Iterable[V] = inner.values

  def entries: Iterable[(K, V)] = inner.view

  def update(using LocalUid)(k: K, v: V): Delta = {
    ObserveRemoveMap(Obrem(Map(k -> v), Dots.single(repr.context.nextDot(LocalUid.replicaId)), Dots.empty))
  }

  def transformPlain(using LocalUid)(k: K)(m: Option[V] => Option[V]): Delta = {
    m(inner.get(k)) match {
      case Some(value) => update(k, value)
      case None => ObserveRemoveMap(Obrem(Map.empty, Dots.single(repr.context.nextDot(LocalUid.replicaId)), Dots.empty))
    }
  }

  def remove(using HasDots[V])(k: K): Delta = {
    inner.get(k) match
      case Some(value) =>
        ObserveRemoveMap(Obrem(
          Map.empty,
          observed = Dots.empty,
          deletions = HasDots[V].dots(value)
        ))
      case None => ObserveRemoveMap.empty

  }

  def removeAll(using Bottom[V], HasDots[V])(keys: Iterable[K]): Delta = {
    val values = keys.map(k => inner.getOrElse(k, Bottom[V].empty))
    val dots   = values.foldLeft(Dots.empty) {
      case (set, v) => set `union` HasDots[V].dots(v)
    }

    ObserveRemoveMap(Obrem(
      Map.empty,
      observed = Dots.empty,
      deletions = dots
    ))
  }

  def removeByValue(using HasDots[V])(cond: V => Boolean): Delta = {
    val toRemove = inner.values.collect {
      case v if cond(v) => v.dots
    }.fold(Dots.empty)(_ `union` _)

    ObserveRemoveMap(Obrem(
      Map.empty,
      observed = Dots.empty,
      deletions = toRemove
    ))
  }

  def clear(using HasDots[V])(): Delta = {
    ObserveRemoveMap(Obrem(
      Map.empty,
      observed = Dots.empty,
      deletions = inner.dots
    ))
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

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Obrem(Map.empty, Dots.empty, Dots.empty))

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given lattice[K, V: {Lattice, HasDots}]: Lattice[ObserveRemoveMap[K, V]] =
    Lattice.derived
}
