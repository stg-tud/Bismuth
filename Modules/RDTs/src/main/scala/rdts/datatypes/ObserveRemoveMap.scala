package rdts.datatypes

import rdts.base.Historized.MetaDelta
import rdts.datatypes.ObserveRemoveMap.Entry
import rdts.base.{Bottom, Decompose, DecoratedLattice, Historized, Lattice, LocalUid}
import rdts.time.Dots

case class ObserveRemoveMap[K, V](inner: Map[K, Entry[V]], removed: Dots) {

  lazy val observed: Dots = removed.union:
    inner.values.foldLeft(Dots.empty) {
      case (set, v) => set `union` v.dots
    }

  type Delta = ObserveRemoveMap[K, V]

  def contains(k: K): Boolean = inner.contains(k)

  def get(k: K): Option[V] = inner.get(k).map(_.value)

  def queryAllEntries: Iterable[V] = inner.values.map(_.value)

  def entries: Iterable[(K, V)] = inner.view.mapValues(_.value)

  /** merges `v` into the current value stored in the map */
  def update(k: K, v: V)(using LocalUid): Delta = {
    val next = Dots.single(observed.nextDot(LocalUid.replicaId))
    ObserveRemoveMap(
      Map(k -> Entry(next, v)),
      Dots.empty
    )
  }

  def transform(k: K)(m: Option[V] => Option[V])(using LocalUid): Delta = {
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

  def removeBy(cond: K => Boolean): Delta = {
    val toRemove = inner.collect {
      case (k, v) if cond(k) => v.dots
    }.fold(Dots.empty)(_ `union` _)
    ObserveRemoveMap(Map.empty, toRemove)
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

object ObserveRemoveMap {

  case class Entry[V](dots: Dots, value: V)
  object Entry {
    given bottom[V: Bottom]: Bottom[Entry[V]]          = Bottom.derived
    given lattice[V: Lattice]: Lattice[Entry[V]]       = Lattice.derived
    given decompose[V: Decompose]: Decompose[Entry[V]] = Decompose.derived
  }

  def empty[K, V]: ObserveRemoveMap[K, V] = ObserveRemoveMap(Map.empty, Dots.empty)

  given bottom[K, V]: Bottom[ObserveRemoveMap[K, V]] = Bottom.derived

  given lattice[K, V: {Lattice}]: Lattice[ObserveRemoveMap[K, V]] =
    DecoratedLattice.filter[ObserveRemoveMap[K, V]](Lattice.derived) { (base, other) =>
      base.copy(inner = base.inner.filter((_, e) => !other.removed.subsumes(e.dots)))
    }

  given decompose[K, V: Decompose]: Decompose[ObserveRemoveMap[K, V]] = Decompose.derived

  given historized[K, V: Historized]: Historized[ObserveRemoveMap[K, V]] = (delta: ObserveRemoveMap[K, V], buffer: Iterable[MetaDelta[ObserveRemoveMap[K, V]]]) => {
    println(s"get redundant dots for OR-Map:\n\tdelta: $delta\n\tbuffer: ${buffer.size} $buffer")
    // all values that are removed by the new delta
    // TODO: was passiert bei gleichzeitigen remove und update, wenn update erst später gesehen wird
    // was passiert wenn nicht causal stability, kann ich die deltas wirklich wegschmeißen?
    var redundant = buffer.filter(bufferedDelta => delta.removed.contains(bufferedDelta.delta.observed)).getAllDots

    // all entries that are updated by the new delta
    // the new delta must contain all keys from the delta in the buffer (no partial updates)
    redundant = redundant.union(
      buffer.filter(bufferedDelta => bufferedDelta.delta.inner.keys.forall(k => delta.contains(k))) // only look at deltas in the buffer which keys are contained by the new delta
        .foldLeft(Dots.empty)((dots, bufferedDelta) =>
          dots.union(
            bufferedDelta.delta.entries.foldLeft(Dots.empty)((dots, entry) => // iterate over all keys of the delta in the buffer
              // TODO intersect instead of union
              dots.union(delta.get(entry._1).get.getRedundantDeltas(
                buffer.filter(bufferedDelta => bufferedDelta.delta.contains(entry._1)).mapDeltas(_.get(entry._1).get)
              ))
            )
          )
        )
    )

    println(f"\tredundant: $redundant")
    redundant
  }

}
