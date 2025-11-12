package rdts.datatypes

import rdts.base.{Bottom, Decompose, DecoratedLattice, Historized, Lattice, LocalUid}
import rdts.time.{Dot, Dots}

/** A set that allows deletes.
  * Each unique element tracks the dots of when it was inserted.
  * Removals do not override concurrent inserts.
  */
case class ReplicatedSet[E](inner: Map[E, Dots], deleted: Dots) {

  type Delta = ReplicatedSet[E]

  def elements: Set[E] = inner.keySet

  def contains(elem: E): Boolean = inner.contains(elem)

  lazy val observed: Dots = inner.values.foldLeft(deleted)(_ `union` _)

  def add(using LocalUid)(e: E): Delta = {
    val nextDot = observed.nextDot(LocalUid.replicaId)
    val v: Dots = inner.getOrElse(e, Dots.empty)

    ReplicatedSet(Map(e -> Dots.single(nextDot)), v)
  }

  def addAll(using LocalUid)(elems: Iterable[E]): Delta = {
    val nextCounter = observed.nextTime(LocalUid.replicaId)
    val nextDots    = Dots.from((nextCounter until nextCounter + elems.size).map(Dot(LocalUid.replicaId, _)))

    val ccontextSet = elems.flatMap(inner.get).foldLeft(Dots.empty)(_ `union` _)

    ReplicatedSet(
      (elems zip nextDots.iterator.map(dot => Dots.single(dot))).toMap,
      ccontextSet
    )
  }

  def remove(e: E): Delta = {
    val v = inner.getOrElse(e, Dots.empty)
    ReplicatedSet(Map.empty, v)
  }

  def removeAll(elems: Iterable[E]): Delta = {
    val dotsToRemove = elems.foldLeft(Dots.empty) {
      case (dots, e) => inner.get(e) match {
          case Some(ds) => dots `union` ds
          case None     => dots
        }
    }

    ReplicatedSet(Map.empty, dotsToRemove)
  }

  def removeBy(cond: E => Boolean): Delta = {
    val removedDots = inner.collect {
      case (k, v) if cond(k) => v
    }.foldLeft(Dots.empty)(_ `union` _)

    ReplicatedSet(Map.empty, removedDots)
  }

  def clear(): Delta = ReplicatedSet(Map.empty, inner.values.foldLeft(Dots.empty)(_ `union` _))

}

object ReplicatedSet {

  def empty[E]: ReplicatedSet[E] = ReplicatedSet(Map.empty, Dots.empty)

  given bottom[E]: Bottom[ReplicatedSet[E]]   = Bottom.provide(empty)
  given lattice[E]: Lattice[ReplicatedSet[E]] = DecoratedLattice.filter[ReplicatedSet[E]](Lattice.derived) {
    (base, other) =>
      base.copy(inner = base.inner.filter((_, dots) => !other.deleted.subsumes(dots)))
  }
  given decompose[E]: Decompose[ReplicatedSet[E]] = Decompose.derived

  given historized[E]: Historized[ReplicatedSet[E]] = (delta, bufferedDelta) =>
    delta.deleted.contains(bufferedDelta.observed)

}
