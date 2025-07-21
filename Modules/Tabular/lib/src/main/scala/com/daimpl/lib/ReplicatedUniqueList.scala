package com.daimpl.lib

import rdts.base.*
import rdts.datatypes.{ObserveRemoveMap, ReplicatedList, LastWriterWins}
import rdts.time.{CausalTime, Dot, Dots}

case class ReplicatedUniqueList[E](
  inner: ReplicatedList[E],
  markers: ObserveRemoveMap[Uid, LastWriterWins[Dot]]
){
  lazy val now: Option[CausalTime] =
    inner.now

  lazy val observed: Dots =
    inner.observed

  lazy val toList: List[E] =
    inner.toList

  def size: Int = inner.size

  def toLazyList: LazyList[E] =
    inner.toLazyList

  def read(i: Int): Option[E] =
    inner.read(i)

  def move(fromIndex: Int, toIndex: Int)(using LocalUid): ReplicatedUniqueList[E] =
    val element = read(fromIndex).get
    copy(inner = inner.removeIndex(fromIndex) `merge` inner.insertAt(toIndex, element)) // wegen der marker, sollte das jetzt besser this und nicht inner sein?

  def insertAt(index: Int, elem: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.insertAt(index, elem))

  def insertAll(index: Int, elems: Iterable[E])(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.insertAll(index, elems))

  def removeAt(index: Int): ReplicatedUniqueList[E] =
    copy(inner = inner.removeIndex(index))

  def appendAll(elements: Iterable[E])(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.appendAll(elements))

  def prependAll(e: Iterable[E])(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.prependAll(e))

  def prepend(e: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.prepend(e))

  def append(e: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.append(e))

  def update(index: Int, elem: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.update(index, elem))

  def deleteBy(test: E => Boolean): ReplicatedUniqueList[E] =
    copy(inner = inner.deleteBy(test))

  def addMarker(id: Uid, index: Int)(using LocalUid): ReplicatedUniqueList[E] =
    copy(markers = markers.update(
      id,
      LastWriterWins(CausalTime.now(), inner.dotList(index + 1)))
    )

  def removeMarker(id: Uid): ReplicatedUniqueList[E] =
    copy(markers = markers.remove(id))

  def getMarker(id: Uid): Option[Int] = {
    markers.get(id).flatMap { marker =>
      val idx = inner.dotList.indexOf(marker.value)
      if idx == -1 then None else Some(idx - 1)
    }
  }

  def filter(other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
  {
    val combinedElements = inner.elements.toList ++ other.inner.elements.toList
    val combinedTimes = (inner.times.toList ++ other.inner.times.toList).toMap

    val latestDotPerUniqueValue =
      Dots.from(
        combinedElements
          .groupBy(_._2)
          .flatMap{
            (_, entries) => Option(entries.maxBy{ (dot, _) => combinedTimes(dot) }._1)
          }
      )

    val localDots = Dots.from(inner.elements.keys)
    val dotsForDuplicateElements = localDots.diff(latestDotPerUniqueValue)

    copy(
      inner.copy(
        removed = dotsForDuplicateElements `union` inner.removed
      ),
      markers.copy(inner = markers.inner.filter((_, e) => !other.markers.removed.subsumes(e.dots)))
    )
  }
}

object ReplicatedUniqueList {
  given decompose[E]: Decompose[ReplicatedUniqueList[E]] = {
    given Decompose[ObserveRemoveMap[Uid, LastWriterWins[Dot]]] = Decompose.atomic
    Decompose.derived
  }

  given bottom[E]: Bottom[ReplicatedUniqueList[E]] = Bottom.derived
  def empty[E]: ReplicatedUniqueList[E] = bottom.empty

  private given undecoratedLattice[E]: Lattice[ReplicatedUniqueList[E]] = Lattice.derived

  given lattice[E]: DecoratedLattice[ReplicatedUniqueList[E]](undecoratedLattice) with {
    override def filter(base: ReplicatedUniqueList[E], other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = base.filter(other)
    override def compact(merged: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = merged.copy(inner = merged.inner.compact)
  }
}
