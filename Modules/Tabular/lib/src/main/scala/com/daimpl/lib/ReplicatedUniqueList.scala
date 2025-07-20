package com.daimpl.lib

import rdts.base.*
import rdts.datatypes.ReplicatedList
import rdts.time.{CausalTime, Dot, Dots}

case class ReplicatedUniqueList[E](
  inner: ReplicatedList[E]
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
    copy(inner = inner.removeIndex(fromIndex) `merge` inner.insertAt(toIndex, element))

  def insertAt(index: Int, elem: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.insertAt(index, elem))

  def insertAll(index: Int, elems: Iterable[E])(using LocalUid): ReplicatedUniqueList[E] =
    copy(inner = inner.insertAll(index, elems))

  def delete(index: Int): ReplicatedUniqueList[E] =
    copy(inner = inner.delete(index))

  def removeIndex(index: Int): ReplicatedUniqueList[E] =
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
      )
    )
  }
}

object ReplicatedUniqueList {
  given decompose[E]: Decompose[ReplicatedUniqueList[E]] = Decompose.derived
  given bottom[E]: Bottom[ReplicatedUniqueList[E]] = Bottom.derived
  def empty[E]: ReplicatedUniqueList[E] = bottom.empty

  private given undecoratedLattice[E]: Lattice[ReplicatedUniqueList[E]] = Lattice.derived

  given lattice[E]: DecoratedLattice[ReplicatedUniqueList[E]](undecoratedLattice) with {
    override def filter(base: ReplicatedUniqueList[E], other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = base.filter(other)
    override def compact(merged: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = merged.copy(inner = merged.inner.compact)
  }
}
