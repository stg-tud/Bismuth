package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.{ElementId, MarkerId, MarkerRemovalBehavior}
import rdts.base.*
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedList}
import rdts.time.{CausalTime, Dot, Dots}

case class ReplicatedUniqueList[E](
  elementIds: ReplicatedList[ElementId] =
    ReplicatedList.empty[ElementId],
  elementIdToValue: ObserveRemoveMap[ElementId, LastWriterWins[E]] =
    ObserveRemoveMap.empty[ElementId, LastWriterWins[E]],
  markerIdToElementIdAndBehavior: ObserveRemoveMap[MarkerId, LastWriterWins[(ElementId, MarkerRemovalBehavior)]] =
    ObserveRemoveMap.empty[MarkerId, LastWriterWins[(ElementId, MarkerRemovalBehavior)]]
){
  lazy val now: Option[CausalTime] =
    elementIds.now

  lazy val observed: Dots =
    elementIds.observed

  lazy val toList: List[E] =
    elementIds.toList.map(elementIdToValue.get(_).get.value)

  def size: Int = elementIds.size

  def toLazyList: LazyList[E] =
    elementIds.toLazyList.map(elementIdToValue.get(_).get.value)

  private def markersImpactedByChangeAt(index: Int) =
    val elementId = elementIds.read(index).get
    val impactedMarkers = markerIdToElementIdAndBehavior.entries.filter { (_, marker) => marker.value._1 == elementId }.toList
    impactedMarkers

  def read(i: Int): Option[E] =
    elementIds.read(i).map(elementIdToValue.get(_).get.value)

  def move(fromIndex: Int, toIndex: Int)(using LocalUid): ReplicatedUniqueList[E] =
    val elementToMove = elementIds.read(fromIndex).get

    val payloadDelta = copy(elementIds = elementIds.removeIndex(fromIndex) `merge` elementIds.insertAt(toIndex, elementToMove))

    markersImpactedByChangeAt(fromIndex).map((id, lww) => addMarker(id, fromIndex, lww.value._2))
      .foldLeft(payloadDelta)(
        (accumulator, other)
        => accumulator.merge(ReplicatedUniqueList[E](markerIdToElementIdAndBehavior = other.markerIdToElementIdAndBehavior))
      )

  def insertAt(index: Int, element: E)(using LocalUid): ReplicatedUniqueList[E] = {
    val elementId = elementIds.observed.nextDot(LocalUid.replicaId)
    copy(
      elementIds = elementIds.insertAt(index, elementId),
      elementIdToValue = elementIdToValue.update(elementId, LastWriterWins(CausalTime.now(), element))
    )
  }

  def removeAt(index: Int)(using LocalUid): ReplicatedUniqueList[E] =
  {
    val payloadDelta = copy(
      elementIds = elementIds.removeIndex(index),
      elementIdToValue = elementIdToValue.remove(elementIds.read(index).get)
    )

    val inRange = (i: Int) => if (0 until elementIds.size) contains i then Some(i) else None

    markersImpactedByChangeAt(index).map(
      (id, content) =>
        val behavior = content.value._2
        (
          id,
          behavior match
            case MarkerRemovalBehavior.Predecessor => inRange(index - 1)
            case MarkerRemovalBehavior.Successor   => inRange(index + 1)
            case MarkerRemovalBehavior.None        => None,
          behavior
        )
    )
    .map(
      (id, optIdx, behavior) =>
        optIdx match
          case Some(newIdx) => addMarker(id, newIdx, behavior)
          case None         => removeMarker(id)
    )
    .foldLeft(payloadDelta)(
      (accumulator, other)
      => accumulator.merge(ReplicatedUniqueList[E](markerIdToElementIdAndBehavior = other.markerIdToElementIdAndBehavior))
    )
  }

  def append(e: E)(using LocalUid): ReplicatedUniqueList[E] =
    insertAt(size, e)

  def update(index: Int, elem: E)(using LocalUid): ReplicatedUniqueList[E] =
    copy(
      elementIds = elementIds.update(index, elementIds.read(index).get),
      elementIdToValue = elementIdToValue.update(elementIds.read(index).get, LastWriterWins(CausalTime.now(), elem))
    )

  def addMarker(id: MarkerId, index: Int, removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None)(using LocalUid): ReplicatedUniqueList[E] =
    copy(
      markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.update(
        id,
        LastWriterWins(CausalTime.now(), (elementIds.read(index).get, removalBehavior))
      ),
    )

  def removeMarker(id: MarkerId): ReplicatedUniqueList[E] =
    copy(markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.remove(id))

  def getMarker(id: MarkerId): Option[Int] = {
    markerIdToElementIdAndBehavior.get(id).flatMap { marker =>
      val elementId = marker.value._1
      val index = elementIds.toList.indexOf(elementId)
      if index == -1 then None else Some(index)
    }
  }

  def filter(other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
  {
    val combinedElements = elementIds.elements.toList ++ other.elementIds.elements.toList
    val combinedTimes = (elementIds.times.toList ++ other.elementIds.times.toList).toMap

    val latestDotPerUniqueValue =
      Dots.from(
        combinedElements
          .groupBy(_._2)
          .flatMap{
            (_, entries) => Option(entries.maxBy{ (dot, _) => combinedTimes(dot) }._1)
          }
      )

    val localDots = Dots.from(elementIds.elements.keys)
    val dotsForLocalDuplicateElements = localDots.diff(latestDotPerUniqueValue)

    copy(
      elementIds = elementIds.copy(
        removed = dotsForLocalDuplicateElements `union` elementIds.removed
      ),
      elementIdToValue = elementIdToValue.copy(
        inner = elementIdToValue.inner.filterNot{ (_, e) =>
          (dotsForLocalDuplicateElements `union` other.elementIdToValue.removed).subsumes(e.dots)
        }
      ),
      markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.copy(
        inner = markerIdToElementIdAndBehavior.inner.filterNot{ (_, e) =>
          other.markerIdToElementIdAndBehavior.removed.subsumes(e.dots)
        }
      )
    )
  }
}

object ReplicatedUniqueList
{
  type ElementId = Dot
  type MarkerId = Uid

  given decompose[E]: Decompose[ReplicatedUniqueList[E]] = Decompose.derived

  enum MarkerRemovalBehavior:
    case Predecessor, Successor, None

  given bottom[E]: Bottom[ReplicatedUniqueList[E]] = Bottom.derived
  def empty[E]: ReplicatedUniqueList[E] = bottom.empty

  private given undecoratedLattice[E]: Lattice[ReplicatedUniqueList[E]] = Lattice.derived

  given lattice[E]: DecoratedLattice[ReplicatedUniqueList[E]](undecoratedLattice) with {
    override def filter(base: ReplicatedUniqueList[E], other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = base.filter(other)
    override def compact(merged: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = merged.copy(elementIds = merged.elementIds.compact)
  }
}
