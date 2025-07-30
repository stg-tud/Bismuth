package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.{ElementId, MarkerId, MarkerRemovalBehavior, OpPrecedence}
import rdts.base.*
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedList}
import rdts.time.{CausalTime, Dot, Dots}

case class ReplicatedUniqueList[E](
  elementIdsAndLastOperation: ReplicatedList[(elementId: ElementId, positionOpPrecedence: OpPrecedence.Positional, contentOpPrecedence: OpPrecedence.Textual)] =
    ReplicatedList.empty[(elementId: ElementId, positionOpPrecedence: OpPrecedence.Positional, contentOpPrecedence: OpPrecedence.Textual)],
  elementIdToValue: ObserveRemoveMap[ElementId, LastWriterWins[E]] =
    ObserveRemoveMap.empty[ElementId, LastWriterWins[E]],
  markerIdToElementIdAndBehavior: ObserveRemoveMap[MarkerId, LastWriterWins[(elementId: ElementId, markerRemovalBehavior: MarkerRemovalBehavior)]] =
    ObserveRemoveMap.empty[MarkerId, LastWriterWins[(elementId: ElementId, markerRemovalBehavior: MarkerRemovalBehavior)]]
){
  lazy val now: Option[CausalTime] =
    elementIdsAndLastOperation.now

  lazy val observed: Dots =
    elementIdsAndLastOperation.observed

  lazy val toList: List[E] =
    elementIdsAndLastOperation.toList.map(x => elementIdToValue.get(x.elementId).get.value)

  def size: Int = elementIdsAndLastOperation.size

  def toLazyList: LazyList[E] =
    elementIdsAndLastOperation.toLazyList.map(x => elementIdToValue.get(x.elementId).get.value)

  private def markersImpactedByChangeAt(index: Int) =
    val elementId = elementIdsAndLastOperation.read(index).get.elementId
    val impactedMarkers = markerIdToElementIdAndBehavior.entries.filter { (_, marker) => marker.value.elementId == elementId }.toList
    impactedMarkers

  def read(i: Int): Option[E] =
    elementIdsAndLastOperation.read(i).map(x => elementIdToValue.get(x.elementId).get.value)

  def move(fromIndex: Int, toIndex: Int)(using LocalUid): ReplicatedUniqueList[E] =
    println(s"[${LocalUid.replicaId}] moving $fromIndex to $toIndex\n")

    val elementToMove = elementIdsAndLastOperation.read(fromIndex).get

    val payloadDelta = copy(
      elementIdsAndLastOperation =
        elementIdsAndLastOperation.removeIndex(fromIndex)
        `merge` elementIdsAndLastOperation.insertAt(toIndex, (elementToMove.elementId, OpPrecedence.Positional.Move, OpPrecedence.Textual.None))
    )

    markersImpactedByChangeAt(fromIndex).map((id, lww) => addMarker(id, fromIndex, lww.value.markerRemovalBehavior))
      .foldLeft(payloadDelta)(
        (accumulator, other)
        => accumulator.merge(ReplicatedUniqueList[E](markerIdToElementIdAndBehavior = other.markerIdToElementIdAndBehavior))
      )

  def insertAt(index: Int, element: E)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] inserting at $index\n")

    val elementId = elementIdsAndLastOperation.observed.nextDot(LocalUid.replicaId)
    copy(
      elementIdsAndLastOperation = elementIdsAndLastOperation.insertAt(index, (elementId, OpPrecedence.Positional.None, OpPrecedence.Textual.Insert)),
      elementIdToValue = elementIdToValue.update(elementId, LastWriterWins(CausalTime.now(), element))
    )
  }

  def removeAt(index: Int)(using LocalUid): ReplicatedUniqueList[E] =
  {
    println(s"[${LocalUid.replicaId}] removing at $index\n")

    val payloadDelta = copy(
      elementIdsAndLastOperation = elementIdsAndLastOperation.removeIndex(index),
      elementIdToValue = elementIdToValue.remove(elementIdsAndLastOperation.read(index).get.elementId)
    )

    val inRange = (i: Int) => if (0 until elementIdsAndLastOperation.size) contains i then Some(i) else None

    markersImpactedByChangeAt(index).map(
      (id, content) =>
        val behavior = content.value.markerRemovalBehavior
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

  def update(index: Int, elementValue: E)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] updating at $index to $elementValue\n")

    val elementMetadata = elementIdsAndLastOperation.read(index).get
    copy(
      elementIdsAndLastOperation = elementIdsAndLastOperation.update(index, (elementMetadata.elementId, OpPrecedence.Positional.Update, OpPrecedence.Textual.Update)),
      elementIdToValue = elementIdToValue.transform(elementMetadata.elementId)(_.map(_.write(elementValue)))
    )
  }

  def addMarker(id: MarkerId, index: Int, removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None)(using LocalUid): ReplicatedUniqueList[E] =
    copy(
      markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.update(
        id,
        LastWriterWins(CausalTime.now(), (elementIdsAndLastOperation.read(index).get.elementId, removalBehavior))
      ),
    )

  def removeMarker(id: MarkerId): ReplicatedUniqueList[E] =
    copy(markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.remove(id))

  def getMarker(id: MarkerId): Option[Int] = {
    markerIdToElementIdAndBehavior.get(id).flatMap { marker =>
      val elementId = marker.value.elementId
      val index = elementIdsAndLastOperation.toList.indexWhere(_.elementId == elementId)
      if index == -1 then None else Some(index)
    }
  }

  def filter(other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
  {
    val combinedElementMetadata = elementIdsAndLastOperation.elements.toList ++ other.elementIdsAndLastOperation.elements.toList
    val combinedDotToElementMetadata = combinedElementMetadata.toMap
    val combinedTimes = (elementIdsAndLastOperation.times.toList ++ other.elementIdsAndLastOperation.times.toList).toMap
    val combinedRemoved = elementIdsAndLastOperation.removed `union` other.elementIdsAndLastOperation.removed

    val updatedDots = Dots.from(
      combinedRemoved.toSet.filter{ removedDot =>
        combinedDotToElementMetadata.get(removedDot) match
        {
          case None => false
          case Some(candidateMetadata) =>
            combinedElementMetadata.exists{ (candidateDot, elementMetadata) =>
              elementMetadata.elementId == candidateMetadata.elementId
              && elementMetadata.positionOpPrecedence == OpPrecedence.Positional.Update
              && candidateDot != removedDot
              && {
                val otherDotTimestamp = combinedTimes(candidateDot)
                (otherDotTimestamp `merge` combinedTimes(removedDot)) == otherDotTimestamp // timestampOther > timestampThis
              }
            }
        }
        // returns whether there is another dot for this element, caused by an update with a more recent timestamp
      }
    )

    val localTrulyRemovedExclUpdated = elementIdsAndLastOperation.removed `diff` updatedDots
    val combinedTrulyRemovedExclUpdated = combinedRemoved `diff` updatedDots

    def latestMostPrivilegedDotPerUniqueValue[P: Ordering](
      precedenceSelector: ((positionOpPrecedence: OpPrecedence.Positional, contentOpPrecedence: OpPrecedence.Textual)) => P
    ): Dots =
    {
      Dots.from(
        combinedElementMetadata
          .groupBy{_._2.elementId}
          .flatMap{
            (_, entries) => entries
              .filterNot{   (dot, _) => combinedTrulyRemovedExclUpdated.contains(dot) }
              .maxByOption{ (dot, elementMetadata) => (
                precedenceSelector(elementMetadata.positionOpPrecedence, elementMetadata.contentOpPrecedence),
                combinedTimes(dot)
              )}
          }.keys
      )
    }

    val localDots = Dots.from(elementIdsAndLastOperation.elements.keys)
    val dotsForLocalDuplicateElementIds    = localDots.diff(latestMostPrivilegedDotPerUniqueValue{_.positionOpPrecedence})
    val dotsForLocalDuplicateElementValues = localDots.diff(latestMostPrivilegedDotPerUniqueValue{_.contentOpPrecedence})

    copy(
      elementIdsAndLastOperation = elementIdsAndLastOperation.copy(
        removed = localTrulyRemovedExclUpdated `union` dotsForLocalDuplicateElementIds
      ),
      elementIdToValue = elementIdToValue.copy(
        inner = elementIdToValue.inner.filterNot{ (_, mapEntry) =>
          dotsForLocalDuplicateElementValues.subsumes(mapEntry.dots)
        }
      ),
      markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.copy(
        inner = markerIdToElementIdAndBehavior.inner.filterNot{ (_, mapEntry) =>
          other.markerIdToElementIdAndBehavior.removed.subsumes(mapEntry.dots)
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

  object OpPrecedence {
    given enumOrdinalOrdering[E <: scala.reflect.Enum]: Ordering[E] =
      Ordering.by[E, Int](_.ordinal)

    enum Positional:
      case None, Update, Move

    enum Textual:
      case None, Insert, Update
  }

  enum MarkerRemovalBehavior:
    case Predecessor, Successor, None

  given bottom[E]: Bottom[ReplicatedUniqueList[E]] = Bottom.derived
  def empty[E]: ReplicatedUniqueList[E] = bottom.empty

  private given undecoratedLattice[E]: Lattice[ReplicatedUniqueList[E]] = Lattice.derived

  given lattice[E]: DecoratedLattice[ReplicatedUniqueList[E]](undecoratedLattice) with {
    override def filter(base: ReplicatedUniqueList[E], other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = base.filter(other)
    override def compact(merged: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = merged.copy(elementIdsAndLastOperation = merged.elementIdsAndLastOperation.compact)
  }
}
