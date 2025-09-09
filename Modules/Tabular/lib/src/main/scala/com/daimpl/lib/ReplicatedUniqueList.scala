package com.daimpl.lib

import com.daimpl.lib.ReplicatedUniqueList.OpPrecedence.{Positional, Textual}
import com.daimpl.lib.ReplicatedUniqueList.{ElementId, MarkerId, MarkerValue, MarkerRemovalBehavior, OpPrecedence}
import rdts.base.*
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedList}
import rdts.time.{CausalTime, Dot, Dots}

import scala.util.chaining.scalaUtilChainingOps

case class ReplicatedUniqueList[E](
    elementIdsAndOperations: ReplicatedList[(elementId: ElementId, opPrecedence: OpPrecedence)] =
      ReplicatedList.empty[(elementId: ElementId, opPrecedence: OpPrecedence)],
    elementIdToValue: ObserveRemoveMap[ElementId, LastWriterWins[E]] =
      ObserveRemoveMap.empty[ElementId, LastWriterWins[E]],
    markerIdToElementIdAndBehavior: ObserveRemoveMap[MarkerId, MarkerValue] =
      ObserveRemoveMap.empty[MarkerId, MarkerValue]
) {
  lazy val now: Option[CausalTime] = elementIdsAndOperations.now

  lazy val observed: Dots =
    elementIdsAndOperations.observed
    `union` elementIdToValue.observed
    `union` markerIdToElementIdAndBehavior.observed

  lazy val toList: List[E] = elementIdsAndOperations.toList.map(x => elementIdToValue.get(x.elementId)).map(_.get.value)

  def size: Int = elementIdsAndOperations.size

  def toLazyList: LazyList[E] = elementIdsAndOperations.toLazyList.map(x => elementIdToValue.get(x.elementId).get.value)

  private def markersImpactedByChangeAt(index: Int) =
    val elementId       = elementIdsAndOperations.read(index).get.elementId
    val impactedMarkers = markerIdToElementIdAndBehavior.entries.filter { (_, marker) =>
      marker.elementId == elementId
    }.toList
    impactedMarkers

  def readAt(i: Int): Option[E] =
    elementIdsAndOperations.read(i).map(elemIdAndOp => elementIdToValue.get(elemIdAndOp.elementId).get.value)

  def move(fromIndex: Int, toIndex: Int)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] moving $fromIndex to $toIndex\n")

    val elementIdToMove = elementIdsAndOperations.read(fromIndex).get

    val updatedMarkers =
      markersImpactedByChangeAt(fromIndex)
        .map { (id, lww) =>
          val newMarkerValue =
            (lww.counter, lww.opPrecedence, CausalTime.now(), elementIdToMove.elementId, lww.markerRemovalBehavior)
          updateMarkerEntry(markerIdToElementIdAndBehavior, id, newMarkerValue)
        }
        .foldLeft(ObserveRemoveMap.empty[MarkerId, MarkerValue])(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations =
        elementIdsAndOperations.removeIndex(fromIndex)
        `merge`
        elementIdsAndOperations.insertAt(toIndex, (elementIdToMove.elementId, OpPrecedence.Move)),
      elementIdToValue =
        elementIdToValue.transform(elementIdToMove.elementId)(_.map(identity)),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  def insertAt(index: Int, element: E)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] inserting at $index\n")

    val elementId = elementIdToValue.observed.nextDot
    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.insertAt(index, (elementId, OpPrecedence.Generic)),
      elementIdToValue = elementIdToValue.update(elementId, LastWriterWins(CausalTime.now(), element))
    )
  }

  def removeAt(index: Int)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] removing at $index\n")

    val inRange = (i: Int) => if (0 until elementIdsAndOperations.size) contains i then Some(i) else None

    val updatedMarkers =
      markersImpactedByChangeAt(index)
        .map { (id, content) =>
          val behavior = content.markerRemovalBehavior
          (
            content.counter,
            id,
            behavior match
              case MarkerRemovalBehavior.Predecessor => inRange(index - 1)
              case MarkerRemovalBehavior.Successor   => inRange(index + 1)
              case MarkerRemovalBehavior.None        => None,
            behavior
          )
        }
        .map { (counter, id, newIdxOpt, behavior) =>
          newIdxOpt match
            case Some(newIdx) =>
              val markedElementId = elementIdsAndOperations.read(newIdx).get.elementId
              val newMarkerValue  = (counter, OpPrecedence.Generic, CausalTime.now(), markedElementId, behavior)
              updateMarkerEntry(markerIdToElementIdAndBehavior, id, newMarkerValue)
            case None =>
              markerIdToElementIdAndBehavior.remove(id)
        }
        .foldLeft(ObserveRemoveMap.empty[MarkerId, MarkerValue])(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.removeIndex(index),
      elementIdToValue = elementIdToValue.remove(elementIdsAndOperations.read(index).get.elementId),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  def append(elementValue: E)(using LocalUid): ReplicatedUniqueList[E] =
    insertAt(size, elementValue)

  def updateAt(index: Int, elementValue: E)(using LocalUid): ReplicatedUniqueList[E] = {
    println(s"[${LocalUid.replicaId}] updating at $index to $elementValue\n")

    val elementMetadata = elementIdsAndOperations.read(index).get

    val updatedMarkers =
      markersImpactedByChangeAt(index)
        .map { (markerId, markerInfo) =>
          val newMarkerValue = (
            markerInfo.counter,
            OpPrecedence.Update,
            CausalTime.now(),
            elementMetadata.elementId,
            markerInfo.markerRemovalBehavior
          )
          updateMarkerEntry(markerIdToElementIdAndBehavior, markerId, newMarkerValue)
        }
        .foldLeft(ObserveRemoveMap.empty[MarkerId, MarkerValue])(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.update(index, (elementMetadata.elementId, OpPrecedence.Update)),
      elementIdToValue = elementIdToValue.transform(elementMetadata.elementId)(_.map(_.write(elementValue))),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  private def updateMarkerEntry(
      base: ObserveRemoveMap[MarkerId, MarkerValue],
      markerId: MarkerId,
      newMarkerValue: MarkerValue,
  )(using
      LocalUid
  )
      : ObserveRemoveMap[MarkerId, MarkerValue] = {
    val updatedMapEntry = base.update(markerId, newMarkerValue).inner(markerId)

    val mergedMapEntry =
      base.inner.get(markerId) match
        case Some(oldInternal) => oldInternal `merge` updatedMapEntry
        case None              => updatedMapEntry

    ObserveRemoveMap.empty[MarkerId, MarkerValue].copy(inner = Map(markerId -> mergedMapEntry))
  }

  def addMarker(
      markerId: MarkerId,
      index: Int,
      removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None,
      opPrecedence: OpPrecedence = OpPrecedence.Generic
  )(using
      LocalUid
  )
      : ReplicatedUniqueList[E] = {
    val markedElementId = elementIdsAndOperations.read(index).get.elementId
    val newCounter      = markerIdToElementIdAndBehavior.inner.get(markerId).map(_.value.counter) match {
      case Some(counter) => counter + 1
      case None          => 0
    }
    val newMarkerValue = (newCounter, opPrecedence, CausalTime.now(), markedElementId, removalBehavior)
    copy(markerIdToElementIdAndBehavior = updateMarkerEntry(markerIdToElementIdAndBehavior, markerId, newMarkerValue))
  }

  def updateMarker(
      markerId: MarkerId,
      index: Int,
      removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None,
      opPrecedence: OpPrecedence = OpPrecedence.Generic
  )(using
      LocalUid
  )
      : ReplicatedUniqueList[E] = {
    val markedElementId = elementIdsAndOperations.read(index).get.elementId
    val oldMarkerValue  = markerIdToElementIdAndBehavior.inner(markerId).value
    val newMarkerValue  = (oldMarkerValue.counter, opPrecedence, CausalTime.now(), markedElementId, removalBehavior)
    copy(markerIdToElementIdAndBehavior = updateMarkerEntry(markerIdToElementIdAndBehavior, markerId, newMarkerValue))
  }

  def removeMarker(markerId: MarkerId): ReplicatedUniqueList[E] =
    ReplicatedUniqueList[E](markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.remove(markerId))

  def getMarker(markerId: MarkerId): Option[Int] = {
    markerIdToElementIdAndBehavior.get(markerId).flatMap { marker =>
      val elementId    = marker.elementId
      val elementIndex = elementIdsAndOperations.toLazyList.indexWhere(_.elementId == elementId)
      if elementIndex == -1 then None else Some(elementIndex)
    }
  }

  def filter(other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] = {
    val combined = Array(elementIdsAndOperations, other.elementIdsAndOperations).view.sortBy(_.hashCode)

    val combinedElementMetadata      = combined.flatMap { _.elements.toList }
    val combinedDotToElementMetadata = combinedElementMetadata.toMap
    val combinedTimes                = combined.flatMap { _.times.toList }.toMap
    val combinedRemoved = combined.foldLeft(Dots.empty) { (accumulator, other) => accumulator `merge` other.removed }

    val updatedDots = Dots.from(
      combinedRemoved.toSet.filter { removedDot =>
        combinedDotToElementMetadata.get(removedDot) match
          case None                    => false
          case Some(candidateMetadata) =>
            combinedElementMetadata.exists { (candidateDot, elementMetadata) =>
              elementMetadata.elementId == candidateMetadata.elementId
              && elementMetadata.opPrecedence.detail.positional == Positional.Update
              && candidateDot != removedDot
              && {
                val otherDotTimestamp = combinedTimes(candidateDot)
                Array(otherDotTimestamp, combinedTimes(removedDot)).max == otherDotTimestamp
              }
            }
          // returns whether there is another dot for this element, caused by an update with a more recent timestamp
      }
    )

    val localTrulyRemovedExclUpdated    = elementIdsAndOperations.removed `diff` updatedDots
    val combinedTrulyRemovedExclUpdated = combinedRemoved `diff` updatedDots

    def latestMostPrivilegedDotPerUniqueValue[P: Ordering](
        precedenceSelector: OpPrecedence => P
    ): Dots =
      combinedElementMetadata
        .groupBy { _._2.elementId }
        .flatMap {
          (_, entries) =>
            entries
              .filterNot { (dot, _) => combinedTrulyRemovedExclUpdated.contains(dot) }
              .maxByOption { (dot, elementMetadata) =>
                (
                  precedenceSelector(elementMetadata.opPrecedence),
                  combinedTimes(dot),
                  dot.toString
                )
              }
        }
        .keys
        .pipe(Dots.from)

    val localDots                       = Dots.from(elementIdsAndOperations.elements.keys)
    val dotsForLocalDuplicateElementIds = localDots.diff(latestMostPrivilegedDotPerUniqueValue { _.detail.positional })
    val dotsForLocalDuplicateElementValues = localDots.diff(latestMostPrivilegedDotPerUniqueValue { _.detail.textual })

    val deletedElementIds =
      elementIdToValue.inner.filterNot { (_, mapEntry) =>
        dotsForLocalDuplicateElementValues.subsumes(mapEntry.dots)
      }

    copy(
      elementIdsAndOperations = elementIdsAndOperations.copy(
        removed =
          localTrulyRemovedExclUpdated
          `merge` dotsForLocalDuplicateElementIds
      ),
      elementIdToValue = elementIdToValue.copy(
        removed =
          deletedElementIds.keys.foldLeft(ObserveRemoveMap.empty[ElementId, LastWriterWins[E]]) {
            (accumulator, key) => accumulator `merge` accumulator.remove(key)
          }.removed
      ),
      markerIdToElementIdAndBehavior = markerIdToElementIdAndBehavior.copy(
        removed =
          markerIdToElementIdAndBehavior.removed
          `union`
          deletedElementIds.values.foldLeft(ObserveRemoveMap.empty[MarkerId, MarkerValue]) {
            (accumulator, elementId) => accumulator `merge` accumulator.removeByValue { _.elementId == elementId }
          }.removed
      )
    )
  }
}

object ReplicatedUniqueList {
  type ElementId   = Dot
  type MarkerId    = Uid
  type MarkerValue = (
      counter: Int,
      opPrecedence: OpPrecedence,
      opTimestamp: CausalTime,
      elementId: ElementId,
      markerRemovalBehavior: MarkerRemovalBehavior
  )

  enum OpPrecedence {
    case Update, Move, Generic

    def detail: (positional: Positional, textual: Textual) = this match {
      case Update  => (Positional.Update, Textual.Update)
      case Move    => (Positional.Move, Textual.Generic)
      case Generic => (Positional.Generic, Textual.Insert)
    }
  }

  object OpPrecedence {
    enum Positional:
      case Generic, Update, Move

    enum Textual:
      case Generic, Insert, Update

    given enumOrdinalOrdering[E <: scala.reflect.Enum]: Ordering[E] =
      Ordering.by[E, Int](_.ordinal)
  }

  enum MarkerRemovalBehavior:
    case Predecessor, Successor, None

  given bottom[E]: Bottom[ReplicatedUniqueList[E]] = Bottom.derived
  def empty[E]: ReplicatedUniqueList[E]            = bottom.empty

  private given Lattice[MarkerValue] =
    Lattice.fromOrdering(using
      Ordering.by { tuple =>
        (tuple.counter, tuple.opPrecedence.detail.positional, tuple.opPrecedence.detail.textual, tuple.opTimestamp)
      }
    )

  private given undecoratedLattice[E]: Lattice[ReplicatedUniqueList[E]] = Lattice.derived

  given lattice[E]: DecoratedLattice[ReplicatedUniqueList[E]](undecoratedLattice) with {
    override def filter(base: ReplicatedUniqueList[E], other: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
      base.filter(other)

    override def compact(merged: ReplicatedUniqueList[E]): ReplicatedUniqueList[E] =
      merged.copy(elementIdsAndOperations = merged.elementIdsAndOperations.compact)
  }
}
