package webapps.ex2025tabular.lib

import rdts.base.*
import rdts.base.Lattice.syntax
import rdts.datatypes.{DotRemoveMap, LastWriterWins, MultiVersionRegister, ReplicatedList}
import rdts.time.{CausalTime, Dot, Dots}
import webapps.ex2025tabular.lib.ReplicatedUniqueList.OpPrecedence.{Positional, Textual}
import webapps.ex2025tabular.lib.ReplicatedUniqueList.{ElementId, MarkerId, MarkerRemovalBehavior, MarkerValue, Markers, OpPrecedence, given}

import scala.util.chaining.scalaUtilChainingOps

case class ReplicatedUniqueList[E](
    elementIdsAndOperations: ReplicatedList[(elementId: ElementId, opPrecedence: OpPrecedence)] =
      ReplicatedList.empty[(elementId: ElementId, opPrecedence: OpPrecedence)],
    elementIdToValue: DotRemoveMap[LastWriterWins[E]] = DotRemoveMap.empty[LastWriterWins[E]],
    markerIdToElementIdAndBehavior: Markers = Map.empty
) {
  lazy val now: Option[CausalTime] = elementIdsAndOperations.now

  lazy val observed: Dots =
    elementIdsAndOperations.observed
    `union` elementIdToValue.observed
    `union` markerIdToElementIdAndBehavior.values.foldLeft(Dots.empty)(_ `union` _.observed)

  lazy val toList: List[E] = elementIdsAndOperations.toList.map(x => elementIdToValue.get(x.elementId)).map(_.get.value)

  def size: Int = elementIdsAndOperations.size

  def toLazyList: LazyList[E] = elementIdsAndOperations.toLazyList.map(x => elementIdToValue.get(x.elementId).get.value)

  private def markersImpactedByChangeAt(index: Int) =
      val elementId       = elementIdsAndOperations.read(index).get.elementId
      val impactedMarkers = markerEntries.filter { (_, marker) =>
        marker.elementId == elementId
      }.toList
      impactedMarkers

  def readAt(i: Int): Option[E] =
    elementIdsAndOperations.read(i).map(elemIdAndOp => elementIdToValue.get(elemIdAndOp.elementId).get.value)

  def move(fromIndex: Int, toIndex: Int)(using ReplicaId): ReplicatedUniqueList[E] = {
    // println(s"[${LocalUid.replicaId}] moving $fromIndex to $toIndex\n")

    val elementIdToMove = elementIdsAndOperations.read(fromIndex).get

    val updatedMarkers =
      markersImpactedByChangeAt(fromIndex)
        .map { (id, lww) =>
          val newMarkerValue =
            (lww.counter, lww.opPrecedence, CausalTime.now(), elementIdToMove.elementId, lww.markerRemovalBehavior)
          writeMarker(id, newMarkerValue)
        }
        .foldLeft(Map.empty: Markers)(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations =
        elementIdsAndOperations.removeIndex(fromIndex)
        `merge`
        elementIdsAndOperations.insertAt(toIndex, (elementIdToMove.elementId, OpPrecedence.Move)),
      elementIdToValue =
        elementIdToValue.transform(elementIdToMove.elementId)(identity),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  def insertAt(index: Int, element: E)(using ReplicaId): ReplicatedUniqueList[E] = {
    // println(s"[${LocalUid.replicaId}] inserting at $index\n")

    val elementId = elementIdToValue.nextId
    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.insertAt(index, (elementId, OpPrecedence.Generic)),
      elementIdToValue = elementIdToValue.insert(LastWriterWins(CausalTime.now(), element))
    )
  }

  def removeAt(index: Int)(using ReplicaId): ReplicatedUniqueList[E] = {
    // println(s"[${LocalUid.replicaId}] removing at $index\n")

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
                writeMarker(id, newMarkerValue)
              case None =>
                clearMarker(id)
        }
        .foldLeft(Map.empty: Markers)(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.removeIndex(index),
      elementIdToValue = elementIdToValue.remove(elementIdsAndOperations.read(index).get.elementId),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  def append(elementValue: E)(using ReplicaId): ReplicatedUniqueList[E] =
    insertAt(size, elementValue)

  def updateAt(index: Int, elementValue: E)(using ReplicaId): ReplicatedUniqueList[E] = {
    // println(s"[${LocalUid.replicaId}] updating at $index to $elementValue\n")

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
          writeMarker(markerId, newMarkerValue)
        }
        .foldLeft(Map.empty: Markers)(_ `merge` _)

    ReplicatedUniqueList[E](
      elementIdsAndOperations = elementIdsAndOperations.update(index, (elementMetadata.elementId, OpPrecedence.Update)),
      elementIdToValue = elementIdToValue.transform(elementMetadata.elementId)(_.write(elementValue)),
      markerIdToElementIdAndBehavior = updatedMarkers
    )
  }

  /** current value of a marker, concurrent values are resolved by the marker lattice */
  private def readMarker(markerId: MarkerId): Option[MarkerValue] =
    markerIdToElementIdAndBehavior.get(markerId).flatMap(_.read.reduceOption(_ `merge` _))

  private def markerEntries: List[(MarkerId, MarkerValue)] =
    markerIdToElementIdAndBehavior.keys.toList.flatMap(id => readMarker(id).map(id -> _))

  /** replaces all observed values of the marker */
  private def writeMarker(markerId: MarkerId, value: MarkerValue)(using ReplicaId): Markers =
    Map(markerId -> markerIdToElementIdAndBehavior.getOrElse(markerId, MultiVersionRegister.empty).write(value))

  /** removes all observed values of the marker, concurrent writes survive */
  private def clearMarker(markerId: MarkerId): Markers =
    markerIdToElementIdAndBehavior.get(markerId).fold(Map.empty)(register => Map(markerId -> register.clear()))

  def addMarker(
      markerId: MarkerId,
      index: Int,
      removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None,
      opPrecedence: OpPrecedence = OpPrecedence.Generic
  )(using
      ReplicaId
  )
      : ReplicatedUniqueList[E] = {
    val markedElementId = elementIdsAndOperations.read(index).get.elementId
    val newCounter      = readMarker(markerId).map(_.counter) match {
      case Some(counter) => counter + 1
      case None          => 0
    }
    val newMarkerValue = (newCounter, opPrecedence, CausalTime.now(), markedElementId, removalBehavior)
    copy(markerIdToElementIdAndBehavior = writeMarker(markerId, newMarkerValue))
  }

  def updateMarker(
      markerId: MarkerId,
      index: Int,
      removalBehavior: MarkerRemovalBehavior = MarkerRemovalBehavior.None,
      opPrecedence: OpPrecedence = OpPrecedence.Generic
  )(using
      ReplicaId
  )
      : ReplicatedUniqueList[E] = {
    val markedElementId = elementIdsAndOperations.read(index).get.elementId
    val oldMarkerValue  = readMarker(markerId).get
    val newMarkerValue  = (oldMarkerValue.counter, opPrecedence, CausalTime.now(), markedElementId, removalBehavior)
    copy(markerIdToElementIdAndBehavior = writeMarker(markerId, newMarkerValue))
  }

  def removeMarker(markerId: MarkerId): ReplicatedUniqueList[E] =
    ReplicatedUniqueList[E](markerIdToElementIdAndBehavior = clearMarker(markerId))

  def getMarker(markerId: MarkerId): Option[Int] =
    readMarker(markerId).flatMap { marker =>
      val elementId    = marker.elementId
      val elementIndex = elementIdsAndOperations.toLazyList.indexWhere(_.elementId == elementId)
      if elementIndex == -1 then None else Some(elementIndex)
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

    val localDots = Dots.from(elementIdsAndOperations.elements.keys)
    val dotsForLocalDuplicateElementIds = localDots.diff(latestMostPrivilegedDotPerUniqueValue { _.detail.positional })

    copy(
      elementIdsAndOperations = elementIdsAndOperations.copy(
        removed =
          localTrulyRemovedExclUpdated
          `merge` dotsForLocalDuplicateElementIds
      ),
      // Element value and marker removes used to be computed here by removing keys from *empty* ObserveRemoveMaps,
      // which always resulted in no removed dots. The element value removes are kept as is, see git history.
      elementIdToValue = elementIdToValue.copy(removed = Dots.empty),
    )
  }
}

object ReplicatedUniqueList {
  type ElementId   = Dot
  type MarkerId    = Uid
  /** markers can be re-added under the same id, concurrent writes are kept by the register and resolved on read */
  type Markers     = Map[MarkerId, MultiVersionRegister[MarkerValue]]
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

  given markerValueLattice: Lattice[MarkerValue] =
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
