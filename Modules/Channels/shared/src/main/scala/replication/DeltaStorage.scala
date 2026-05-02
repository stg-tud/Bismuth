package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Historized, Lattice}
import rdts.time.Dots
import replication.PlumtreeMessage.Payload

import scala.collection.immutable.Queue

trait DeltaStorage[State] {

  def getHistory: List[Payload[State]]

  def remember(message: Payload[State]): DeltaStorage[State]

}

object DeltaStorage {

  enum Type {
    case Discarding(maxSize: Int)
    case State
    case KeepAll
    case Merging(blockSize: Int)
    case NoHistory
  }

  def getStorage[State: JsonValueCodec](t: Type, getState: () => State)(using
      Lattice[Payload[State]]
  ): DeltaStorage[State] = t match {
    case Type.Discarding(maxSize) => DiscardingHistory(maxSize)
    case Type.State               => StateDeltaStorage[State](getState)
    case Type.KeepAll             => KeepAllHistory[State]()
    case Type.Merging(blockSize)  => MergingHistory[State](blockSize)
    case Type.NoHistory           => NoHistory[State]()
  }

}

final case class NoHistory[State]() extends DeltaStorage[State] {
  override def getHistory: List[Payload[State]] = List.empty

  override def remember(message: Payload[State]): DeltaStorage[State] = this
}

final case class DiscardingHistory[State](size: Int, pastPayloads: Queue[Payload[State]] = Queue.empty) extends DeltaStorage[State] {

  override def getHistory: List[Payload[State]] = pastPayloads.toList

  override def remember(message: Payload[State]): DeltaStorage[State] = {
    val next = pastPayloads.enqueue(message)
    copy(pastPayloads = if next.sizeIs > size then next.drop(1) else next)
  }

}

final case class StateDeltaStorage[State: JsonValueCodec](getState: () => State, dots: Dots = Dots.empty)(using Lattice[Dots]) extends DeltaStorage[State] {

  override def getHistory: List[Payload[State]] =
    List(Payload(dots, getState()))

  override def remember(message: Payload[State]): DeltaStorage[State] =
    copy(dots = dots.merge(message.dots))

}

final case class KeepAllHistory[State](history: List[Payload[State]] = List.empty) extends DeltaStorage[State] {

  override def getHistory: List[Payload[State]] = history

  override def remember(message: Payload[State]): DeltaStorage[State] =
    copy(history = message :: history)

}

final case class MergingHistory[State: JsonValueCodec](
    blockSize: Int,
    mergedHistory: List[Payload[State]] = List.empty,
    history: List[Payload[State]] = List.empty,
)(using Lattice[Payload[State]]) extends DeltaStorage[State] {

  override def getHistory: List[Payload[State]] = mergedHistory ::: history

  override def remember(message: Payload[State]): DeltaStorage[State] = {
    val nextHistory = message :: history
    if nextHistory.sizeIs >= blockSize then
      copy(
        mergedHistory = nextHistory.reduce(Lattice.merge) :: mergedHistory,
        history = List.empty,
      )
    else copy(history = nextHistory)
  }

}

final case class NonRedundantHistory[State: {JsonValueCodec, Historized}](history: Set[Payload[State]] = Set.empty) extends DeltaStorage[State] {

  override def getHistory: List[Payload[State]] = history.toList

  override def remember(message: Payload[State]): DeltaStorage[State] = {
    val redundantDeltas: Dots = history.toMetaDeltas.getRedundantDeltas(message.data)
    val redundantDots: Dots   = history.foldLeft(Dots.empty)((dots, bufferedDelta) =>
      if !dots.contains(bufferedDelta.dots) then dots.union(bufferedDelta.redundantDots) else dots
    )

    copy(history = history.filterNot(p => redundantDots.contains(p.dots)) + message.copy(redundantDots = redundantDeltas))
  }

}
