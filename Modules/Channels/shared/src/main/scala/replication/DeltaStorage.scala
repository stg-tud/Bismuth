package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Lattice, Uid}
import rdts.time.Dots
import replication.DeltaDissemination.pmscodec
import replication.DeltaStorage.Type.Discarding
import replication.ProtocolMessage.Payload

import scala.collection.immutable.Queue

trait DeltaStorage[State] {

  def getHistory: List[CachedMessage[Payload[State]]]

  def remember(message: CachedMessage[Payload[State]]): Unit

}

object DeltaStorage {

  enum Type {
    case Discarding(maxSize: Int)
    case State
    case KeepAll
    case Merging(blockSize: Int)
  }

  def getStorage[State: JsonValueCodec](t: Type, getState: () => State)(using
      Lattice[Payload[State]]
  ): DeltaStorage[State] = t match {
    case Type.Discarding(maxSize) => DiscardingHistory(maxSize)
    case Type.State               => StateDeltaStorage[State](getState)
    case Type.KeepAll             => KeepAllHistory[State]()
    case Type.Merging(blockSize)  => MergingHistory[State](blockSize)
  }

}

class DiscardingHistory[State](val size: Int) extends DeltaStorage[State] {

  private var pastPayloads: Queue[CachedMessage[Payload[State]]] = Queue.empty

  override def getHistory: List[CachedMessage[Payload[State]]] = pastPayloads.toList

  override def remember(message: CachedMessage[Payload[State]]): Unit = {
    pastPayloads = pastPayloads.enqueue(message)
    if pastPayloads.sizeIs > size then
      pastPayloads = pastPayloads.drop(1)
      ()
  }

}

class StateDeltaStorage[State: JsonValueCodec](getState: () => State)(using Lattice[Dots]) extends DeltaStorage[State] {

  private var dots    = Dots.empty
  private var senders = Set.empty[Uid]

  override def getHistory: List[CachedMessage[Payload[State]]] =
    List(SentCachedMessage(Payload(senders, dots, getState()))(using pmscodec))

  override def remember(message: CachedMessage[Payload[State]]): Unit = {
    dots = dots.merge(message.payload.dots)
    senders = senders ++ message.payload.senders
  }

}

class KeepAllHistory[State] extends DeltaStorage[State] {

  private var history: List[CachedMessage[Payload[State]]] = List.empty

  override def getHistory: List[CachedMessage[Payload[State]]] = history

  override def remember(message: CachedMessage[Payload[State]]): Unit =
    history = message :: history

}

class MergingHistory[State: JsonValueCodec](blockSize: Int)(using Lattice[Payload[State]]) extends DeltaStorage[State] {

  private var mergedHistory: List[CachedMessage[Payload[State]]] = List.empty
  private var history: List[CachedMessage[Payload[State]]]       = List.empty

  override def getHistory: List[CachedMessage[Payload[State]]] = mergedHistory ::: history

  override def remember(message: CachedMessage[Payload[State]]): Unit = {
    history = message :: history

    if history.sizeIs >= blockSize then {
      val merged = SentCachedMessage(history.map(_.payload).reduce(Lattice.merge))(using pmscodec)

      mergedHistory = merged :: mergedHistory
      history = List.empty
    }
  }

}
