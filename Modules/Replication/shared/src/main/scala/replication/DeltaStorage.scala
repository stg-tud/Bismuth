package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.time.Dots
import replication.DeltaDissemination.pmscodec
import replication.DeltaStorage.Type.Discarding
import replication.ProtocolMessage.Payload

import scala.collection.immutable.Queue

trait DeltaStorage[State] {

  def allPayloads: List[CachedMessage[Payload[State]]]

  def rememberPayload(message: CachedMessage[Payload[State]]): Unit

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

  private val lock = new {}

  private var pastPayloads: Queue[CachedMessage[Payload[State]]] = Queue.empty

  override def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(pastPayloads.toList)

  override def rememberPayload(message: CachedMessage[Payload[State]]): Unit = lock.synchronized {
    pastPayloads = pastPayloads.enqueue(message)
    if pastPayloads.sizeIs > size then
      pastPayloads = pastPayloads.drop(1)
      ()
  }

}

class StateDeltaStorage[State: JsonValueCodec](getState: () => State)(using Lattice[Dots]) extends DeltaStorage[State] {

  private var dots    = Dots.empty
  private var senders = Set.empty[Uid]

  override def allPayloads: List[CachedMessage[Payload[State]]] =
    List(SentCachedMessage(Payload(senders, dots, getState()))(using pmscodec))

  override def rememberPayload(message: CachedMessage[Payload[State]]): Unit = {
    dots = dots.merge(message.payload.dots)
    senders = senders ++ message.payload.senders
  }

}

class KeepAllHistory[State] extends DeltaStorage[State] {

  private val lock = new {}

  private var history: List[CachedMessage[Payload[State]]] = List.empty

  override def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(history)

  override def rememberPayload(message: CachedMessage[Payload[State]]): Unit = lock.synchronized {
    history = message :: history
  }

}

class MergingHistory[State: JsonValueCodec](blockSize: Int)(using Lattice[Payload[State]]) extends DeltaStorage[State] {

  private val lock = new {}

  private var mergedHistory: List[CachedMessage[Payload[State]]] = List.empty
  private var history: List[CachedMessage[Payload[State]]]       = List.empty

  override def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(mergedHistory ::: history)

  override def rememberPayload(message: CachedMessage[Payload[State]]): Unit = lock.synchronized {
    history = message :: history

    if history.sizeIs >= blockSize then {
      val merged = SentCachedMessage(history.map(_.payload).reduce(Lattice.merge))(using pmscodec)

      mergedHistory = merged :: mergedHistory
      history = List.empty
    }
  }

}
