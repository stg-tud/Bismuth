package replication.authz

import channels.connection.{ByteBufferMessageBuffer, MessageBuffer}
import crypto.Commitment.RevealedValue
import crypto.{Hash, PublicIdentity}
import replication.authz.AntiEntropy.*
import replication.sync.{ConnectionManager, MessageReceiver}

import java.nio.ByteBuffer
import scala.collection.mutable

class AntiEntropy(
    replica: Replica[?],
    connectionManagerProvider: MessageReceiver[MessageBuffer] => ConnectionManager,
    controlPlaneProvider: ConnectionManager => MessageReceiver[ByteBuffer]
) extends MessageReceiver[MessageBuffer] {

  private val missingEvents: mutable.Set[Hash] = mutable.Set.empty
  private val eventsWithMissingDependencies: mutable.Map[Hash, (Array[Byte], Set[Hash], PublicIdentity)] =
    mutable.Map.empty
  private val deltasWithMissingEvent: mutable.Map[Hash, RevealedValue] = mutable.Map.empty
  private lazy val connectionManager                                   = connectionManagerProvider(this)
  private lazy val controlPlane                                        = controlPlaneProvider(connectionManager)

  def listenAddress: Option[(String, Int)] = connectionManager.listenAddress

  def broadcastEvents(events: Iterable[Array[Byte]]): Unit =
    connectionManager.broadcast(
      events.map(encodedEvent => encodeEventMsg(encodedEvent))
    )

  def sendEvents(destination: PublicIdentity, events: Iterable[Array[Byte]]): Unit =
    connectionManager.sendMultiple(
      destination,
      events.map(encodedEvent => encodeEventMsg(encodedEvent))
    )

  def broadcastDeltasFiltered(deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]): Unit =
    connectionManager.connectedPeers.foreach { peer =>
      sendDeltasFiltered(deltas, peer)
    }

  def sendDeltasFiltered(
      deltas: Iterable[(eventHash: Hash, delta: RevealedValue)],
      destination: PublicIdentity
  ): Unit = {
    val filtered = replica.filterDeltas(destination, deltas)
    val msgs     = filtered.map {
      case (eventHash, deltaValue) => encodeDeltaMsg(eventHash, deltaValue)
    }
    connectionManager.sendMultiple(destination, msgs)
  }

  def receivedMessage(msg: MessageBuffer, sender: PublicIdentity): Unit = {
    val msgBytes = msg.asByteBuffer
    msgBytes.get(0) match {
      case EVENT_MSG_TAG =>
        val encodedEvent = decodeEventMsg(msgBytes)
        replica.receiveEvent(encodedEvent) match {
          case Right(eventHash) =>
            missingEvents.remove(eventHash): Unit
          // TODO: Remove from missing dependencies and receive events that are now receivable
          case Left(missingEvents) =>
            val eventHash = Hash.compute(encodedEvent)
            enqueueEventWithMissingPredecessors(eventHash, encodedEvent, missingEvents, sender)
        }
      case DELTA_VALUE_MSG_TAG =>
        val (event, deltaValue) = decodeDeltaMsg(msgBytes)
        if replica.containsEvent(event) then replica.receiveDelta(event, deltaValue)
        else deltasWithMissingEvent.put(event, deltaValue): Unit
      case CONTROL_PLANE_MSG_TAG => controlPlane.receivedMessage(msgBytes, sender)
      case _                     => ???
    }
  }

  override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
    controlPlane.connectionEstablished(publicIdentity)

  override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
    controlPlane.connectionShutdown(publicIdentity)

  private def enqueueEventWithMissingPredecessors(
      eventHash: Hash,
      encodedEvent: Array[Byte],
      missingEvents: Set[Hash],
      learnedFrom: PublicIdentity
  ): Unit =
    eventsWithMissingDependencies.updateWith(Hash.compute(encodedEvent)) {
      case old @ Some(_) => old
      case None          => Some((encodedEvent, missingEvents, learnedFrom))
    }: Unit
}

object AntiEntropy {
  // TODO: maybe add batching of messages for better handling of missing dependencies
  // event message format: tag(1 byte) | event(variable length)
  val EVENT_MSG_TAG: Byte = 0.toByte

  // delta value message format: tag(1 byte) | eventHash(32 bytes) | witness(32 bytes) | delta(variable length)
  val DELTA_VALUE_MSG_TAG: Byte = 1.toByte

  // control messages that are forwarded to handler: tag(1 byte) | ???
  val CONTROL_PLANE_MSG_TAG: Byte = Byte.MaxValue

  def encodeEventMsg(event: Array[Byte]): ByteBufferMessageBuffer = ByteBufferMessageBuffer(
    ByteBuffer.allocate(event.length + 1)
      .put(EVENT_MSG_TAG)
      .put(event)
      .rewind()
  )

  def decodeEventMsg(buffer: ByteBuffer): Array[Byte] = {
    val tag = buffer.get()
    require(tag == EVENT_MSG_TAG)
    val event = new Array[Byte](buffer.remaining - 1)
    buffer.get(event)
    event
  }

  def encodeDeltaMsg(eventHash: Hash, deltaValue: RevealedValue): ByteBufferMessageBuffer = {
    require(deltaValue.witness.length == Hash.length)
    ByteBufferMessageBuffer(
      ByteBuffer.allocate(Hash.length + Hash.length + deltaValue.value.length + 1)
        .put(DELTA_VALUE_MSG_TAG)
        .put(eventHash.toArray)
        .put(deltaValue.witness)
        .put(deltaValue.value)
        .rewind()
    )
  }

  def decodeDeltaMsg(buffer: ByteBuffer): (Hash, RevealedValue) = {
    require(buffer.get() == DELTA_VALUE_MSG_TAG)
    val eventHash = new Array[Byte](Hash.length)
    buffer.get(eventHash)
    val witness = new Array[Byte](Hash.length)
    buffer.get(witness)
    val deltaValue = new Array[Byte](buffer.remaining - Hash.length - 1)
    buffer.get(deltaValue)
    val revealedValue = RevealedValue(deltaValue, witness)
    (Hash.unsafeFromArray(eventHash), revealedValue)
  }
}
