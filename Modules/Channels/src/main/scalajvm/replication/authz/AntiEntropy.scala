package replication.authz

import channels.connection.{ByteBufferMessageBuffer, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.Commitment.RevealedValue
import crypto.{Hash, PublicIdentity}
import replication.authz.AntiEntropy.*
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.sync.{ConnectionManager, MessageReceiver}

import java.nio.ByteBuffer
import scala.collection.mutable

class AntiEntropy(
    replica: Replica[?],
    connectionManagerProvider: MessageReceiver[MessageBuffer] => ConnectionManager,
    controlPlaneProvider: ConnectionManager => MessageReceiver[ByteBuffer]
) extends MessageReceiver[MessageBuffer] {

  private val missingEvents: mutable.Set[Hash]                                           = mutable.Set.empty
  private val eventsWithMissingDependencies: mutable.Map[Hash, (Array[Byte], Set[Hash])] = mutable.Map.empty
  private val knowledgeableReplicas: mutable.Queue[PublicIdentity]                       = mutable.Queue.empty
  private val deltasWithMissingEvent: mutable.Map[Hash, RevealedValue]                   = mutable.Map.empty

  private lazy val connectionManager: ConnectionManager      = connectionManagerProvider(this)
  private lazy val controlPlane: MessageReceiver[ByteBuffer] = controlPlaneProvider(connectionManager)

  def listenAddress: Option[(String, Int)]  = connectionManager.listenAddress
  def connect(address: (String, Int)): Unit = connectionManager.connectTo(address)

  @volatile private var running = true

  def start(): Unit = {
    running = true
    connectionManager.acceptIncomingConnections()
    Thread.ofVirtual().start(() =>
      while running do {
        try Thread.sleep(1_000)
        catch { case e: InterruptedException => }
        requestMissing()
      }
    ): Unit
  }

  def stop(): Unit = {
    running = false
    connectionManager.shutdown()
  }

  def broadcastEvents(events: Iterable[Array[Byte]]): Unit =
    connectionManager.broadcast(
      events.map(encodedEvent => encodeEventMsg(encodedEvent))
    )

  def sendEvents(destination: PublicIdentity, events: Iterable[Array[Byte]]): Unit =
    connectionManager.sendMultiple(
      destination,
      events.map(encodedEvent => encodeEventMsg(encodedEvent))
    )

  def sendEventsWithDelta(destination: PublicIdentity, eventHashes: Iterable[Hash]): Unit =
      val events: Iterable[(Hash, ArdtEvent)] = eventHashes.flatMap(hash => replica.event(hash).map(hash -> _))
      sendEvents(destination, replica.heads.flatMap(replica.event).map(writeToArray(_)))

      val deltas = events.flatMap {
        case (eventHash, ArdtEvent(DeltaCommitment(commitmentHash), _, _, _, _)) =>
          replica.delta(commitmentHash).map(eventHash -> _)
        case _ => None
      }
      sendDeltasFiltered(destination, deltas)

  def broadcastDeltasFiltered(deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]): Unit =
    connectionManager.connectedPeers.foreach { peer =>
      sendDeltasFiltered(peer, deltas)
    }

  def sendDeltasFiltered(
      destination: PublicIdentity,
      deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]
  ): Unit = {
    val filtered = replica.filterDeltas(destination, deltas)
    val msgs     = filtered.map {
      case (eventHash, deltaValue) => encodeDeltaMsg(eventHash, deltaValue)
    }
    connectionManager.sendMultiple(destination, msgs)
  }

  def receivedMessage(msg: MessageBuffer, sender: PublicIdentity): Unit = synchronized {
    val msgBytes = msg.asByteBuffer
    msgBytes.get(0) match {
      case EVENT_MSG_TAG =>
        val encodedEvent = decodeEventMsg(msgBytes)
        replica.receiveEvent(encodedEvent) match {
          case Right(Some(eventHash)) =>
            missingEvents.remove(eventHash): Unit
          // TODO: Remove from missing dependencies and receive events that are now receivable
          case Right(None)         => // Duplicate event
          case Left(missingEvents) =>
            val eventHash = Hash.compute(encodedEvent)
            enqueueEventWithMissingPredecessors(eventHash, encodedEvent, missingEvents, sender)
        }
      case DELTA_VALUE_MSG_TAG =>
        val (event, deltaValue) = decodeDeltaMsg(msgBytes)
        if replica.containsEvent(event) then replica.receiveDelta(event, deltaValue)
        else deltasWithMissingEvent.put(event, deltaValue): Unit
      case REQUEST_EVENTS_MSG_TAG =>
        val requestedEventHashes = decodeRequestEventsMsg(msgBytes)
        sendEventsWithDelta(sender, requestedEventHashes)
      case CONTROL_PLANE_MSG_TAG     => controlPlane.receivedMessage(msgBytes, sender)
      case REQUEST_BOOTSTRAP_MSG_TAG =>
        val events        = replica.allEventsInCausalOrder
        val encodedEvents = events.map((_, ev) => writeToArray(ev))
        sendEvents(sender, encodedEvents)
        sendDeltasFiltered(
          sender,
          events.flatMap {
            case (eventHash, ArdtEvent(DeltaCommitment(deltaCommitment), _, _, _, _)) =>
              replica.delta(deltaCommitment).map(eventHash -> _)
            case _ => None
          }
        )
      case _ => ???
    }
  }

  def requestMissing(): Unit = synchronized {
    if missingEvents.isEmpty || knowledgeableReplicas.isEmpty then return
    val replicaToAsk = knowledgeableReplicas.dequeue()
    connectionManager.send(replicaToAsk, encodeRequestEventsMsg(missingEvents))
  }

  override def connectionEstablished(publicIdentity: PublicIdentity): Unit = {
    println(s"Connection established: $publicIdentity")
    if replica.heads.isEmpty then
        connectionManager.send(publicIdentity, ByteBufferMessageBuffer(Array(REQUEST_BOOTSTRAP_MSG_TAG)))
    else
        sendEventsWithDelta(publicIdentity, replica.heads)

    controlPlane.connectionEstablished(publicIdentity)
  }

  override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
    controlPlane.connectionShutdown(publicIdentity)

  private def enqueueEventWithMissingPredecessors(
      eventHash: Hash,
      encodedEvent: Array[Byte],
      missingEvents: Set[Hash],
      learnedFrom: PublicIdentity
  ): Unit = synchronized {
    eventsWithMissingDependencies.updateWith(Hash.compute(encodedEvent)) {
      case old @ Some(_) => old
      case None          => Some((encodedEvent, missingEvents))
    }
    if !knowledgeableReplicas.contains(learnedFrom) then knowledgeableReplicas.enqueue(learnedFrom)
  }
}

object AntiEntropy {
  // TODO: maybe add batching of messages for better handling of missing dependencies
  // event message format: tag(1 byte) | event(variable length)
  val EVENT_MSG_TAG: Byte = 0.toByte

  // delta value message format: tag(1 byte) | eventHash(32 bytes) | witness(32 bytes) | delta(variable length)
  val DELTA_VALUE_MSG_TAG: Byte = 1.toByte

  // message format: tag(1 byte) | numberOfHashes(4 bytes) | eventHash(32 bytes) | ...
  val REQUEST_EVENTS_MSG_TAG: Byte = 2.toByte

  // message format: tag(1 byte)
  val REQUEST_BOOTSTRAP_MSG_TAG: Byte = 3.toByte

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
    val event = new Array[Byte](buffer.remaining)
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
    val deltaValue = new Array[Byte](buffer.remaining)
    buffer.get(deltaValue)
    val revealedValue = RevealedValue(deltaValue, witness)
    (Hash.unsafeFromArray(eventHash), revealedValue)
  }

  def encodeRequestEventsMsg(heads: Iterable[Hash]): ByteBufferMessageBuffer = {
    val msg = ByteBuffer.allocate(1 + Integer.BYTES + heads.size * Hash.length)
      .put(REQUEST_EVENTS_MSG_TAG)
      .putInt(heads.size)
    heads.foreach(hash => msg.put(hash.toArray))
    msg.rewind()
    ByteBufferMessageBuffer(msg)
  }

  def decodeRequestEventsMsg(buffer: ByteBuffer): Set[Hash] = {
    require(buffer.get() == REQUEST_EVENTS_MSG_TAG)

    val numHeads = buffer.getInt()

    (0 until numHeads).map(_ =>
        val hash = Array.ofDim[Byte](Hash.length)
        buffer.get(hash)
        Hash.unsafeFromArray(hash)
    ).toSet
  }
}
