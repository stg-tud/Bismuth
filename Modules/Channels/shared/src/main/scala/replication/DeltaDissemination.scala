package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.time.{Dot, Dots}
import replication.DeltaDissemination.pmscodec
import replication.JsoniterCodecs.given
import replication.ProtocolMessage.*

import scala.annotation.{nowarn, unused}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait Aead {
  def encrypt(plain: Array[Byte], associated: Array[Byte]): Array[Byte]
  def decrypt(cypher: Array[Byte], associated: Array[Byte]): Try[Array[Byte]]
}

object DeltaDissemination {
  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  def pmscodec[State: JsonValueCodec, T <: ProtocolMessage[State]]: JsonValueCodec[T] = {
    val pmscodecInv: JsonValueCodec[ProtocolMessage[State]] = JsonCodecMaker.make
    // we use the supertype codec here, this has the consequence of adding the type discriminators even for concrete subtypes of ProtocolMessage, which is generally what we want.
    pmscodecInv.asInstanceOf[JsonValueCodec[T]]
  }
}

class DeltaDissemination[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    @unused crypto: Option[Aead] = None,
    immediateForward: Boolean = false,
    sendingActor: ExecutionContext = DeltaDissemination.executeImmediately,
    val globalAbort: Abort = Abort(),
    val deltaStorage: DeltaStorage[State] = DiscardingHistory[State](size = 108),
)(using JsonValueCodec[State]) {

  type Message = CachedMessage[ProtocolMessage[State]]

  given LocalUid = replicaId

  def cachedMessages(conn: LatentConnection[MessageBuffer])
      : LatentConnection[CachedMessage[ProtocolMessage[State]]] = {
    LatentConnection.adapt(
      (mb: MessageBuffer) => ReceivedCachedMessage[ProtocolMessage[State]](mb)(using pmscodec),
      (pm: CachedMessage[ProtocolMessage[State]]) => pm.messageBuffer
    )(conn)
  }

  type ConnectionContext = Connection[Message]

  @volatile var connections: List[ConnectionContext] = Nil

  def debugCallbackAndRemoveCon(con: ConnectionContext): Callback[Any] =
    case Success(value)     => ()
    case Failure(exception) =>
      lock.synchronized {
        connections = connections.filter(cc => cc != con)
      }
      println(s"exception during message handling, removing connection $con from list of connections")
      exception.printStackTrace()

  def requestData(): Unit = {
    val msg = SentCachedMessage(Request(replicaId.uid, treeContext.getSelfKnowledge))(using pmscodec)
    connections.foreach: con =>
      send(con, msg)
  }

  def pingAll(): Unit = {
    val msg = SentCachedMessage(Ping(System.nanoTime()))(using pmscodec)
    connections.foreach { conn =>
      send(conn, msg)
    }
  }

  val printExceptionHandler: Callback[Any] =
    case Failure(ex) =>
      println(s"exception during connection activation")
      ex.printStackTrace()
    case Success(_) => ()

  def addBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Unit =
    prepareBinaryConnection(latentConnection).run(using ())(printExceptionHandler)

  def addObjectConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit =
    prepareObjectConnection(latentConnection).run(using ())(printExceptionHandler)

  /** prepare a connection that serializes to some binary format. Primary means of network communication. Adds a serialization and caching layer */
  def prepareBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Async[Any, Unit] =
    prepareLatentConnection(cachedMessages(latentConnection))

  /** prepare a connection that passes objects around somewhere in memory. For in prozess communication or custom serialization. */
  def prepareObjectConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Async[Any, Unit] = {
    prepareLatentConnection(LatentConnection.adapt[ProtocolMessage[State], Message](
      pm => SentCachedMessage(pm)(using pmscodec),
      cm => cm.payload
    )(latentConnection))
  }

  def prepareLatentConnection(latentConnection: LatentConnection[Message]): Async[Any, Unit] = {

    val preparedConnection: Async[Abort, Connection[Message]] = latentConnection.prepare { from =>
      {
        case Success(msg)   => handleMessage(msg, from)
        case Failure(error) =>
          println(s"exception during message handling")
          error.printStackTrace()
      }
    }
    Async.provided(globalAbort) {
      val conn: Connection[Message] = preparedConnection.bind
      lock.synchronized {
        connections = conn :: connections
      }

      sendInitialSyncRequest(conn)
    }
  }

  private def sendInitialSyncRequest(conn: ConnectionContext): Unit = {
    send(
      conn,
      SentCachedMessage(Request(
        replicaId.uid,
        treeContext.getSelfKnowledge
      ))(using pmscodec)
    )
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef = new {}

  val treeContext: DeltaTreeContext[State] = DeltaTreeContext[State](replicaId.uid)

  def applyDelta(delta: State): Unit =
    val message = lock.synchronized {
      val nextDot = treeContext.getNextDot
      val payload = Payload(replicaId.uid, Dots.single(nextDot), delta, treeContext.getSelfKnowledge)
      val message = SentCachedMessage(payload)(using pmscodec)
      treeContext.storeOutgoingMessage(nextDot, message)
      message
    }
    disseminatePayload(message)

  def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(treeContext.getAllPayloads)

  def handleMessage(msg: Message, from: ConnectionContext): Unit = {
    if globalAbort.closeRequest then return
    msg.payload match
      case Ping(time) =>
        send(from, SentCachedMessage(Pong(time))(using pmscodec))
      case Pong(time) =>
      // println(s"ping took ${(System.nanoTime() - time.toLong).doubleValue / 1000_000}ms")
      case Request(uid, knows) =>
        val (relevant, context) = lock.synchronized {
          val unknownDots = treeContext.getUnknownDotsForPeer(uid, knows)
          val payloads    = treeContext.getPayloads(unknownDots)
          (payloads, unknownDots)
        }
        relevant.foreach: msg =>
          val newMsg = augmentPayloadWithLastKnownDot(msg.payload.addSender(replicaId.uid), uid)
          send(from, newMsg)
      case payload @ Payload(uid, dots, data, causalPredecessors, lastKnownDots) =>
        lock.synchronized {
          // TODO sent unknown dots back to peer?
          if uid.size == 1 && uid.head != replicaId.uid then treeContext.updateKnowledgeOfPeer(uid.head, lastKnownDots)
          else if uid.size == 1 && uid.head == replicaId.uid then
            println(s"cannot update knowledge of peer, received message from self: $uid")
          else println(s"cannot update knowledge of peer, received message from ambiguous peers: $uid")
          val nonRedundantDots = treeContext.addNonRedundant(dots, causalPredecessors)
          if nonRedundantDots.isEmpty then return
          treeContext.storeMessage(dots, msg.asInstanceOf[CachedMessage[Payload[State]]])
        }

        receiveCallback(data)
        if immediateForward then
          disseminatePayload(msg.asInstanceOf[CachedMessage[Payload[State]]], Set(from))

  }

  def send(con: ConnectionContext, payload: Message): Unit =
    if globalAbort.closeRequest then ()
    else
      sendingActor.execute { () =>
        con.send(payload).run(using ())(debugCallbackAndRemoveCon(con))
      }

  def disseminate(payload: Message, except: Set[ConnectionContext] = Set.empty): Unit = {
    val cons = lock.synchronized(connections)
    cons.filterNot(con => except.contains(con)).foreach: con =>
      send(con, payload)

  }

  def disseminatePayload(payload: CachedMessage[Payload[State]], except: Set[ConnectionContext] = Set.empty): Unit = {
    val cons = lock.synchronized(connections)
    cons.filterNot(con => except.contains(con)).foreach { con =>
      val p = con.authenticatedPeerReplicaId match {
        case Some(receiver) => augmentPayloadWithLastKnownDot(payload.payload, receiver)
        case _              => payload
      }
      send(con, p)
    }
  }

  private def augmentPayloadWithLastKnownDot(payload: Payload[State], receiver: Uid): Message =
    SentCachedMessage(treeContext.getLeaf(receiver) match {
      case Some(leaf) => payload.addLastKnownDot(leaf)
      case None       => payload
    })(using pmscodec)

}
