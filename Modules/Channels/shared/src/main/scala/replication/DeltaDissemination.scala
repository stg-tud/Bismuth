package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.Lattice.syntax
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.time.Dots
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
    val msg = SentCachedMessage(Request(replicaId.uid, selfContext))(using pmscodec)
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
      println("exception during connection activation")
      ex.printStackTrace()
    case Success(_) => ()

  def addBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Unit =
    prepareBinaryConnection(latentConnection).run(printExceptionHandler)

  def addObjectConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit =
    prepareObjectConnection(latentConnection).run(printExceptionHandler)

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
          println("exception during message handling")
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
        selfContext
      ))(using pmscodec)
    )
  }

  // note that deltas are not guaranteed to be ordered the same in the buffers
  val lock: AnyRef = new {}

  private var contexts: Map[Uid, Dots] = Map.empty

  def selfContext: Dots = contexts.getOrElse(replicaId.uid, Dots.empty)

  def applyDelta(delta: State): Unit =
    val message = lock.synchronized {
      val nextDot = selfContext.nextDot(replicaId.uid)
      val payload = Payload(replicaId.uid, Dots.single(nextDot), delta)
      updateContext(replicaId.uid, payload.dots)
      val message = SentCachedMessage(payload)(using pmscodec)
      rememberPayload(message)
      message
    }
    disseminate(message)

  def updateContext(rr: Uid, dots: Dots): Unit = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr `merge` Some(dots))
  }

  def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(deltaStorage.getHistory)

  def rememberPayload(message: CachedMessage[Payload[State]]): Unit = lock.synchronized(deltaStorage.remember(message))

  def handleMessage(msg: Message, from: ConnectionContext): Unit = {
    if globalAbort.closeRequest then return
    msg.payload match
      case Ping(time) =>
        send(from, SentCachedMessage(Pong(time))(using pmscodec))
      case Pong(time) =>
      // println(s"ping took ${(System.nanoTime() - time.toLong).doubleValue / 1000_000}ms")
      case Request(uid, knows) =>
        val (relevant, context) = lock.synchronized {
          val relevant = allPayloads.filterNot { dt => dt.payload.dots <= knows }
          val newknowledge =
            knows.merge(relevant.map { dt => dt.payload.dots }.reduceOption(Lattice.merge).getOrElse(Dots.empty))
          val context = selfContext
          val diff = context `subtract` newknowledge
          if !diff.isEmpty then
            throw IllegalStateException(
              s"could not answer request, missing deltas for: ${diff}\n  relevant: ${relevant.map(_.payload)}\n knows: ${knows}\n  selfcontext: ${selfContext}}"
            )
          (relevant, context)
        }
        relevant.foreach: msg =>
          send(from, SentCachedMessage(msg.payload.addSender(replicaId.uid))(using pmscodec))
        updateContext(uid, context `merge` knows)
      case payload@Payload(uid, context, data, redundantDots) =>
        if context <= selfContext then return
        lock.synchronized {
          uid.foreach { uid =>
            updateContext(uid, context)
          }
          updateContext(replicaId.uid, context)
          rememberPayload(msg.asInstanceOf[CachedMessage[Payload[State]]])
        }
        receiveCallback(data)
        if immediateForward then
          disseminate(msg, Set(from))

  }

  def send(con: ConnectionContext, payload: Message): Unit =
    if globalAbort.closeRequest then ()
    else
      sendingActor.execute { () =>
        con.send(payload).run(debugCallbackAndRemoveCon(con))
      }

  def disseminate(payload: Message, except: Set[ConnectionContext] = Set.empty): Unit = {
    val cons = lock.synchronized(connections)
    cons.filterNot(con => except.contains(con)).foreach: con =>
      send(con, payload)

  }

}
