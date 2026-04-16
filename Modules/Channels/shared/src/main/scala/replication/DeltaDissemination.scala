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

import java.net.SocketException
import scala.annotation.unused
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

/** Combined Delta + Plumtree dissemination.
  *
  * Public surface intentionally mirrors DeltaDissemination for compatibility (`applyDelta`, `receiveCallback`).
  */
class DeltaDissemination[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    @unused crypto: Option[Aead] = None,
    defaultTimetolive: Int = 0,
    sendingActor: ExecutionContext = DeltaDissemination.executeImmediately,
    val globalAbort: Abort = Abort(),
    val deltaStorage: DeltaStorage[State] = DiscardingHistory[State](size = 108),
)(using JsonValueCodec[State]) {

  type Message = CachedMessage[ProtocolMessage[State]]

  given LocalUid = replicaId

  enum PeerRole {
    case Eager, Lazy
  }

  case class PeerState(conn: Connection[Message], var role: PeerRole = PeerRole.Eager)

  type ConnectionContext = Connection[Message]

  @volatile private var peers: Map[ConnectionContext, PeerState] = Map.empty

  private def cachedMessages(conn: LatentConnection[MessageBuffer]): LatentConnection[Message] =
    LatentConnection.adapt(
      (mb: MessageBuffer) => ReceivedCachedMessage[ProtocolMessage[State]](mb)(using pmscodec),
      (pm: Message) => pm.messageBuffer,
      "json caching"
    )(conn)

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()

  def addBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Unit =
    prepareBinaryConnection(latentConnection).run(printExceptionHandler)

  def addObjectConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Unit =
    prepareObjectConnection(latentConnection).run(printExceptionHandler)

  def prepareBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Async[Any, Unit] =
    prepareLatentConnection(cachedMessages(latentConnection))

  def prepareObjectConnection(latentConnection: LatentConnection[ProtocolMessage[State]]): Async[Any, Unit] =
    prepareLatentConnection(LatentConnection.adapt[ProtocolMessage[State], Message](
      pm => SentCachedMessage(pm)(using pmscodec),
      cm => cm.payload,
      "message serialization"
    )(latentConnection))

  def prepareLatentConnection(latentConnection: LatentConnection[Message]): Async[Any, Unit] = {
    val preparedConnection: Async[Abort, Connection[Message]] = latentConnection.prepare { from =>
      new Callback {
        override def complete(tr: Try[Message]): Unit = tr match {
          case Success(msg)   => handleMessage(msg, from)
          case Failure(error) =>
            error match {
              case se: SocketException if se.getMessage == "Connection reset" =>
                println(s"$replicaId: disconnected ${from.info} (${from})")
              case _: NoMoreDataException =>
                println(s"$replicaId: disconnected ${from.info} (${from})")
              case _ =>
                println(s"$replicaId: error during message handling")
                error.printStackTrace()
            }
            removePeer(from)
        }
      }
    }

    Async.provided(globalAbort) {
      val conn = preparedConnection.bind
      lock.synchronized {
        peers = peers.updated(conn, PeerState(conn, PeerRole.Eager))
      }
      // Ask for missing state on new link.
      send(conn, SentCachedMessage(Request(replicaId.uid, localKnownDeltaContext))(using pmscodec))
    }
  }

  def requestData(): Unit = {
    val msg      = SentCachedMessage(Request(replicaId.uid, localKnownDeltaContext))(using pmscodec)
    val snapshot = lock.synchronized(peers.values.map(_.conn).toList)
    snapshot.foreach(send(_, msg))
  }

  def pingAll(): Unit = {
    val msg      = SentCachedMessage(Ping(System.nanoTime()))(using pmscodec)
    val snapshot = lock.synchronized(peers.values.map(_.conn).toList)
    snapshot.foreach(send(_, msg))
  }

  val lock: AnyRef = new {}

  @volatile private var localContext: Dots = Dots.empty

  def localKnownDeltaContext: Dots      = localContext
  def addLocalContext(dots: Dots): Unit = lock.synchronized { localContext = localContext.merge(dots) }

  def allPayloads: List[CachedMessage[Payload[State]]] = lock.synchronized(deltaStorage.getHistory)

  def rememberPayload(message: CachedMessage[Payload[State]]): Unit =
    lock.synchronized(deltaStorage.remember(message))

  def applyDelta(delta: State, timetolive: Int = defaultTimetolive): Unit = {
    val message = lock.synchronized {
      val nextDot = localKnownDeltaContext.nextDot(replicaId.uid)
      val payload = Payload(Dots.single(nextDot), delta, timetolive)
      addLocalContext(payload.dots)
      val msg = SentCachedMessage(payload)(using pmscodec)
      rememberPayload(msg)
      msg
    }
    disseminate(message)
  }

  private def removePeer(conn: ConnectionContext): Unit =
    lock.synchronized {
      peers = peers.removed(conn)
    }

  private def setRole(conn: ConnectionContext, role: PeerRole): Unit =
    lock.synchronized {
      peers.get(conn).foreach(_.role = role)
    }

  private def disseminate(payload: Message, except: Set[ConnectionContext] = Set.empty): Unit = {
    val (eagerPeers, lazyPeers) = lock.synchronized {
      val ps = peers.values.filterNot(p => except.contains(p.conn)).toList
      (ps.filter(_.role == PeerRole.Eager).map(_.conn), ps.filter(_.role == PeerRole.Lazy).map(_.conn))
    }

    // eager push full payload
    eagerPeers.foreach(send(_, payload))

    // lazy push IHave context summary
    if lazyPeers.nonEmpty then
        val ih = SentCachedMessage(IHave(replicaId.uid, localKnownDeltaContext))(using pmscodec)
        lazyPeers.foreach(send(_, ih))
  }

  private def send(conn: ConnectionContext, payload: Message): Unit =
    if globalAbort.closeRequest then ()
    else
        sendingActor.execute { () =>
          conn.send(payload).run {
            case Success(_) => ()
            case Failure(_) => removePeer(conn)
          }
        }

  private def handleMessage(msg: Message, from: ConnectionContext): Unit = {
    if globalAbort.closeRequest then return

    msg.payload match
        case Ping(time) =>
          send(from, SentCachedMessage(Pong(time))(using pmscodec))

        case Pong(_) => ()

        case Prune(_) =>
          setRole(from, PeerRole.Lazy)

        case IHave(_, knows) =>
          // If remote knows something we do not, request missing via existing Request mechanism.
          if !(knows <= localKnownDeltaContext) then
              send(from, SentCachedMessage(Request(replicaId.uid, localKnownDeltaContext))(using pmscodec))

        case Request(_, knows) =>
          // Request acts as graft/repair signal: keep requester eager.
          setRole(from, PeerRole.Eager)

          val relevant = lock.synchronized {
            allPayloads.filterNot(dt => dt.payload.dots <= knows)
          }
          relevant.foreach(send(from, _))

        case payload @ Payload(context, data, _, timetolive) =>
          if context <= localKnownDeltaContext then
              // duplicate path => prune sender edge for eager forwarding
              send(from, SentCachedMessage(Prune(replicaId.uid))(using pmscodec))
              setRole(from, PeerRole.Lazy)
              return

          lock.synchronized {
            addLocalContext(context)
            rememberPayload(msg.asInstanceOf[CachedMessage[Payload[State]]])
          }

          receiveCallback(data)
          setRole(from, PeerRole.Eager)

          if timetolive > 0 then
              val forwarded = SentCachedMessage(payload.copy(timetolive = timetolive - 1))(using pmscodec)
              disseminate(forwarded, except = Set(from))
          else
              // still lazy-advertise new knowledge
              disseminate(msg, except = Set(from))
  }
}
