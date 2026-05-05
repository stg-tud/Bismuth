package replication

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.LocalUid
import rdts.time.Dots
import replication.BroadcastIO.Envelope
import replication.JsoniterCodecs.given
import replication.PlumtreeBroadcast.Event.Send
import replication.PlumtreeBroadcast.{Event, Peer}
import replication.PlumtreeMessage.*
import replication.overlay.OverlayController
import replication.overlay.OverlayController.OverlayMessage

import java.net.SocketException
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object BroadcastIO {
  enum Envelope[+State] {
    case Membership(message: OverlayMessage)
    case Protocol(message: PlumtreeMessage[State])
    case Ping(time: Long)
    case Pong(time: Long)
  }

  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  def messageCodec[State: JsonValueCodec]: JsonValueCodec[Envelope[State]] = JsonCodecMaker.make

  def objectMessages[State: JsonValueCodec](conn: LatentConnection[MessageBuffer])
      : LatentConnection[Envelope[State]] =
    LatentConnection.adapt(
      (mb: MessageBuffer) => readFromArray[Envelope[State]](mb.asArray)(using BroadcastIO.messageCodec),
      (pm: Envelope[State]) => ArrayMessageBuffer(writeToArray(pm)(using BroadcastIO.messageCodec)),
      "broadcast io serialization"
    )(conn)
}

/** Combined Delta + Plumtree dissemination.
  *
  * Network/state-machine split:
  * - [[PlumtreeBroadcast]] tracks immutable Plumtree eager/lazy roles and protocol transitions.
  * - this class maps concrete connections to abstract peers and handles payload storage / callback side effects.
  */
class BroadcastIO[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    resolver: ChannelResolver[Envelope[State]] = ChannelResolver.disconnected[Envelope[State]],
    sendingActor: ExecutionContext = BroadcastIO.executeImmediately,
    val globalAbort: Abort = Abort(),
    val deltaStorage: DeltaStorage[State] = DiscardingHistory[State](size = 108),
    @volatile private var overlay: OverlayController = OverlayController.none
)(using val stateCodec: JsonValueCodec[State]) {

  val lock: AnyRef = new {}

  @volatile private var connections: Map[Peer, Connection[Envelope[State]]]       = Map.empty
  @volatile private var peersByConnection: Map[Connection[Envelope[State]], Peer] = Map.empty
  @volatile private var plumtree: PlumtreeBroadcast[State]                =
    PlumtreeBroadcast(replicaId.uid, deltaStorage = deltaStorage)

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()

  def addBinaryConnection(latentConnection: LatentConnection[MessageBuffer]): Unit =
    addConnection(BroadcastIO.objectMessages(latentConnection))

  def addConnection(latentConnection: LatentConnection[Envelope[State]]): Unit =
    Async.provided(globalAbort) {
      val conn = latentConnection.prepare { connectionReceiver }.bind
      applyRoutingResult(registerConnection(conn))
    }.run(printExceptionHandler)

  private val connectionReceiver: Receive[Envelope[State]] = new Receive[Envelope[State]] {
    override def messageHandler(conn: Connection[Envelope[State]]): Callback[Envelope[State]] = new Callback {
      override def complete(tr: Try[Envelope[State]]): Unit = tr match {
        case Success(msg)   => handleMessage(msg, conn)
        case Failure(error) =>
          error match {
            case se: SocketException if se.getMessage == "Connection reset" =>
              println(s"$replicaId: disconnected ${conn.info} (${conn})")
            case _: NoMoreDataException =>
              println(s"$replicaId: disconnected ${conn.info} (${conn})")
            case _ =>
              println(s"$replicaId: error during message handling")
              error.printStackTrace()
          }
          removePeer(conn)
      }
    }
  }

  def pingAll(): Unit = synchronized {
    val snapshot = lock.synchronized(connections.values.toList)
    snapshot.foreach(send(_, Envelope.Ping(System.nanoTime())))
  }

  def repairTick(): Unit = {
    val result = lock.synchronized(plumtree.tickGrafts())
    applyRoutingResult(result)
  }

  private def localKnownDeltaContext: Dots = lock.synchronized(plumtree.localContext)

  def allPayloads: List[Payload[State]] =
    lock.synchronized(plumtree.deltaStorage.getHistory)

  def applyDelta(delta: State): Unit = {
    val nextDot = localKnownDeltaContext.nextDot(replicaId.uid)
    val payload = Payload(Dots.single(nextDot), delta)
    val result  = lock.synchronized(plumtree.broadcast(payload))
    applyRoutingResult(result)
  }

  private def removePeer(conn: Connection[Envelope[State]]): Unit = {
    lock.synchronized {
      peersByConnection.get(conn) match
          case Some(peer) =>
            connections = connections.removed(peer)
            peersByConnection = peersByConnection.removed(conn)
            plumtree = plumtree.removePeer(peer)
          case None => ()
    }
  }

  private def registerConnection(conn: Connection[Envelope[State]]): PlumtreeBroadcast.Result[State] =
    lock.synchronized {
      peersByConnection.get(conn) match
          case Some(peer) =>
            connections = connections.updated(peer, conn)
            PlumtreeBroadcast.Result(plumtree, Nil)
          case None =>
            val peer = Peer(conn.authenticatedPeerReplicaId.getOrElse(LocalUid.gen().uid))
            connections = connections.updated(peer, conn)
            peersByConnection = peersByConnection.updated(conn, peer)
            plumtree.addPeer(peer)
    }

  private def applyRoutingResult(result: PlumtreeBroadcast.Result[State]): Unit = {
    val events = lock.synchronized {
      plumtree = result.state
      result.events
    }
    events.foreach(handleRoutingEvent)
  }

  private def handleRoutingEvent(event: Event[State]): Unit =
    event match
        case Event.Deliver(payload) =>
          receiveCallback(payload.data)
        case Send(peers, message) =>
          peers.foreach(peer =>
            lock.synchronized(connections.get(peer)).foreach(send(_, Envelope.Protocol(message)))
          )

  private def applyOverlayResult(next: OverlayController, actions: List[OverlayController.OverlayAction]): Unit = {
    lock.synchronized {
      overlay = next
    }
    actions.foreach(handleOverlayAction)
  }

  private def connectAndSend(details: Iterable[ChannelConnectInfo], label: String, payload: Envelope[State]): Unit =
    details.iterator.collectFirst(Function.unlift(detail => resolver.connect(detail, label))) match
        case Some(latentConnection) =>
          Async.provided(globalAbort) {
            val conn = latentConnection.prepare { connectionReceiver }.bind
            applyRoutingResult(registerConnection(conn))
            send(conn, payload)
          }.run(printExceptionHandler)
        case None =>
          ()

  private def handleOverlayAction(action: OverlayController.OverlayAction): Unit =
    action match
        case OverlayController.OverlayAction.Send(to, message) =>
          lock.synchronized(connections.get(Peer(to.uid))) match
              case Some(conn) => send(conn, Envelope.Membership(message))
              case None       =>
                connectAndSend(to.channelConnectors, s"$replicaId->${to.uid}", Envelope.Membership(message))
        case OverlayController.OverlayAction.SendJoin(details, message) =>
          connectAndSend(details, s"$replicaId-join", Envelope.Membership(message))

  private def send(conn: Connection[Envelope[State]], payload: Envelope[State]): Unit =
    if globalAbort.closeRequest then ()
    else
        sendingActor.execute { () =>
          conn.send(payload).run {
            case Success(_) => ()
            case Failure(_) => removePeer(conn)
          }
        }

  private def handleMessage(msg: Envelope[State], from: Connection[Envelope[State]]): Unit = synchronized {
    if globalAbort.closeRequest then return

    val peer = lock.synchronized(peersByConnection.get(from)) match
        case Some(existing) => existing
        case None           =>
          val registration = registerConnection(from)
          applyRoutingResult(registration)
          lock.synchronized(peersByConnection(from))

    msg match
        case Envelope.Ping(time) =>
          send(from, Envelope.Pong(time))
        case Envelope.Pong(_) =>
          ()
        case Envelope.Membership(message) =>
          val (next, actions) = lock.synchronized(overlay.receiveActions(message))
          applyOverlayResult(next, actions)
        case Envelope.Protocol(protocol) =>
          val result = lock.synchronized(plumtree.handleMessage(peer, protocol))
          applyRoutingResult(result)
  }
}
