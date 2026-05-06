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
import replication.overlay.{DirectConnectionOverlay, OverlayController}
import replication.overlay.OverlayController.OverlayMessage

import java.net.SocketException
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object BroadcastIO {
  enum Envelope[+State] {
    case Membership(message: OverlayMessage)
    case Protocol(message: PlumtreeMessage[State])
  }

  def apply[State](
      replicaId: LocalUid,
      receiveCallback: State => Unit,
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = BroadcastIO.executeImmediately,
      globalAbort: Abort = Abort(),
      broadcast: Option[PlumtreeBroadcast[State]] = None,
  )(using stateCodec: JsonValueCodec[State]): BroadcastIO[State] =
    new BroadcastIO[State](
      replicaId = replicaId,
      receiveCallback = receiveCallback,
      overlay = overlay.getOrElse(DirectConnectionOverlay(PeerConnectInfo(replicaId.uid, Set.empty))),
      resolver = resolver,
      sendingActor = sendingActor,
      globalAbort = globalAbort,
      plumtree =
        broadcast.getOrElse(PlumtreeBroadcast(replicaId.uid, deltaStorage = DiscardingHistory[State](size = 108)))
    )

  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  def messageCodec[State: JsonValueCodec]: JsonValueCodec[Envelope[State]] = JsonCodecMaker.make

  def decodeEnevlop[State: JsonValueCodec](messageBuffer: MessageBuffer): Envelope[State] =
    readFromArray[Envelope[State]](messageBuffer.asArray)(using BroadcastIO.messageCodec)

  def encodeEnvelope[State: JsonValueCodec](envelope: Envelope[State]) =
    ArrayMessageBuffer(writeToArray(envelope)(using BroadcastIO.messageCodec))
}

/** Combined Delta + Plumtree dissemination.
  *
  * Network/state-machine split:
  * - [[PlumtreeBroadcast]] tracks immutable Plumtree eager/lazy roles and protocol transitions.
  * - the overlay controller owns the mapping between abstract peers and concrete [[Connection]] objects.
  * - this class handles payload storage, message encoding/decoding, and callback side effects.
  */
class BroadcastIO[State](
    val replicaId: LocalUid,
    receiveCallback: State => Unit,
    @volatile private var overlay: OverlayController,
    resolver: ChannelResolver,
    sendingActor: ExecutionContext,
    val globalAbort: Abort,
    @volatile private var plumtree: PlumtreeBroadcast[State]
)(using val stateCodec: JsonValueCodec[State]) {

  val lock: AnyRef = new {}

  def overlayController: OverlayController = overlay

  def plumtreeState: PlumtreeBroadcast[State] = plumtree

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()

  /** Register a transport that already speaks raw [[MessageBuffer]] frames for [[BroadcastIO.Envelope]]. */
  def addBinaryConnection(latentConnection: LatentConnection): Unit =
    Async.provided(globalAbort) {
      val conn = latentConnection.prepare { connectionReceiver }.bind
      registerConnection(conn)
    }.run(printExceptionHandler)

  private val connectionReceiver: Receive = new Receive {
    override def messageHandler(conn: Connection): Callback[MessageBuffer] = new Callback {
      override def complete(tr: Try[MessageBuffer]): Unit = tr match {
        case Success(msg)   => handleMessage(BroadcastIO.decodeEnevlop(msg), conn)
        case Failure(error) =>
          error match {
            case se: SocketException if se.getMessage == "Connection reset" =>
              println(s"$replicaId: disconnected ${conn.info} (${conn})")
            case _: (NoMoreDataException | ConnectionClosedException) =>
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
    val snapshot = lock.synchronized(plumtree.peerRoles.keys.flatMap(peer => overlay.connectionFor(peer.uid)).toList)
    snapshot.foreach(send(_, Envelope.Membership(OverlayMessage.Ping(System.nanoTime()))))
  }

  /** Run overlay periodic maintenance (promotion + shuffle) then plumtree graft repair. */
  def repairTick(): Unit = {
    val actions = lock.synchronized {
      val (next, a) = overlay.tick()
      overlay = next
      a
    }
    actions.foreach(handleOverlayAction)

    val result = lock.synchronized(plumtree.tickGrafts())
    applyRoutingResult(result)
  }

  /** Register externally discovered peers with the overlay. */
  def discover(peers: Set[PeerConnectInfo]): Unit = {
    val (next, actions) = overlay.discoverPeers(peers)
    applyOverlayResult(next, actions)
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

  /** Remove a failed connection from overlay bookkeeping and propagate resulting overlay actions. */
  private def removePeer(conn: Connection): Unit = {
    val actions = lock.synchronized {
      val (nextOverlay, actions) = overlay.removeConnection(conn)
      overlay = nextOverlay
      actions
    }
    actions.foreach(handleOverlayAction)
  }

  /** Attach a newly established connection to the overlay state.
    * Peer identity is learned later from HyParView control-plane messages carried on that connection.
    */
  private def registerConnection(conn: Connection): Unit = {
    val actions = lock.synchronized {
      val (next, actions) = overlay.registerConnection(conn)
      overlay = next
      actions
    }
    actions.foreach(handleOverlayAction)
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
            lock.synchronized(overlay.connectionFor(peer.uid)).foreach(send(_, Envelope.Protocol(message)))
          )

  private def applyOverlayResult(next: OverlayController, actions: List[OverlayController.OverlayAction]): Unit = {
    lock.synchronized {
      overlay = next
    }
    actions.foreach(handleOverlayAction)
  }

  /** Resolve connection details, establish a connection, register it, and send the first payload atomically from the caller's perspective. */
  private def connectAndSend(details: Iterable[ChannelConnectInfo], label: String, payload: Envelope[State]): Unit =
    details.iterator.collectFirst(Function.unlift(detail => resolver.connect(detail, label))) match
        case Some(latentConnection) =>
          Async.provided(globalAbort) {
            val conn = latentConnection.prepare { connectionReceiver }.bind
            registerConnection(conn)
            send(conn, payload)
          }.run(printExceptionHandler)
        case None =>
          ()

  private def handleOverlayAction(action: OverlayController.OverlayAction): Unit =
    action match
        case OverlayController.OverlayAction.Send(connection, message) =>
          send(connection, Envelope.Membership(message))
        case OverlayController.OverlayAction.SendJoin(details, message) =>
          connectAndSend(details, s"$replicaId-join", Envelope.Membership(message))
        case OverlayController.OverlayAction.ActiveConnectionAdded(peer) =>
          val result = lock.synchronized(plumtree.addPeer(Peer(peer)))
          applyRoutingResult(result)
        case OverlayController.OverlayAction.ActiveConnectionRemoved(peer) =>
          lock.synchronized { plumtree = plumtree.removePeer(Peer(peer)) }

  private def send(conn: Connection, payload: Envelope[State]): Unit =
    if globalAbort.closeRequest then ()
    else
        sendingActor.execute { () =>
          conn.send(BroadcastIO.encodeEnvelope(payload)).run {
            case Success(_) => ()
            case Failure(_) => removePeer(conn)
          }
        }

  private def handleMessage(msg: Envelope[State], from: Connection): Unit = synchronized {
    if globalAbort.closeRequest then return

    msg match
        case Envelope.Membership(message) =>
          val (next, actions) = lock.synchronized(overlay.receiveActions(message, from))
          applyOverlayResult(next, actions)
        case Envelope.Protocol(protocol) =>
          val result = lock.synchronized {
            val peer = overlay.peerForConnection(from).map(Peer.apply).orElse(protocolSender(protocol).map(Peer.apply))
            peer match
                case Some(existing) =>
                  // NOTE:
                  // DirectConnectionOverlay currently marks a peer active as soon as it replies with NeighborReply.
                  // That immediately triggers Plumtree `addPeer`, which emits a `Graft`.
                  // This `Graft` can reach the remote side before that side has processed enough overlay traffic to have the
                  // sender installed in its Plumtree peer roles / active broadcast peers.
                  // Ideally the overlay handshake would delay activation until both sides have observed each other as active.
                  // Until that is fixed, we defensively fall back to the sender encoded in the protocol message.
                  val seeded =
                    if plumtree.peerRoles.contains(existing) then plumtree
                    else plumtree.addPeer(existing).state
                  seeded.handleMessage(existing, protocol)
                case None =>
                  throw new IllegalStateException(s"received protocol message $msg from unknown connection $from")
          }
          applyRoutingResult(result)
  }

  private def protocolSender(message: PlumtreeMessage[State]): Option[rdts.base.Uid] =
    message match
        case Graft(sender, _) => Some(sender)
        case IHave(sender, _) => Some(sender)
        case Prune(sender)    => Some(sender)
        case Payload(_, _)    => None
}
