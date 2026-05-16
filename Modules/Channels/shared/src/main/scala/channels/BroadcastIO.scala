package channels

import channels.*
import channels.BroadcastIO.Envelope
import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast.Event.Send
import channels.broadcast.PlumtreeBroadcast.{Event, Peer}
import channels.broadcast.PlumtreeMessage.*
import channels.broadcast.{BroadcastProtocol, PlumtreeBroadcast, PlumtreeMessage}
import channels.connection.{Abort, ArrayMessageBuffer, ChannelResolver, Connection, ConnectionClosedException, ConnectionDescriptor, LatentConnection, MessageBuffer, NoMoreDataException, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayMessage}
import channels.overlay.{FullMeshOverlay, OverlayController}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.{LocalUid, Uid}
import rdts.time.Dots

import java.net.SocketException
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object BroadcastIO {
  enum Envelope[+State] {
    case Membership(message: OverlayMessage)
    case Broadcast(sender: Uid, message: PlumtreeMessage[State])
  }

  def apply[State](
      replicaId: LocalUid,
      receiveCallback: State => Unit,
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = BroadcastIO.executeImmediately,
      globalAbort: Abort = Abort(),
      broadcast: Option[BroadcastProtocol[State]] = None,
      aead: Aead = Aead.identity,
  )(using stateCodec: JsonValueCodec[State]): BroadcastIO[State] =
    new BroadcastIO[State](
      replicaId = replicaId,
      receiveCallback = receiveCallback,
      overlay = overlay.getOrElse(FullMeshOverlay(PeerConnectInfo(replicaId.uid, Set.empty))),
      resolver = resolver,
      sendingActor = sendingActor,
      globalAbort = globalAbort,
      broadcast =
        broadcast.getOrElse(PlumtreeBroadcast(replicaId.uid, deltaStorage = DiscardingHistory[State](size = 1000))),
      aead = aead,
    )

  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  def messageCodec[State: JsonValueCodec]: JsonValueCodec[Envelope[State]] = JsonCodecMaker.make

  def decodeEnvelope[State: JsonValueCodec](messageBuffer: MessageBuffer, aead: Aead): Try[Envelope[State]] =
    aead.decrypt(messageBuffer.asArray, Aead.emptyAssociatedData).flatMap(bytes =>
      Try(readFromArray[Envelope[State]](bytes)(using BroadcastIO.messageCodec))
    )

  def encodeEnvelope[State: JsonValueCodec](envelope: Envelope[State], aead: Aead) =
    ArrayMessageBuffer(aead.encrypt(writeToArray(envelope)(using BroadcastIO.messageCodec), Aead.emptyAssociatedData))
}

/** Combined Delta + Plumtree dissemination.
  *
  * Network/state-machine split:
  * - the [[BroadcastProtocol]] implementation tracks eager/lazy peer roles and protocol transitions.
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
    @volatile private var broadcast: BroadcastProtocol[State],
    val aead: Aead
)(using val stateCodec: JsonValueCodec[State]) {

  val lock: AnyRef                                                                       = new {}
  @volatile private var connectionDetails: Map[Connection, Option[ConnectionDescriptor]] = Map.empty

  def overlayController: OverlayController = overlay

  def plumtreeState: BroadcastProtocol[State] = broadcast

  def selfConnectionDescriptors: Set[ConnectionDescriptor] = overlay.selfConnectionDescriptors

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()

  private def updateOwnConnectionDescriptor(descriptor: ConnectionDescriptor): Unit = lock.synchronized {
    overlay = overlay.addSelfConnectionDescriptor(descriptor)
  }

  /** Register an outgoing/client transport that yields a usable [[Connection]]. */
  def addClientConnection(latentConnection: LatentConnection[Connection]): Unit =
    Async.provided(globalAbort) {
      bindConnection(latentConnection).bind
    }.run(printExceptionHandler)

  /** Register an incoming/server transport and learn its published local [[ConnectionDescriptor]]. */
  def addServerConnection(latentConnection: LatentConnection[ConnectionDescriptor]): Unit =
    Async.provided(globalAbort) {
      val descriptor = bindConnection(latentConnection).bind
      updateOwnConnectionDescriptor(descriptor)
    }.run(printExceptionHandler)

  private def bindConnection[T](latentConnection: LatentConnection[T]): Async[Abort, T] = {
    latentConnection.prepare { (conn: Connection) =>
      registerConnection(conn, None)
      messageReceiver(conn)
    }
  }

  /** receive message, decode, decrypt. remove peer on failure. */
  private def messageReceiver(conn: Connection): Callback[MessageBuffer] = {
    case Success(msg) =>
      BroadcastIO.decodeEnvelope(msg, aead) match
          case Success(decoded) => handleMessage(decoded, conn)
          case Failure(error)   =>
            println(s"$replicaId: error during message handling")
            error.printStackTrace()
            removePeer(conn)
    case Failure(error) =>
      error match {
        case se: SocketException if se.getMessage == "Connection reset" =>
        // println(s"$replicaId: disconnected ${conn.info} (${conn})")
        case _: (NoMoreDataException | ConnectionClosedException) =>
        // println(s"$replicaId: disconnected ${conn.info} (${conn})")
        case _ =>
          println(s"$replicaId: error during message handling")
          error.printStackTrace()
      }
      removePeer(conn)
  }

  /** Run overlay periodic maintenance (promotion + shuffle) then plumtree graft repair. */
  def tick(): Unit = lock.synchronized {
    applyOverlayResult:
        overlay.tick()
    applyRoutingResult:
        broadcast.tick()
  }

  /** Register externally discovered peers with the overlay. */
  def discover(peers: Set[PeerConnectInfo]): Unit = lock.synchronized {
    applyOverlayResult(overlay.discoverPassive(peers))
  }

  /** Initiate overlay-native bootstrap through a single descriptor. */
  def bootstrapVia(contact: ConnectionDescriptor): Unit = lock.synchronized {
    applyOverlayResult(overlay.bootstrapVia(contact))
  }

  def allPayloads: List[Payload[State]] =
    lock.synchronized(broadcast.allPayloads)

  def broadcast(delta: State): Unit = lock.synchronized {
    applyRoutingResult:
        broadcast.broadcast(delta)
  }

  /** Remove a failed connection from overlay bookkeeping and propagate resulting overlay actions. */
  private def removePeer(conn: Connection): Unit = lock.synchronized {
    val info = connectionDetails.get(conn).flatten
    connectionDetails = connectionDetails.removed(conn)
    val (nextOverlay, actions) = overlay.removeConnection(conn, info)
    overlay = nextOverlay
    actions.foreach(handleOverlayAction)
  }

  /** Track a newly established connection and the optional connect info used to create it. */
  private def registerConnection(conn: Connection, connectInfo: Option[ConnectionDescriptor]): Unit =
    lock.synchronized {
      connectionDetails = connectionDetails.updated(conn, connectInfo)
      applyOverlayResult(overlay.activateConnection(conn, connectInfo))
    }

  private def applyRoutingResult(result: PlumtreeBroadcast.Result[State]): Unit = {
    broadcast = result.state
    result.events.foreach(handleRoutingEvent)
  }

  private def handleRoutingEvent(event: Event[State]): Unit =
    event match
        case Event.Deliver(payload) =>
          receiveCallback(payload.data)
        case Send(peers, message) =>
          peers.foreach(peer =>
            overlay.connectionFor(peer.uid).foreach(send(
              _,
              Envelope.Broadcast(replicaId.uid, message)
            ))
          )

  private def applyOverlayResult(result: (next: OverlayController, actions: List[OverlayAction]))
      : Unit = {
    overlay = result.next
    result.actions.foreach(handleOverlayAction)
  }

  /** Resolve and establish an outgoing connection for the given descriptor, registering it with that descriptor. */
  def addOutgoingConnection(connectInfo: ConnectionDescriptor): Unit =
    resolver.connect(connectInfo) match
        case Some(latentConnection) =>
          Async.provided(globalAbort) {
            val conn = latentConnection.prepare((conn: Connection) => messageReceiver(conn)).bind
            registerConnection(conn, Some(connectInfo))
          }.run(printExceptionHandler)
        case None => ()

  /** Resolve connection details, establish a connection, register it, and send the first payload atomically from the caller's perspective. */
  private def connectAndSend(
      details: Iterable[ConnectionDescriptor],
      expectedPeer: Uid,
      payload: Envelope[State]
  ): Unit =
    details.iterator.flatMap(detail => resolver.connect(detail).map(detail -> _)).nextOption() match
        case Some((detail, latentConnection)) =>
          Async.provided(globalAbort) {
            val conn = latentConnection.prepare((conn: Connection) => messageReceiver(conn)).bind
            registerConnection(conn, Some(detail))
            send(conn, payload)
          }.run(printExceptionHandler)
        case None =>
          ()

  private def handleOverlayAction(action: OverlayAction): Unit =
    action match
        case OverlayAction.Send(connection, message) =>
          send(connection, Envelope.Membership(message))
        case OverlayAction.SendJoin(details, expectedPeer, message) =>
          connectAndSend(details, expectedPeer, Envelope.Membership(message))
        case OverlayAction.Disconnect(connection) =>
          connection.close()
          removePeer(connection)
        case OverlayAction.ActiveConnectionAdded(peer) =>
          applyRoutingResult:
              broadcast.addPeer(Peer(peer))
        case OverlayAction.ActiveConnectionRemoved(peer) =>
          applyRoutingResult:
              broadcast.removePeer(Peer(peer))

  private def send(conn: Connection, payload: Envelope[State]): Unit =
    if globalAbort.closeRequest then ()
    else
        sendingActor.execute { () =>
          conn.send(BroadcastIO.encodeEnvelope(payload, aead)).run {
            case Success(_) => ()
            case Failure(_) => removePeer(conn)
          }
        }

  private def handleMessage(msg: Envelope[State], conn: Connection): Unit = lock.synchronized {
    if globalAbort.closeRequest then return
    msg match
        case Envelope.Membership(message) =>
          applyOverlayResult(overlay.receiveActions(message, conn))
        case Envelope.Broadcast(sender, msg) =>
          applyRoutingResult(broadcast.handleMessage(Peer(sender), msg))
  }
}
