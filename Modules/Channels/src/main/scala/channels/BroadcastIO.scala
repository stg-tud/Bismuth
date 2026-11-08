package channels

import channels.BroadcastIO.Envelope
import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast.{Event, Peer}
import channels.broadcast.PlumtreeMessage.*
import channels.broadcast.{BroadcastProtocol, PlumtreeBroadcast, PlumtreeMessage}
import channels.connection.*
import channels.experiments.Aead
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}
import channels.overlay.{AttemptId, FullMeshOverlay, OverlayController}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.{ReplicaId, Uid}
import rdts.time.{Dot, Dots}

import java.net.SocketException
import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object BroadcastIO {
  enum Envelope[+State] {
    case Membership(message: OverlayMessage)
    case Broadcast(sender: Uid, message: PlumtreeMessage[State])
  }

  /** `receiveCallback` gets each delivered delta together with the dots assigned to it by the sender. */
  def apply[State](
      replicaId: ReplicaId,
      receiveCallback: Payload[State] => Unit,
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = BroadcastIO.executeImmediately,
      globalAbort: Abort = Abort(),
      broadcast: Option[BroadcastProtocol[State]] = None,
      aead: Aead = Aead.identity,
      timers: Timers = Timers.Manual(),
      timerDelays: TimerDelays = TimerDelays(),
  )(using stateCodec: JsonValueCodec[State]): BroadcastIO[State] =
    new BroadcastIO[State](
      replicaId = replicaId,
      receiveCallback = receiveCallback,
      overlay = overlay.getOrElse(FullMeshOverlay(PeerConnectInfo(replicaId.uid, Set.empty))),
      resolver = resolver,
      sendingActor = sendingActor,
      globalAbort = globalAbort,
      broadcast =
        broadcast.getOrElse(PlumtreeBroadcast(deltaStorage = DiscardingHistory[State](size = 1000))),
      aead = aead,
      timers = timers,
      timerDelays = timerDelays,
    )

  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  def messageCodec[State: JsonValueCodec]: JsonValueCodec[Envelope[State]] = JsonCodecMaker.make

  def decodeEnvelope[State: JsonValueCodec](messageBuffer: MessageBuffer, aead: Aead): Try[Envelope[State]] =
    aead.decrypt(messageBuffer.asByteBuffer, Aead.emptyAssociatedData).flatMap(bytes =>
      Try(readFromByteBuffer[Envelope[State]](bytes)(using BroadcastIO.messageCodec))
    )

  def encodeEnvelope[State: JsonValueCodec](envelope: Envelope[State], aead: Aead): ByteBufferMessageBuffer =
    ByteBufferMessageBuffer(aead.encrypt(
      ByteBuffer.wrap(writeToArray(envelope)(using BroadcastIO.messageCodec)),
      Aead.emptyAssociatedData
    ))
}

/** Driver connecting transports, an overlay state machine, and a broadcast state machine.
  *
  * - the overlay ([[channels.overlay.OverlayController]]) decides which connections exist and which peers are neighbors
  * - the broadcast protocol ([[channels.broadcast.BroadcastProtocol]]) decides what to send to which neighbor
  * - this class only translates: transport callbacks become overlay events, actions become transport operations,
  *   timer requests are scheduled on [[channels.Timers]], and the overlay's `NeighborUp`/`NeighborDown` are passed on to the
  *   broadcast protocol. It makes no protocol decisions of its own, and keeps no state besides the two state machines.
  *
  * Concurrency: each input runs the state transitions under `lock` and gets back the resulting [[Output]]s, which are
  * performed on the calling thread after the lock is released. Exceptions therefore reach the caller. Outputs that
  * synchronously cause further inputs (e.g. synchronous local connections) simply recurse on already committed state.
  */
class BroadcastIO[State](
    val replicaId: ReplicaId,
    receiveCallback: Payload[State] => Unit,
    @volatile private var overlay: OverlayController,
    resolver: ChannelResolver,
    sendingActor: ExecutionContext,
    val globalAbort: Abort,
    @volatile private var broadcast: BroadcastProtocol[State],
    val aead: Aead,
    val timers: Timers,
    timerDelays: TimerDelays,
)(using val stateCodec: JsonValueCodec[State]) {

  /** A broadcast message to send on `connection`, resolved from the overlay during the transition. */
  final private case class Transmit(connection: Connection, envelope: Envelope[State])

  /** What the transitions ask for. `NeighborUp`/`NeighborDown` and broadcast `Send`s are resolved inside the transition. */
  private type Output = OverlayAction | Event[State] | Transmit

  private val lock: AnyRef = new {}

  /** Connections not dialed by the overlay (accepted by a listener, or added via `addClientConnection`). */
  private val incoming: Receive = (conn: Connection) => {
    transition(overlayStep(OverlayEvent.Incoming(conn)))
    messageReceiver(conn)
  }

  // ------------------------------------------------------------------------------------------------
  // snapshots
  // ------------------------------------------------------------------------------------------------

  def overlayController: OverlayController = overlay

  def plumtreeState: BroadcastProtocol[State] = broadcast

  def selfConnectionDescriptors: Set[ConnectionDescriptor] = overlay.selfConnectionDescriptors

  /** The transport connection backing the active edge to `peer`, if any. */
  def connectionTo(peer: Uid): Option[Connection] = overlay.connectionFor(peer)

  def allPayloads: List[Payload[State]] = broadcast.allPayloads

  // ------------------------------------------------------------------------------------------------
  // public API
  // ------------------------------------------------------------------------------------------------

  /** Register an outgoing/client transport that yields a usable `Connection` (not dialed by the overlay). */
  def addClientConnection(connector: Connector): Unit =
    connector.connect(incoming).runIn(globalAbort)(printExceptionHandler)

  /** Register an incoming/server transport and learn its published local `ConnectionDescriptor`. */
  def addServerConnection(listener: Listener): Unit =
    Async.provided(globalAbort) {
      val descriptor = listener.listen(incoming).bind
      transition(overlayStep(OverlayEvent.SelfDescriptorAdded(descriptor)))
    }.run(printExceptionHandler)

  /** Register externally discovered peers with the overlay. */
  def discover(peers: Set[PeerConnectInfo]): Unit = transition(overlayStep(OverlayEvent.Discovered(peers)))

  /** Initiate overlay-native bootstrap through a single descriptor. */
  def bootstrapVia(contact: ConnectionDescriptor): Unit = transition(overlayStep(OverlayEvent.Bootstrap(contact)))

  /** Disseminate a local delta and return the dot assigned to it (`None` if this BroadcastIO is closed).
    * Receivers get the same dot as part of the [[Payload]], which allows applications to track causality.
    */
  def broadcast(delta: State): Option[Dot] = {
    var assigned: Option[Dot] = None
    transition(broadcastStep { protocol =>
      val dot = protocol.localContext.nextDot(replicaId.uid)
      assigned = Some(dot)
      protocol.broadcast(Payload(Dots.single(dot), delta))
    })
    assigned
  }

  // ------------------------------------------------------------------------------------------------
  // transitions (run under `lock`, no I/O)
  // ------------------------------------------------------------------------------------------------

  /** Run `step` under the lock, then perform its outputs without the lock. */
  private def transition(step: => List[Output]): Unit =
    lock.synchronized {
      if globalAbort.closeRequest then Nil else step
    }.foreach(perform)

  private def overlayStep(event: OverlayEvent): List[Output] = {
    val (next, actions) = overlay.step(event)
    overlay = next
    actions.flatMap {
      case OverlayAction.NeighborUp(peer)   => broadcastStep(_.addPeer(Peer(peer)))
      case OverlayAction.NeighborDown(peer) => broadcastStep(_.removePeer(Peer(peer)))
      case other                            => List(other)
    }
  }

  private def broadcastStep(f: BroadcastProtocol[State] => PlumtreeBroadcast.Result[State]): List[Output] = {
    val result = f(broadcast)
    broadcast = result.state
    result.events.toList.flatMap {
      case Event.Send(peers, message) =>
        peers.flatMap(peer =>
          overlay.connectionFor(peer.uid).map(Transmit(_, Envelope.Broadcast(replicaId.uid, message)))
        )
      case other => List(other)
    }
  }

  private def handleIncoming(conn: Connection, message: MessageBuffer): List[Output] =
    BroadcastIO.decodeEnvelope(message, aead) match
        case Success(Envelope.Membership(overlayMessage)) =>
          overlayStep(OverlayEvent.Received(conn, overlayMessage))
        case Success(Envelope.Broadcast(sender, broadcastMessage)) =>
          // messages still arriving on a connection the overlay already closed (e.g. the losing one of two
          // connections to the same peer) would be handled as duplicates and prune the broadcast tree
          if overlay.holds(conn)
          then broadcastStep(_.handleMessage(Peer(sender), broadcastMessage))
          else Nil
        case Failure(error) =>
          println(s"$replicaId: error during message decoding")
          error.printStackTrace()
          OverlayAction.Close(conn) :: overlayStep(OverlayEvent.Closed(conn))

  // ------------------------------------------------------------------------------------------------
  // performing outputs
  // ------------------------------------------------------------------------------------------------

  private def perform(output: Output): Unit = output match
      case Transmit(connection, envelope)    => send(connection, envelope)
      case OverlayAction.Send(conn, message) => send(conn, Envelope.Membership(message))
      case OverlayAction.Close(conn)         => conn.close()
      case OverlayAction.Dial(attempt, to)   => dial(attempt, to)
      case OverlayAction.StartTimer(timer)   =>
        timers.schedule(timerDelays(timer))(() => transition(overlayStep(OverlayEvent.TimerFired(timer))))
      case _: (OverlayAction.NeighborUp | OverlayAction.NeighborDown | Event.Send[?]) =>
        throw IllegalStateException(s"resolved during the transition: $output")
      case Event.Deliver(payload)  => receiveCallback(payload)
      case Event.StartTimer(timer) =>
        timers.schedule(timerDelays(timer))(() => transition(broadcastStep(_.timerFired(timer))))

  private def send(connection: Connection, envelope: Envelope[State]): Unit =
    if !globalAbort.closeRequest then
        sendingActor.execute { () =>
          connection.send(BroadcastIO.encodeEnvelope(envelope, aead)).run {
            case Success(_) => ()
            case Failure(_) =>
              connection.close()
              transition(overlayStep(OverlayEvent.Closed(connection)))
          }
        }

  /** Establish a connection requested by the overlay; reports `Established` or `DialFailed`. */
  private def dial(attempt: AttemptId, to: Set[ConnectionDescriptor]): Unit =
    to.iterator.flatMap(resolver.connect).nextOption() match
        case None            => transition(overlayStep(OverlayEvent.DialFailed(attempt)))
        case Some(connector) =>
          connector.connect { (conn: Connection) =>
            transition(overlayStep(OverlayEvent.Established(attempt, conn)))
            messageReceiver(conn)
          }.runIn(globalAbort) {
            case Success(_) => ()
            case Failure(_) => transition(overlayStep(OverlayEvent.DialFailed(attempt)))
          }

  private def messageReceiver(conn: Connection): Callback[MessageBuffer] = {
    case Success(message) => transition(handleIncoming(conn, message))
    case Failure(error)   =>
      error match
          case se: SocketException if se.getMessage == "Connection reset" =>
          case _: (NoMoreDataException | ConnectionClosedException)       =>
          case _                                                          =>
            println(s"$replicaId: error during message handling")
            error.printStackTrace()
      transition(overlayStep(OverlayEvent.Closed(conn)))
  }

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()
}
