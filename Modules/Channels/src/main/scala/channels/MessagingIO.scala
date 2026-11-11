package channels

import channels.MessagingIO.Envelope
import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeMessage.*
import channels.broadcast.Routing.{Effect, Step, Timer}
import channels.broadcast.{PlumtreeBroadcast, PlumtreeMessage, Routing}
import channels.connection.*
import channels.experiments.Aead
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}
import channels.overlay.{AttemptId, FullMeshOverlay, OverlayController}
import channels.routing.CausalPlumtree
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.ReplicaId

import java.net.SocketException
import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object MessagingIO {

  /** A [[MessagingIO]] running [[PlumtreeBroadcast]]. */
  type Plumtree[State] = MessagingIO[PlumtreeBroadcast.Protocol[State]]

  /** Settings shared by all [[MessagingIO]] instances. */
  final case class Config(
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = MessagingIO.executeImmediately,
      globalAbort: Abort = Abort(),
      aead: Aead = Aead.identity,
      timers: Timers = Timers.Manual(),
      timerDelays: TimerDelays = TimerDelays(),
  ) {
    def overlayController(replicaId: ReplicaId): OverlayController = {
      overlay.foreach(o =>
        require(
          o.selfPeer.uid == replicaId.uid,
          s"overlay identity ${o.selfPeer} does not match $replicaId"
        )
      )
      overlay.getOrElse(FullMeshOverlay(PeerConnectInfo(Peer.Claimed(replicaId.uid), Set.empty)))
    }
  }

  val executeImmediately: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit     = runnable.run()
    override def reportFailure(cause: Throwable): Unit = throw cause
  }

  enum Envelope[+Msg] {
    case Membership(message: OverlayMessage)

    /** `sender` is part of the message, as a single (unauthenticated) connection may be shared by multiple peers.
      * Receivers only accept it if the connection may carry messages of `sender`, see [[Peer.mayUse]].
      */
    case Broadcast(sender: Peer, message: Msg)
  }

  /** `receiveCallback` gets each delivered delta together with the dots assigned to it by the sender.
    * `replicaId` is the identity of the default overlay; a given `overlay` must use the same id.
    */
  def usingPlumtree[State](
      replicaId: ReplicaId,
      receiveCallback: Payload[State] => Unit,
      history: DeltaStorage[State] = DiscardingHistory[State](size = 1000),
      config: Config = Config(),
  )(using stateCodec: JsonValueCodec[State]): MessagingIO.Plumtree[State] =
    new MessagingIO[PlumtreeBroadcast.Protocol[State]](
      using
      PlumtreeBroadcast.protocol[State],
      PlumtreeBroadcast.messageCodec[State]
    )(
      replicaId,
      PlumtreeBroadcast(deltaStorage = history),
      receiveCallback,
      config,
    )

  def messageCodec[Msg: JsonValueCodec]: JsonValueCodec[Envelope[Msg]] = JsonCodecMaker.make

  def decodeMessage[Msg: JsonValueCodec](messageBuffer: MessageBuffer, aead: Aead): Try[Envelope[Msg]] =
    aead.decrypt(messageBuffer.asByteBuffer, Aead.emptyAssociatedData).flatMap(bytes =>
      Try(readFromByteBuffer[Envelope[Msg]](bytes)(using MessagingIO.messageCodec))
    )

  def encodeMessage[Msg: JsonValueCodec](envelope: Envelope[Msg], aead: Aead): ByteBufferMessageBuffer =
    ByteBufferMessageBuffer(aead.encrypt(
      ByteBuffer.wrap(writeToArray(envelope)(using MessagingIO.messageCodec)),
      Aead.emptyAssociatedData
    ))

  /** Plumtree envelope with deltas of type `State`. */
  def decodeEnvelope[State: JsonValueCodec](
      messageBuffer: MessageBuffer,
      aead: Aead
  ): Try[Envelope[PlumtreeMessage[State]]] =
    decodeMessage(messageBuffer, aead)(using PlumtreeBroadcast.messageCodec[State])

  def encodeEnvelope[State: JsonValueCodec](
      envelope: Envelope[PlumtreeMessage[State]],
      aead: Aead
  ): ByteBufferMessageBuffer =
    encodeMessage(envelope, aead)(using PlumtreeBroadcast.messageCodec[State])
}

/** Driver connecting transports, an overlay state machine, and a broadcast protocol state machine.
  *
  * - the overlay ([[channels.overlay.OverlayController]]) decides which connections exist and which peers are neighbors
  * - the protocol (an instance of [[channels.broadcast.Routing]]) decides what to send to which
  *   neighbor, and what to deliver to the application
  * - this class only translates: transport callbacks become overlay events, actions become transport operations,
  *   timer requests are scheduled on [[channels.Timers]], and the overlay's `NeighborUp`/`NeighborDown` are passed on to the
  *   protocol. It makes no protocol decisions of its own, and keeps no state besides the two state machines.
  *
  * The application interacts with the protocol through [[state]] and [[update]]; protocols typically provide a more
  * convenient API as extension methods in the companion of their state type (see [[PlumtreeBroadcast]], [[CausalPlumtree]]).
  *
  * Concurrency: each input runs the state transitions under `lock` and gets back the resulting [[Output]]s, which are
  * performed on the calling thread after the lock is released. Exceptions therefore reach the caller. Outputs that
  * synchronously cause further inputs (e.g. synchronous local connections) simply recurse on already committed state.
  */
class MessagingIO[P <: Routing](
    using
    val protocol: P,
    val msgCodec: JsonValueCodec[protocol.Msg],
)(
    replicaId: ReplicaId,
    initial: protocol.State,
    deliver: protocol.Out => Unit,
    config: MessagingIO.Config = MessagingIO.Config(),
) {

  @volatile private var overlay: OverlayController    = config.overlayController(replicaId)
  @volatile private var protocolState: protocol.State = initial

  val globalAbort: Abort                     = config.globalAbort
  val aead: Aead                             = config.aead
  val timers: Timers                         = config.timers
  private val resolver: ChannelResolver      = config.resolver
  private val sendingActor: ExecutionContext = config.sendingActor
  private val timerDelays: TimerDelays       = config.timerDelays

  /** A protocol message to send on `connection`, resolved from the overlay during the transition. */
  final private case class Transmit(connection: Connection, envelope: Envelope[protocol.Msg])
  final private case class Deliver(output: protocol.Out)
  final private case class Schedule(timer: Timer)

  /** What the transitions ask for. `NeighborUp`/`NeighborDown` and protocol `Send`s are resolved inside the transition. */
  private type Output = OverlayAction | Transmit | Deliver | Schedule

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

  /** The identity of this node, as defined by the overlay. */
  def selfPeer: Peer = overlay.selfPeer

  /** Snapshot of the protocol state. */
  def state: protocol.State = protocolState

  def selfConnectionDescriptors: Set[ConnectionDescriptor] = overlay.selfConnectionDescriptors

  /** The transport connection backing the active edge to `peer`, if any. */
  def connectionTo(peer: Peer): Option[Connection] = overlay.connectionFor(peer)

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

  /** Run a local operation of the protocol. `f` runs under the lock, its effects are performed afterwards.
    * Returns `None` (and does not run `f`) if this is closed.
    */
  def update[R](f: protocol.State => Step[protocol.State, protocol.Msg, protocol.Out]): Unit =
    transition(protocolStep(f))

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
      case OverlayAction.NeighborUp(peer)   => protocolStep(protocol.addPeer(_, peer))
      case OverlayAction.NeighborDown(peer) => protocolStep(protocol.removePeer(_, peer))
      case other                            => List(other)
    }
  }

  private def protocolStep(f: protocol.State => Step[protocol.State, protocol.Msg, protocol.Out]): List[Output] = {
    val result = f(protocolState)
    protocolState = result.state
    result.events.toList.flatMap {
      case Effect.Send(peers, message) =>
        peers.flatMap(peer =>
          overlay.connectionFor(peer).map(Transmit(_, Envelope.Broadcast(overlay.selfPeer, message)))
        )
      case Effect.Deliver(output)   => List(Deliver(output))
      case Effect.StartTimer(timer) => List(Schedule(timer))
    }
  }

  private def handleIncoming(conn: Connection, message: MessageBuffer): List[Output] =
    MessagingIO.decodeMessage[protocol.Msg](message, aead) match
        case Success(Envelope.Membership(overlayMessage)) =>
          overlayStep(OverlayEvent.Received(conn, overlayMessage))
        case Success(Envelope.Broadcast(sender, protocolMessage)) =>
          // messages still arriving on a connection the overlay already closed (e.g. the losing one of two
          // connections to the same peer) would be handled as duplicates and prune the broadcast tree.
          // `mayUse` rejects senders the connection can not carry (impersonation of authenticated peers).
          if overlay.holds(conn) && Peer.mayUse(conn, sender)
          then protocolStep(protocol.handleMessage(_, sender, protocolMessage))
          else Nil
        case Failure(error) =>
          println(s"$selfPeer: error during message decoding")
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
      case _: (OverlayAction.NeighborUp | OverlayAction.NeighborDown) =>
        throw IllegalStateException(s"resolved during the transition: $output")
      case Deliver(output) => deliver(output)
      case Schedule(timer) =>
        timers.schedule(timerDelays(timer))(() => transition(protocolStep(protocol.timerFired(_, timer))))

  private def send(connection: Connection, envelope: Envelope[protocol.Msg]): Unit =
    if !globalAbort.closeRequest then
        sendingActor.execute { () =>
          connection.send(MessagingIO.encodeMessage(envelope, aead)).run {
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
            println(s"$selfPeer: error during message handling")
            error.printStackTrace()
      transition(overlayStep(OverlayEvent.Closed(conn)))
  }

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()
}
