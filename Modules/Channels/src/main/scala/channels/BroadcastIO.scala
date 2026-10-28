package channels

import channels.BroadcastIO.Envelope
import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast.{Event, Peer}
import channels.broadcast.PlumtreeMessage.*
import channels.broadcast.{BroadcastProtocol, PlumtreeBroadcast, PlumtreeMessage}
import channels.connection.{Abort, ByteBufferMessageBuffer, ChannelResolver, Connection, ConnectionClosedException, ConnectionDescriptor, LatentConnection, MessageBuffer, NoMoreDataException, PeerConnectInfo}
import channels.experiments.Aead
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}
import channels.overlay.{AttemptId, ConnId, FullMeshOverlay, OverlayController}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromByteBuffer, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback}
import rdts.base.{ReplicaId, Uid}

import java.net.SocketException
import java.nio.ByteBuffer
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object BroadcastIO {
  enum Envelope[+State] {
    case Membership(message: OverlayMessage)
    case Broadcast(sender: Uid, message: PlumtreeMessage[State])
  }

  def apply[State](
      replicaId: ReplicaId,
      receiveCallback: State => Unit,
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
        broadcast.getOrElse(PlumtreeBroadcast(replicaId.uid, deltaStorage = DiscardingHistory[State](size = 1000))),
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
  * - the overlay ([[OverlayController]]) decides which connections exist and which peers are neighbors
  * - the broadcast protocol ([[BroadcastProtocol]]) decides what to send to which neighbor
  * - this class only translates: transport callbacks become overlay events, actions become transport operations,
  *   timer requests are scheduled on [[Timers]], and the overlay's `NeighborUp`/`NeighborDown` are passed on to the
  *   broadcast protocol. It makes no protocol decisions of its own.
  *
  * All state machine steps run one at a time under `lock`; steps triggered while another step is running
  * (e.g. by synchronous transports or callbacks) are queued and run afterwards.
  */
class BroadcastIO[State](
    val replicaId: ReplicaId,
    receiveCallback: State => Unit,
    @volatile private var overlay: OverlayController,
    resolver: ChannelResolver,
    sendingActor: ExecutionContext,
    val globalAbort: Abort,
    @volatile private var broadcast: BroadcastProtocol[State],
    val aead: Aead,
    val timers: Timers,
    timerDelays: TimerDelays,
)(using val stateCodec: JsonValueCodec[State]) {

  val lock: AnyRef = new {}

  /** Live connections known to the overlay. */
  private var connections: Map[ConnId, Connection] = Map.empty
  private var nextConnId: Long                     = 0L

  /** Outgoing connections whose `Established` was not yet reported; messages received meanwhile are buffered. */
  private var establishing: Map[ConnId, Vector[MessageBuffer]] = Map.empty

  def overlayController: OverlayController = overlay

  def plumtreeState: BroadcastProtocol[State] = broadcast

  def selfConnectionDescriptors: Set[ConnectionDescriptor] = overlay.selfConnectionDescriptors

  /** The transport connection backing the active edge to `peer`, if any. */
  def connectionTo(peer: Uid): Option[Connection] = lock.synchronized {
    overlay.connectionFor(peer).flatMap(connections.get)
  }

  // ------------------------------------------------------------------------------------------------
  // public API
  // ------------------------------------------------------------------------------------------------

  /** Register an outgoing/client transport that yields a usable `Connection` (not dialed by the overlay). */
  def addClientConnection(latentConnection: LatentConnection[Connection]): Unit =
    Async.provided(globalAbort) {
      acceptIncoming(latentConnection).bind
    }.run(printExceptionHandler)

  /** Register an incoming/server transport and learn its published local `ConnectionDescriptor`. */
  def addServerConnection(latentConnection: LatentConnection[ConnectionDescriptor]): Unit =
    Async.provided(globalAbort) {
      val descriptor = acceptIncoming(latentConnection).bind
      stepOverlay(OverlayEvent.SelfDescriptorAdded(descriptor))
    }.run(printExceptionHandler)

  /** Register externally discovered peers with the overlay. */
  def discover(peers: Set[PeerConnectInfo]): Unit = stepOverlay(OverlayEvent.Discovered(peers))

  /** Initiate overlay-native bootstrap through a single descriptor. */
  def bootstrapVia(contact: ConnectionDescriptor): Unit = stepOverlay(OverlayEvent.Bootstrap(contact))

  def allPayloads: List[Payload[State]] = lock.synchronized(broadcast.allPayloads)

  def broadcast(delta: State): Unit = stepBroadcast(_.broadcast(delta))

  // ------------------------------------------------------------------------------------------------
  // serialized state machine steps
  // ------------------------------------------------------------------------------------------------

  private val work    = mutable.Queue.empty[() => Unit]
  private var running = false

  private def serialized(task: => Unit): Unit = lock.synchronized {
    work.enqueue(() => task)
    if !running then
        running = true
        try while work.nonEmpty do work.dequeue()()
        finally running = false
  }

  private def stepOverlay(event: OverlayEvent): Unit = serialized {
    if !globalAbort.closeRequest then
        val (next, actions) = overlay.step(event)
        overlay = next
        actions.foreach(handleOverlayAction)
  }

  private def stepBroadcast(f: BroadcastProtocol[State] => PlumtreeBroadcast.Result[State]): Unit = serialized {
    if !globalAbort.closeRequest then
        val result = f(broadcast)
        broadcast = result.state
        result.events.foreach(handleBroadcastEvent)
  }

  private def handleOverlayAction(action: OverlayAction): Unit = action match
      case OverlayAction.Dial(attempt, to)   => dial(attempt, to)
      case OverlayAction.Send(conn, message) => send(conn, Envelope.Membership(message))
      case OverlayAction.Close(conn)         =>
        val closed = connections.get(conn)
        forget(conn)
        closed.foreach(_.close())
      case OverlayAction.StartTimer(timer) =>
        timers.schedule(timerDelays(timer))(() => stepOverlay(OverlayEvent.TimerFired(timer)))
      case OverlayAction.NeighborUp(peer)   => stepBroadcast(_.addPeer(Peer(peer)))
      case OverlayAction.NeighborDown(peer) => stepBroadcast(_.removePeer(Peer(peer)))

  private def handleBroadcastEvent(event: Event[State]): Unit = event match
      case Event.Deliver(payload)     => receiveCallback(payload.data)
      case Event.Send(peers, message) =>
        peers.foreach(peer =>
          overlay.connectionFor(peer.uid).foreach(send(_, Envelope.Broadcast(replicaId.uid, message)))
        )
      case Event.StartTimer(timer) =>
        timers.schedule(timerDelays(timer))(() => stepBroadcast(_.timerFired(timer)))

  // ------------------------------------------------------------------------------------------------
  // transport
  // ------------------------------------------------------------------------------------------------

  private def register(conn: Connection): ConnId = lock.synchronized {
    val id = ConnId(nextConnId)
    nextConnId += 1
    connections = connections.updated(id, conn)
    id
  }

  private def forget(conn: ConnId): Unit = lock.synchronized {
    connections = connections.removed(conn)
    establishing = establishing.removed(conn)
  }

  private def acceptIncoming[T](latentConnection: LatentConnection[T]): Async[Abort, T] =
    latentConnection.prepare { (conn: Connection) =>
      val id = register(conn)
      stepOverlay(OverlayEvent.Incoming(id))
      messageReceiver(id)
    }

  /** Establish a connection requested by the overlay; reports `Established` or `DialFailed`. */
  private def dial(attempt: AttemptId, to: Set[ConnectionDescriptor]): Unit =
    to.iterator.flatMap(resolver.connect).nextOption() match
        case None                   => stepOverlay(OverlayEvent.DialFailed(attempt))
        case Some(latentConnection) =>
          var registered: Option[ConnId] = None
          Async.provided(globalAbort) {
            val conn = latentConnection.prepare { (conn: Connection) =>
              val id = register(conn)
              lock.synchronized { establishing = establishing.updated(id, Vector.empty) }
              registered = Some(id)
              messageReceiver(id)
            }.bind
            lock.synchronized {
              registered.filter(establishing.contains) match
                  case None     => stepOverlay(OverlayEvent.DialFailed(attempt)) // closed while establishing
                  case Some(id) =>
                    val buffered = establishing(id)
                    establishing = establishing.removed(id)
                    // the connection returned by `prepare` is the one to send on
                    connections = connections.updated(id, conn)
                    stepOverlay(OverlayEvent.Established(attempt, id))
                    buffered.foreach(message => serialized(handleIncoming(id, message)))
            }
          }.run {
            case Success(_)     => ()
            case Failure(error) =>
              registered.foreach(forget)
              stepOverlay(OverlayEvent.DialFailed(attempt))
          }

  /** Receive, decode, and dispatch messages; report the end of the connection to the overlay. */
  private def messageReceiver(conn: ConnId): Callback[MessageBuffer] = {
    case Success(message) =>
      lock.synchronized {
        establishing.get(conn) match
            case Some(buffered) => establishing = establishing.updated(conn, buffered :+ message)
            case None           => serialized(handleIncoming(conn, message))
      }
    case Failure(error) =>
      error match
          case se: SocketException if se.getMessage == "Connection reset" =>
          case _: (NoMoreDataException | ConnectionClosedException)       =>
          case _                                                          =>
            println(s"$replicaId: error during message handling")
            error.printStackTrace()
      transportClosed(conn)
  }

  private def transportClosed(conn: ConnId): Unit = lock.synchronized {
    if connections.contains(conn) then
        forget(conn)
        stepOverlay(OverlayEvent.Closed(conn))
  }

  private def handleIncoming(conn: ConnId, message: MessageBuffer): Unit =
    if connections.contains(conn) then
        BroadcastIO.decodeEnvelope(message, aead) match
            case Success(Envelope.Membership(overlayMessage)) =>
              stepOverlay(OverlayEvent.Received(conn, overlayMessage))
            case Success(Envelope.Broadcast(sender, broadcastMessage)) =>
              stepBroadcast(_.handleMessage(Peer(sender), broadcastMessage))
            case Failure(error) =>
              println(s"$replicaId: error during message decoding")
              error.printStackTrace()
              connections.get(conn).foreach(_.close())
              transportClosed(conn)

  private def send(conn: ConnId, payload: Envelope[State]): Unit =
    if !globalAbort.closeRequest then
        connections.get(conn).foreach { connection =>
          sendingActor.execute { () =>
            connection.send(BroadcastIO.encodeEnvelope(payload, aead)).run {
              case Success(_) => ()
              case Failure(_) => transportClosed(conn)
            }
          }
        }

  private def printExceptionHandler: Callback[Any] =
      case Failure(ex) =>
        println("exception during connection activation")
        ex.printStackTrace()
      case Success(_) => ()
}
