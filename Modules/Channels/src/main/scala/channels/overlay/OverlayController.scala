package channels.overlay

import channels.connection.{ConnectionDescriptor, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent}
import rdts.base.Uid

/** Identifies one live transport connection. Assigned by the driver (e.g. [[channels.BroadcastIO]]). */
final case class ConnId(value: Long)

/** Identifies one outgoing connection attempt. Chosen by the overlay when it emits [[OverlayAction.Dial]]. */
final case class AttemptId(value: Long)

/** Immutable overlay state machine.
  *
  * The overlay is driven exclusively by [[OverlayEvent]]s and responds with [[OverlayAction]]s.
  * It never observes time, transports, or connection objects directly: connections are referred to by
  * [[ConnId]]s that the driver assigns, and outgoing attempts by [[AttemptId]]s the overlay assigns.
  * Every connection reported to the overlay (`Incoming`, `Established`) is owned by it until it emits `Close`
  * or the driver reports `Closed`.
  */
trait OverlayController {

  def step(event: OverlayEvent): (OverlayController, List[OverlayAction])

  /** Peers with an established active edge (what the broadcast layer sees as neighbors). */
  def neighbors: Set[Uid]

  /** The connection backing the active edge to `peer`, if any. */
  def connectionFor(peer: Uid): Option[ConnId]

  /** Currently advertised self connection descriptors. */
  def selfConnectionDescriptors: Set[ConnectionDescriptor]
}

object OverlayController {

  enum OverlayEvent {

    /** The application asks to join the overlay through the given contact. */
    case Bootstrap(contact: ConnectionDescriptor)

    /** Peers learned out of band. */
    case Discovered(peers: Set[PeerConnectInfo])

    /** The local node is reachable under an additional descriptor. */
    case SelfDescriptorAdded(descriptor: ConnectionDescriptor)

    /** A remote node connected to us (or a connection was handed to us without a dial). */
    case Incoming(conn: ConnId)

    /** The connection requested by `Dial(attempt, _)` was established. */
    case Established(attempt: AttemptId, conn: ConnId)

    /** The connection requested by `Dial(attempt, _)` could not be established. */
    case DialFailed(attempt: AttemptId)

    case Received(conn: ConnId, message: OverlayMessage)

    /** The connection ended for a reason other than our own `Close` (remote close, transport error). */
    case Closed(conn: ConnId)

    /** A timer requested by `StartTimer` expired. Stale timers are expected and must be ignored. */
    case TimerFired(timer: OverlayTimer)
  }

  /** Timers carry only their purpose; the driver decides what delay each purpose maps to. */
  enum OverlayTimer {

    /** Periodic passive-view maintenance. */
    case Shuffle

    /** A connection that has not completed its handshake by now is given up. */
    case Handshake(conn: ConnId)
  }

  enum OverlayMessage {
    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Uid)
    case Neighbor(from: PeerConnectInfo, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Shuffle(origin: PeerConnectInfo, sample: Set[PeerConnectInfo], ttl: Int, sender: Uid)

    /** `answeredSample` echoes the sample of the answered [[Shuffle]] (empty if the reply answers no shuffle),
      * so the receiver can prefer evicting the peers it sent without keeping per-shuffle state.
      */
    case ShuffleReply(from: Uid, sample: Set[PeerConnectInfo], answeredSample: Set[PeerConnectInfo])
  }

  enum OverlayAction {

    /** Establish a connection to one of the descriptors; answered by `Established` or `DialFailed`. */
    case Dial(attempt: AttemptId, to: Set[ConnectionDescriptor])

    case Send(conn: ConnId, message: OverlayMessage)

    /** Close the connection. The overlay has already forgotten it; no `Closed` event is expected. */
    case Close(conn: ConnId)

    case StartTimer(timer: OverlayTimer)

    /** `peer` became a neighbor (an active edge was established). */
    case NeighborUp(peer: Uid)

    /** `peer` is no longer a neighbor. */
    case NeighborDown(peer: Uid)
  }
}
