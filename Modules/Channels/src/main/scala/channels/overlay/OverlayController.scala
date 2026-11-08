package channels.overlay

import channels.connection.{Connection, ConnectionDescriptor, Peer, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent}

/** Identifies one outgoing connection attempt. Chosen by the overlay when it emits [[OverlayController.OverlayAction.Dial]]. */
final case class AttemptId(value: Long)

/** Immutable overlay state machine.
  *
  * The overlay is driven exclusively by [[OverlayController.OverlayEvent]]s and responds with [[OverlayController.OverlayAction]]s.
  * It never observes time or transports directly: connections are only used as identities (compared by reference),
  * the overlay never sends on or closes them itself. Outgoing attempts are referred to by [[AttemptId]]s the overlay assigns.
  * Every connection reported to the overlay (`Incoming`, `Established`) is owned by it until it emits `Close`
  * or the driver reports `Closed`.
  */
trait OverlayController {

  def step(event: OverlayEvent): (OverlayController, List[OverlayAction])

  /** Peers with an established active edge (what the broadcast layer sees as neighbors). */
  def neighbors: Set[Peer]

  /** The connection backing the active edge to `peer`, if any. */
  def connectionFor(peer: Peer): Option[Connection]

  /** The identity of the local node, used as the sender of its messages. */
  def selfPeer: Peer

  /** Whether `conn` is still owned by the overlay (pending or active), i.e. it was reported and not yet closed. */
  def holds(conn: Connection): Boolean

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
    case Incoming(conn: Connection)

    /** The connection requested by `Dial(attempt, _)` was established. */
    case Established(attempt: AttemptId, conn: Connection)

    /** The connection requested by `Dial(attempt, _)` could not be established. */
    case DialFailed(attempt: AttemptId)

    case Received(conn: Connection, message: OverlayMessage)

    /** The connection ended for a reason other than our own `Close` (remote close, transport error). */
    case Closed(conn: Connection)

    /** A timer requested by `StartTimer` expired. Stale timers are expected and must be ignored. */
    case TimerFired(timer: OverlayTimer)
  }

  /** Timers carry only their purpose; the driver decides what delay each purpose maps to. */
  enum OverlayTimer {

    /** Periodic passive-view maintenance. */
    case Shuffle

    /** A connection that has not completed its handshake by now is given up. */
    case Handshake(conn: Connection)
  }

  /** Whether `message` may have been sent by the peer at the other end of `conn`, see [[Peer.mayUse]].
    * Overlays discard messages that fail this check.
    */
  def isAuthentic(conn: Connection, message: OverlayMessage): Boolean =
    Peer.mayUse(conn, message.claimedSender)

  enum OverlayMessage {

    /** The peer that sent this message (on the connection it arrived on), as claimed by the message. */
    def claimedSender: Peer = this match
        case Join(newNode)                  => newNode.id
        case ForwardJoin(_, _, sender)      => sender
        case Neighbor(from, _)              => from.id
        case NeighborReply(from, _)         => from
        case Shuffle(_, _, _, sender)       => sender
        case ShuffleReply(from, _, _)       => from

    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Peer)
    case Neighbor(from: PeerConnectInfo, highPriority: Boolean)
    case NeighborReply(from: Peer, accepted: Boolean)
    case Shuffle(origin: PeerConnectInfo, sample: Set[PeerConnectInfo], ttl: Int, sender: Peer)

    /** `answeredSample` echoes the sample of the answered [[Shuffle]] (empty if the reply answers no shuffle),
      * so the receiver can prefer evicting the peers it sent without keeping per-shuffle state.
      */
    case ShuffleReply(from: Peer, sample: Set[PeerConnectInfo], answeredSample: Set[PeerConnectInfo])
  }

  enum OverlayAction {

    /** Establish a connection to one of the descriptors; answered by `Established` or `DialFailed`. */
    case Dial(attempt: AttemptId, to: Set[ConnectionDescriptor])

    case Send(conn: Connection, message: OverlayMessage)

    /** Close the connection. The overlay has already forgotten it; no `Closed` event is expected. */
    case Close(conn: Connection)

    case StartTimer(timer: OverlayTimer)

    /** `peer` became a neighbor (an active edge was established). */
    case NeighborUp(peer: Peer)

    /** `peer` is no longer a neighbor. */
    case NeighborDown(peer: Peer)
  }
}
