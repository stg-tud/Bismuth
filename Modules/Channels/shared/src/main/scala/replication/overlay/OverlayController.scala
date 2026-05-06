package replication.overlay

import channels.{ChannelConnectInfo, Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

trait OverlayController {

  /** Handle one overlay control-plane message and return the next controller state plus side-effect actions.
    * If a concrete connection is supplied, the controller may learn which peer sent the message and attach that
    * opaque connection handle to the corresponding active-view entry.
    */
  def receiveActions(message: OverlayMessage, from: Connection): (OverlayController, List[OverlayAction]) =
    (this, Nil)

  /** Register a newly established connection before the remote peer identity is known. */
  def registerConnection(conn: Connection): (OverlayController, List[OverlayAction]) = (this, Nil)

  /** Remove a connection previously registered with the controller and return resulting actions. */
  def removeConnection(conn: Connection): (OverlayController, List[OverlayAction]) = (this, Nil)

  /** Lookup the currently known connection for a peer, if one is attached to an active-view entry. */
  def connectionFor(peer: Uid): Option[Connection] = None

  /** Reverse lookup from connection object to peer identity, if known. */
  def peerForConnection(conn: Connection): Option[Uid] = None

  /** Overlay lifecycle tick: periodic maintenance (promotion, shuffle, etc.). */
  def tick(): (OverlayController, List[OverlayAction]) = (this, Nil)

  /** Learn externally discovered peers. */
  def discoverPeers(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = (this, Nil)
}

object OverlayController {

  enum OverlayMessage {
    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Uid)
    case Neighbor(from: PeerConnectInfo, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(origin: PeerConnectInfo, sample: Set[PeerConnectInfo], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[PeerConnectInfo])
    case Ping(time: Long)
    case Pong(time: Long)

    def getSender: Option[Uid] = this match
        case Join(newNode)             => Some(newNode.uid)
        case ForwardJoin(_, _, sender) => Some(sender)
        case Neighbor(from, _)         => Some(from.uid)
        case NeighborReply(from, _)    => Some(from)
        case Disconnect(peer)          => Some(peer)
        case Shuffle(_, _, _, sender)  => Some(sender)
        case ShuffleReply(from, _)     => Some(from)
        case Ping(_) | Pong(_)         => None
  }

  enum OverlayAction {

    /** Send a control-plane message over an already active connection.
      * The state machine only emits this action when a connection is known.
      */
    case Send(connection: Connection, message: OverlayMessage)

    /** Bootstrap-only send where no peer identity is known yet, only raw connection details. */
    case SendJoin(to: Set[ChannelConnectInfo], message: OverlayMessage)

    /** Notify the caller that a peer now has an active attached connection. */
    case ActiveConnectionAdded(peer: Uid)

    /** Notify the caller that a peer lost its active attached connection. */
    case ActiveConnectionRemoved(peer: Uid)
  }
}
