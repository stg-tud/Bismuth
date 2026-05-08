package replication.overlay

import channels.{ChannelConnectInfo, Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

trait OverlayController {

  /** Called when a raw connection becomes available and should be activated for this overlay.
    * HyParView does nothing here because it identifies peers from received protocol messages.
    * Simpler overlays may use this to emit an initial handshake.
    */
  def activateConnection(conn: Connection, connectInfo: Option[ChannelConnectInfo]): (OverlayController, List[OverlayAction]) =
    (this, Nil)

  /** Handle one overlay control-plane message and return the next controller state plus side-effect actions.
    * If a concrete connection is supplied, the controller may learn which peer sent the message and attach that
    * opaque connection handle to the corresponding active-view entry.
    */
  def receiveActions(message: OverlayMessage, conn: Connection): (OverlayController, List[OverlayAction]) =
    (this, Nil)

  /** Remove a connection and optionally provide the connect info used to establish it, if known.
    * This lets overlays clean up pending outbound attempts without tracking raw connection objects internally.
    */
  def removeConnection(conn: Connection, connectInfo: Option[ChannelConnectInfo] = None): (OverlayController, List[OverlayAction]) =
    (this, Nil)

  /** Lookup the currently known connection for a peer, if one is attached to an active-view entry. */
  def connectionFor(peer: Uid): Option[Connection] = None

  /** Overlay lifecycle tick: periodic maintenance (promotion, shuffle, etc.). */
  def tick(): (OverlayController, List[OverlayAction]) = (this, Nil)

  /** Learn externally discovered peers. */
  def discoverPassive(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = (this, Nil)

  /** Initiate an overlay-native join/bootstrap through a contact peer. */
  def join(contact: PeerConnectInfo): (OverlayController, List[OverlayAction]) = (this, Nil)
}

object OverlayController {

  enum OverlayMessage {
    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Uid)
    case Neighbor(from: PeerConnectInfo, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Shuffle(origin: PeerConnectInfo, sample: Set[PeerConnectInfo], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[PeerConnectInfo])
  }

  enum OverlayAction {

    /** Send a control-plane message over an already active connection.
      * The state machine only emits this action when a connection is known.
      */
    case Send(connection: Connection, message: OverlayMessage)

    /** Bootstrap-only send where no peer identity is known yet, only raw connection details. */
    case SendJoin(to: Set[ChannelConnectInfo], expectedPeer: Uid, message: OverlayMessage)

    /** Close a live transport connection; the resulting closure must be fed back through `removeConnection`. */
    case Disconnect(connection: Connection)

    /** Notify the caller that a peer now has an active attached connection. */
    case ActiveConnectionAdded(peer: Uid)

    /** Notify the caller that a peer lost its active attached connection. */
    case ActiveConnectionRemoved(peer: Uid)
  }
}
