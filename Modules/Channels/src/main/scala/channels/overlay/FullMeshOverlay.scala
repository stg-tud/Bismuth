package channels.overlay

import channels.connection.{Connection, ConnectionDescriptor, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayMessage}
import rdts.base.Uid

case class FullMeshOverlay(
    self: PeerConnectInfo,
    active: Map[Uid, Connection] = Map.empty,
    known: Map[Uid, PeerConnectInfo] = Map.empty,
) extends OverlayController {

  private def rememberPeer(peer: PeerConnectInfo): FullMeshOverlay =
    if peer.uid == self.uid then copy(known = known.updated(self.uid, self))
    else {
      val merged = known.get(peer.uid) match
          case Some(existing) => existing.copy(channelConnectors = existing.channelConnectors ++ peer.channelConnectors)
          case None           => peer
      copy(known = known.updated(peer.uid, merged))
    }

  private def rememberAndActivatePeer(
      peer: PeerConnectInfo,
      conn: Connection
  ): (FullMeshOverlay, List[OverlayAction]) = {
    val remembered = rememberPeer(peer)
    val previous   = remembered.active.get(peer.uid)
    val next       = remembered.copy(active = remembered.active.updated(peer.uid, conn))
    val actions    = previous match
        case None                               => List(OverlayAction.ActiveConnectionAdded(peer.uid))
        case Some(existing) if existing == conn => Nil
        case Some(existing)                     => List(OverlayAction.Disconnect(existing))
    (next, actions)
  }

  override def addSelfConnectionDescriptor(descriptor: ConnectionDescriptor): OverlayController =
    copy(self = self.copy(channelConnectors = self.channelConnectors + descriptor))

  override def selfConnectionDescriptors: Set[ConnectionDescriptor] = self.channelConnectors

  override def discoverPassive(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val actions = peers.iterator.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.Connect(peer.channelConnectors, peer.uid, None)
    }.toList
    (this, actions)
  }

  /** The full mesh announces its own peer id as soon as a connection is established.
    * This is the only handshake mechanism: the remote side learns us from the received Neighbor.
    */
  override def activateConnection(
      conn: Connection,
      connectInfo: Option[ConnectionDescriptor]
  ): (OverlayController, List[OverlayAction]) =
    (this, List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true))))

  override def receiveActions(
      message: OverlayMessage,
      conn: Connection
  ): (OverlayController, List[OverlayAction]) =
    message match
        case OverlayMessage.Join(peer) =>
          val bootstrapPeers      = known.valuesIterator.toSet + self
          (this, List(OverlayAction.Send(conn, OverlayMessage.ShuffleReply(self.uid, bootstrapPeers))))

        case OverlayMessage.Neighbor(peer, _) =>
          rememberAndActivatePeer(peer, conn)

        case OverlayMessage.ShuffleReply(_, peers) =>
          discoverPassive(peers)

        case _ =>
          (this, Nil)

  override def removeConnection(
      conn: Connection,
      connectInfo: Option[ConnectionDescriptor] = None
  ): (OverlayController, List[OverlayAction]) =
    active.find(_._2 == conn) match
        case None            => (this, Nil)
        case Some((peer, _)) =>
          (
            copy(active = active.removed(peer), known = known.removed(peer)),
            List(OverlayAction.ActiveConnectionRemoved(peer))
          )

  override def connectionFor(peer: Uid): Option[Connection] = active.get(peer)

  override def bootstrapVia(contact: ConnectionDescriptor): (OverlayController, List[OverlayAction]) =
    (this, List(OverlayAction.Connect(Set(contact), self.uid, Some(OverlayMessage.Join(self)))))
}
