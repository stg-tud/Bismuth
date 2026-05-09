package replication.overlay

import channels.{ConnectionDescriptor, Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

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

  private def rememberActivePeer(peer: PeerConnectInfo, conn: Connection): (FullMeshOverlay, List[OverlayAction]) = {
    val remembered = rememberPeer(peer)
    val previous   = remembered.active.get(peer.uid)
    val next       = remembered.copy(active = remembered.active.updated(peer.uid, conn))
    val actions = previous match
        case None                           => List(OverlayAction.ActiveConnectionAdded(peer.uid))
        case Some(existing) if existing == conn => Nil
        case Some(existing)                 => List(OverlayAction.Disconnect(existing))
    (next, actions)
  }

  override def addSelfConnectionDescriptor(descriptor: ConnectionDescriptor): OverlayController =
    copy(self = self.copy(channelConnectors = self.channelConnectors + descriptor))

  override def selfConnectionDescriptors: Set[ConnectionDescriptor] = self.channelConnectors

  override def discoverPassive(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val actions = peers.iterator.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.SendJoin(peer.channelConnectors, peer.uid, OverlayMessage.Neighbor(self, highPriority = true))
    }.toList
    (this, actions)
  }

  override def activateConnection(
      conn: Connection,
      connectInfo: Option[ConnectionDescriptor]
  ): (OverlayController, List[OverlayAction]) =
    if connectInfo.nonEmpty then (this, Nil)
    else (this, List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true))))

  override def receiveActions(
      message: OverlayMessage,
      conn: Connection
  ): (OverlayController, List[OverlayAction]) = {
    message match
        case OverlayMessage.Join(peer) =>
          val (next, peerActions) = rememberActivePeer(peer, conn)
          val bootstrapPeers      = next.known.valuesIterator.toSet + next.self
          (next, peerActions :+ OverlayAction.Send(conn, OverlayMessage.ShuffleReply(self.uid, bootstrapPeers)))

        case OverlayMessage.Neighbor(peer, _) =>
          rememberActivePeer(peer, conn)

        case OverlayMessage.ShuffleReply(_, peers) =>
          discoverPassive(peers)

        case _ =>
          (this, Nil)
  }

  override def removeConnection(
      conn: Connection,
      connectInfo: Option[channels.ConnectionDescriptor] = None
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
    (
      this,
      List(OverlayAction.SendJoin(Set(contact), self.uid, OverlayMessage.Join(self)))
    )
}
