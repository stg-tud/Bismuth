package replication.overlay

import channels.{Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

case class DirectConnectionOverlay(
    self: PeerConnectInfo,
    known: Map[Uid, PeerConnectInfo] = Map.empty,
    active: Map[Uid, Connection] = Map.empty,
    identifiedConnections: Map[Connection, Uid] = Map.empty,
    unknownConnections: Set[Connection] = Set.empty,
) extends OverlayController {

  override def discoverPeers(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val next    = copy(known = known ++ peers.iterator.filterNot(_.uid == self.uid).map(p => p.uid -> p))
    val actions = peers.toList.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.SendJoin(peer.channelConnectors, peer.uid, OverlayMessage.Neighbor(self, highPriority = true))
    }
    (next, actions)
  }

  override def receiveActions(
      message: OverlayMessage,
      conn: Connection
  ): (OverlayController, List[OverlayAction]) = {
    message match
        case OverlayMessage.Neighbor(peer, _) =>
          val wasKnown = active.contains(peer.uid)
          val next     = copy(
            known = known.updated(peer.uid, peer),
            active = active.updated(peer.uid, conn),
            identifiedConnections = identifiedConnections.updated(conn, peer.uid),
            unknownConnections = unknownConnections - conn
          )
          // NOTE:
          // This overlay currently promotes the sender to `active` as soon as a Neighbor request is received,
          // before we know that the sender has processed our NeighborReply and installed us as active too.
          // That allows the local BroadcastIO to emit `ActiveConnectionAdded`, which causes Plumtree to send an
          // immediate `Graft`. The remote side may receive that `Graft` before its own overlay/plumtree activation
          // has completed. BroadcastIO therefore contains a temporary fallback for early protocol messages.
          // A cleaner fix would make activation a true two-sided handshake.
          val actions = List(OverlayAction.Send(conn, OverlayMessage.NeighborReply(self.uid, accepted = true))) :::
            Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer.uid)).toList
          (next, actions)

        case OverlayMessage.NeighborReply(peer, accepted) if accepted =>
          val wasKnown = active.contains(peer)
          val next     = copy(
            active = active.updated(peer, conn),
            identifiedConnections = identifiedConnections.updated(conn, peer),
            unknownConnections = unknownConnections - conn
          )
          (next, Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer)).toList)

        case _ =>
          (this, Nil)
  }

  override def removeConnection(
      conn: Connection,
      connectInfo: Option[channels.ChannelConnectInfo] = None
  ): (OverlayController, List[OverlayAction]) =
    active.find(_._2 == conn) match
        case Some((peer, _)) =>
          (
            copy(
              active = active.removed(peer),
              identifiedConnections = identifiedConnections.removed(conn),
              unknownConnections = unknownConnections - conn
            ),
            List(OverlayAction.ActiveConnectionRemoved(peer))
          )
        case None =>
          (
            copy(
              identifiedConnections = identifiedConnections.removed(conn),
              unknownConnections = unknownConnections - conn
            ),
            Nil
          )

  override def connectionFor(peer: Uid): Option[Connection] = active.get(peer)

  override def peerForConnection(conn: Connection): Option[Uid] =
    active.collectFirst { case (peer, c) if c == conn => peer }
      .orElse(identifiedConnections.get(conn))
}
