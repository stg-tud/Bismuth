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

  override def registerConnection(conn: Connection, expectedPeer: Option[Uid] = None): (OverlayController, List[OverlayAction]) =
    if unknownConnections.contains(conn) || active.values.exists(_ == conn) then (this, Nil)
    else
        (
          copy(unknownConnections = unknownConnections + conn),
          List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true)))
        )

  override def discoverPeers(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val next    = copy(known = known ++ peers.iterator.filterNot(_.uid == self.uid).map(p => p.uid -> p))
    val actions = peers.toList.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.SendJoin(peer.channelConnectors, peer.uid, OverlayMessage.Neighbor(self, highPriority = true))
    }
    (next, actions)
  }

  override def receiveActions(message: OverlayMessage, from: Connection): (OverlayController, List[OverlayAction]) = {
    message match
        case OverlayMessage.Neighbor(peer, _) =>
          val wasKnown = active.contains(peer.uid)
          val next     = copy(
            known = known.updated(peer.uid, peer),
            active = active.updated(peer.uid, from),
            identifiedConnections = identifiedConnections.updated(from, peer.uid),
            unknownConnections = unknownConnections - from
          )
          // NOTE:
          // This overlay currently promotes the sender to `active` as soon as a Neighbor request is received,
          // before we know that the sender has processed our NeighborReply and installed us as active too.
          // That allows the local BroadcastIO to emit `ActiveConnectionAdded`, which causes Plumtree to send an
          // immediate `Graft`. The remote side may receive that `Graft` before its own overlay/plumtree activation
          // has completed. BroadcastIO therefore contains a temporary fallback for early protocol messages.
          // A cleaner fix would make activation a true two-sided handshake.
          val actions = List(OverlayAction.Send(from, OverlayMessage.NeighborReply(self.uid, accepted = true))) :::
            Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer.uid)).toList
          (next, actions)

        case OverlayMessage.NeighborReply(peer, accepted) if accepted =>
          val wasKnown = active.contains(peer)
          val next     = copy(
            active = active.updated(peer, from),
            identifiedConnections = identifiedConnections.updated(from, peer),
            unknownConnections = unknownConnections - from
          )
          (next, Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer)).toList)

        case OverlayMessage.Ping(time) =>
          (this, List(OverlayAction.Send(from, OverlayMessage.Pong(time))))

        case OverlayMessage.Pong(_) =>
          (this, Nil)

        case _ =>
          (this, Nil)
  }

  override def removeConnection(conn: Connection): (OverlayController, List[OverlayAction]) =
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
