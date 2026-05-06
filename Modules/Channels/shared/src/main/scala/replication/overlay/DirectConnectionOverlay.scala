package replication.overlay

import channels.{Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

case class DirectConnectionOverlay(
    self: PeerConnectInfo,
    known: Map[Uid, PeerConnectInfo] = Map.empty,
    active: Map[Uid, Connection] = Map.empty,
    unknownConnections: Set[Connection] = Set.empty,
) extends OverlayController {

  override def registerConnection(conn: Connection): (OverlayController, List[OverlayAction]) =
    if unknownConnections.contains(conn) || active.values.exists(_ == conn) then (this, Nil)
    else
        (
          copy(unknownConnections = unknownConnections + conn),
          List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true)))
        )

  override def discoverPeers(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val next = copy(known = known ++ peers.iterator.filterNot(_.uid == self.uid).map(p => p.uid -> p))
    val actions = peers.toList.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.SendJoin(peer.channelConnectors, OverlayMessage.Neighbor(self, highPriority = true))
    }
    (next, actions)
  }

  override def receiveActions(message: OverlayMessage, from: Connection): (OverlayController, List[OverlayAction]) =
    message match
        case OverlayMessage.Neighbor(peer, _) =>
          val wasKnown = active.contains(peer.uid)
          val next     = copy(
            known = known.updated(peer.uid, peer),
            active = active.updated(peer.uid, from),
            unknownConnections = unknownConnections - from
          )
          val added = Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer.uid)).toList
          (next, added :+ OverlayAction.Send(from, OverlayMessage.NeighborReply(self.uid, accepted = true)))

        case OverlayMessage.NeighborReply(peer, accepted) if accepted =>
          val wasKnown = active.contains(peer)
          val next     = copy(
            active = active.updated(peer, from),
            unknownConnections = unknownConnections - from
          )
          (next, Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer)).toList)

        case OverlayMessage.Disconnect(peer) =>
          active.get(peer) match
              case Some(_) =>
                (copy(active = active.removed(peer)), List(OverlayAction.ActiveConnectionRemoved(peer)))
              case None =>
                (this, Nil)

        case OverlayMessage.Ping(time) =>
          (this, List(OverlayAction.Send(from, OverlayMessage.Pong(time))))

        case OverlayMessage.Pong(_) =>
          (this, Nil)

        case _ =>
          (this, Nil)

  override def removeConnection(conn: Connection): (OverlayController, List[OverlayAction]) =
    active.find(_._2 == conn) match
        case Some((peer, _)) =>
          (
            copy(active = active.removed(peer), unknownConnections = unknownConnections - conn),
            List(OverlayAction.ActiveConnectionRemoved(peer))
          )
        case None =>
          (copy(unknownConnections = unknownConnections - conn), Nil)

  override def connectionFor(peer: Uid): Option[Connection] = active.get(peer)

  override def peerForConnection(conn: Connection): Option[Uid] =
    active.collectFirst { case (peer, c) if c == conn => peer }
}
