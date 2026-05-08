package replication.overlay

import channels.{ChannelConnectInfo, Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

case class DirectConnectionOverlay(
    self: PeerConnectInfo,
    active: Map[Uid, Connection] = Map.empty,
) extends OverlayController {

  override def discoverPassive(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val actions = peers.iterator.collect {
      case peer if peer.uid != self.uid && !active.contains(peer.uid) =>
        OverlayAction.SendJoin(peer.channelConnectors, peer.uid, OverlayMessage.Neighbor(self, highPriority = true))
    }.toList
    (this, actions)
  }

  override def activateConnection(
      conn: Connection,
      connectInfo: Option[ChannelConnectInfo]
  ): (OverlayController, List[OverlayAction]) =
    if connectInfo.nonEmpty then (this, Nil)
    else (this, List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true))))

  override def receiveActions(
      message: OverlayMessage,
      conn: Connection
  ): (OverlayController, List[OverlayAction]) = {
    message match
        case OverlayMessage.Neighbor(peer, _) =>
          val wasKnown = active.contains(peer.uid)
          val next     = copy(active = active.updated(peer.uid, conn))
          (next, Option.when(!wasKnown)(OverlayAction.ActiveConnectionAdded(peer.uid)).toList)
        case _ =>
          (this, Nil)
  }

  override def removeConnection(
      conn: Connection,
      connectInfo: Option[channels.ChannelConnectInfo] = None
  ): (OverlayController, List[OverlayAction]) =
    active.find(_._2 == conn) match
        case None            => (this, Nil)
        case Some((peer, _)) =>
          (
            copy(active = active.removed(peer)),
            List(OverlayAction.ActiveConnectionRemoved(peer))
          )

  override def connectionFor(peer: Uid): Option[Connection] = active.get(peer)

  override def join(contact: PeerConnectInfo): (OverlayController, List[OverlayAction]) =
    discoverPassive(Set(contact))

}
