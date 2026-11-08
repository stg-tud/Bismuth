package channels.overlay

import channels.connection.{Connection, ConnectionDescriptor, Peer, PeerConnectInfo}
import channels.overlay.FullMeshOverlay.Purpose
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}

object FullMeshOverlay {

  /** Why an outgoing connection is dialed. */
  enum Purpose {

    /** Bootstrap: additionally send `Join`, so the contact replies with all peers it knows. */
    case Join

    /** Connect to a known peer. */
    case Connect(peer: Peer)
  }
}

/** Connects to every peer it learns about.
  *
  * Both ends of every connection announce themselves with a `Neighbor`; a connection becomes the active edge to
  * the announced peer. If there are two connections to the same peer (e.g. both sides dialed each other at the
  * same time), both sides keep the one dialed by the peer with the smaller id and close the other.
  */
case class FullMeshOverlay(
    self: PeerConnectInfo,
    active: Map[Peer, Connection] = Map.empty,
    known: Map[Peer, PeerConnectInfo] = Map.empty,
    /** Connections that have not yet announced their peer. */
    pending: Set[Connection] = Set.empty,
    /** Connections we dialed (as opposed to incoming ones). */
    outgoing: Set[Connection] = Set.empty,
    dialing: Map[AttemptId, Purpose] = Map.empty,
    nextAttempt: Long = 0L,
) extends OverlayController {

  override def neighbors: Set[Peer] = active.keySet

  override def connectionFor(peer: Peer): Option[Connection] = active.get(peer)

  override def selfPeer: Peer = self.id

  override def holds(conn: Connection): Boolean = pending.contains(conn) || active.valuesIterator.contains(conn)

  override def selfConnectionDescriptors: Set[ConnectionDescriptor] = self.channelConnectors

  override def step(event: OverlayEvent): (OverlayController, List[OverlayAction]) = event match
      case OverlayEvent.Bootstrap(contact) =>
        dial(Purpose.Join, Set(contact))

      case OverlayEvent.Discovered(peers) =>
        connectTo(peers)

      case OverlayEvent.SelfDescriptorAdded(descriptor) =>
        (copy(self = self.copy(channelConnectors = self.channelConnectors + descriptor)), Nil)

      case OverlayEvent.Incoming(conn) =>
        (copy(pending = pending + conn), List(announce(conn)))

      case OverlayEvent.Established(attempt, conn) =>
        val base = copy(dialing = dialing.removed(attempt), pending = pending + conn, outgoing = outgoing + conn)
        dialing.get(attempt) match
            case None               => (copy(dialing = dialing.removed(attempt)), List(OverlayAction.Close(conn)))
            case Some(Purpose.Join) => (base, List(announce(conn), OverlayAction.Send(conn, OverlayMessage.Join(self))))
            case Some(Purpose.Connect(_)) => (base, List(announce(conn)))

      case OverlayEvent.DialFailed(attempt) =>
        (copy(dialing = dialing.removed(attempt)), Nil)

      case OverlayEvent.Closed(conn) =>
        active.find(_._2 == conn) match
            case None            => (copy(pending = pending - conn, outgoing = outgoing - conn), Nil)
            case Some((peer, _)) =>
              (
                copy(
                  active = active.removed(peer),
                  known = known.removed(peer),
                  pending = pending - conn,
                  outgoing = outgoing - conn
                ),
                List(OverlayAction.NeighborDown(peer))
              )

      case OverlayEvent.TimerFired(_) =>
        (this, Nil)

      case OverlayEvent.Received(conn, message) =>
        if !holds(conn) || !OverlayController.isAuthentic(conn, message) then (this, Nil)
        else
            message match
                case OverlayMessage.Join(_) =>
                  val bootstrapPeers = known.valuesIterator.toSet + self
                  (
                    this,
                    List(OverlayAction.Send(conn, OverlayMessage.ShuffleReply(self.id, bootstrapPeers, Set.empty)))
                  )

                case OverlayMessage.Neighbor(peer, _) =>
                  activate(peer, conn)

                case OverlayMessage.ShuffleReply(_, peers, _) =>
                  connectTo(peers)

                case _ =>
                  (this, Nil)

  private def announce(conn: Connection) = OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true))

  private def dial(purpose: Purpose, to: Set[ConnectionDescriptor]): (FullMeshOverlay, List[OverlayAction]) = {
    val attempt = AttemptId(nextAttempt)
    (
      copy(nextAttempt = nextAttempt + 1, dialing = dialing.updated(attempt, purpose)),
      List(OverlayAction.Dial(attempt, to))
    )
  }

  private def connectTo(peers: Set[PeerConnectInfo]): (FullMeshOverlay, List[OverlayAction]) =
    peers.foldLeft((this, List.empty[OverlayAction])) { case ((state, actions), peer) =>
      val skip = peer.id == self.id || state.active.contains(peer.id) ||
        state.dialing.valuesIterator.contains(Purpose.Connect(peer.id))
      if skip then (state, actions)
      else {
        // the peer is only remembered once it announces itself on the connection
        val (next, more) = state.dial(Purpose.Connect(peer.id), peer.channelConnectors)
        (next, actions ++ more)
      }
    }

  private def activate(peer: PeerConnectInfo, conn: Connection): (FullMeshOverlay, List[OverlayAction]) = {
    val remembered         = rememberPeer(peer).copy(pending = pending - conn)
    def use(c: Connection) = remembered.copy(active = remembered.active.updated(peer.id, c))
    active.get(peer.id) match
        case None                               => (use(conn), List(OverlayAction.NeighborUp(peer.id)))
        case Some(existing) if existing == conn => (remembered, Nil)
        case Some(existing)                     =>
          // keep the connection dialed by the smaller id; both sides come to the same conclusion
          val preferOutgoing = Ordering[Peer].lt(self.id, peer.id)
          val keepNew        =
            outgoing.contains(conn) == preferOutgoing || outgoing.contains(existing) == outgoing.contains(conn)
          // the edge moves to another connection: messages in flight on the old one are lost, so the
          // broadcast layer has to restart its handshake with the peer
          if keepNew then
              (
                use(conn).copy(outgoing = outgoing - existing),
                List(
                  OverlayAction.Close(existing),
                  OverlayAction.NeighborDown(peer.id),
                  OverlayAction.NeighborUp(peer.id)
                )
              )
          else (remembered.copy(outgoing = outgoing - conn), List(OverlayAction.Close(conn)))
  }

  private def rememberPeer(peer: PeerConnectInfo): FullMeshOverlay =
    if peer.id == self.id then this
    else {
      val merged = known.get(peer.id) match
          case Some(existing) => existing.copy(channelConnectors = existing.channelConnectors ++ peer.channelConnectors)
          case None           => peer
      copy(known = known.updated(peer.id, merged))
    }
}
