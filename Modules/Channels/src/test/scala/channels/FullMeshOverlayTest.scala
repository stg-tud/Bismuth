package channels

import channels.connection.{ConnectionDescriptor, Peer, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}
import channels.overlay.{AttemptId, FullMeshOverlay}
import munit.FunSuite
import rdts.base.Uid

class FullMeshOverlayTest extends FunSuite {

  private val self =
    PeerConnectInfo(Peer.Claimed(Uid.predefined("self")), Set(ConnectionDescriptor.QueuedLocal("self")))
  private val peer =
    PeerConnectInfo(Peer.Claimed(Uid.predefined("peer")), Set(ConnectionDescriptor.QueuedLocal("peer")))
  private val other =
    PeerConnectInfo(Peer.Claimed(Uid.predefined("other")), Set(ConnectionDescriptor.QueuedLocal("other")))

  private val c1 = StubConnection("c1")
  private val c2 = StubConnection("c2")

  extension (m: FullMeshOverlay) {
    def on(event: OverlayEvent): (FullMeshOverlay, List[OverlayAction]) =
        val (next, actions) = m.step(event)
        (next.asInstanceOf[FullMeshOverlay], actions)
  }

  test("a new connection is announced, but the peer only becomes a neighbor once it announces itself") {
    val (pending, announce) = FullMeshOverlay(self).on(OverlayEvent.Incoming(c1))
    assertEquals(announce, List(OverlayAction.Send(c1, OverlayMessage.Neighbor(self, highPriority = true))))
    assertEquals(pending.neighbors, Set.empty)

    val (active, up) = pending.on(OverlayEvent.Received(c1, OverlayMessage.Neighbor(peer, highPriority = true)))
    assertEquals(up, List(OverlayAction.NeighborUp(peer.id)))
    assertEquals(active.connectionFor(peer.id), Some(c1))
  }

  test("messages claiming another sender than the authenticated peer are discarded") {
    val authPeer            = peer.copy(id = Peer.Authenticated(peer.id.uid))
    val authenticatedAsPeer = StubConnection("authenticated", authenticatedPeer = Some(peer.id.uid))
    val (pending, _)        = FullMeshOverlay(self).on(OverlayEvent.Incoming(authenticatedAsPeer))

    // neither another authenticated identity, nor a merely claimed one (even with the same uid) is accepted
    val impostors = List(other.copy(id = Peer.Authenticated(other.id.uid)), other, peer)
    impostors.foreach { impostor =>
      val (ignored, ignoredActions) =
        pending.on(OverlayEvent.Received(authenticatedAsPeer, OverlayMessage.Neighbor(impostor, highPriority = true)))
      assertEquals(ignoredActions, Nil)
      assertEquals(ignored.neighbors, Set.empty)
    }

    val (accepted, acceptedActions) =
      pending.on(OverlayEvent.Received(authenticatedAsPeer, OverlayMessage.Neighbor(authPeer, highPriority = true)))
    assertEquals(acceptedActions, List(OverlayAction.NeighborUp(authPeer.id)))
    assertEquals(accepted.connectionFor(authPeer.id), Some(authenticatedAsPeer))
    assertEquals(accepted.connectionFor(peer.id), None)
  }

  test("unauthenticated connections can not claim authenticated peers") {
    val authPeer     = peer.copy(id = Peer.Authenticated(peer.id.uid))
    val (pending, _) = FullMeshOverlay(self).on(OverlayEvent.Incoming(c1))

    val (ignored, ignoredActions) =
      pending.on(OverlayEvent.Received(c1, OverlayMessage.Neighbor(authPeer, highPriority = true)))
    assertEquals(ignoredActions, Nil)
    assertEquals(ignored.neighbors, Set.empty)
  }

  test("of two connections to the same peer, both sides keep the one dialed by the smaller id") {
    // peer < self: the peer's dial (incoming c2) wins over our own dial (c1), regardless of announcement order
    def withBoth(firstAnnounced: channels.connection.Connection, secondAnnounced: channels.connection.Connection) =
      FullMeshOverlay(self)
        .on(OverlayEvent.Discovered(Set(peer)))._1
        .on(OverlayEvent.Established(AttemptId(0), c1))._1
        .on(OverlayEvent.Incoming(c2))._1
        .on(OverlayEvent.Received(firstAnnounced, OverlayMessage.Neighbor(peer, highPriority = true)))._1
        .on(OverlayEvent.Received(secondAnnounced, OverlayMessage.Neighbor(peer, highPriority = true)))

    val (kept, keptActions) = withBoth(c2, c1)
    assertEquals(kept.connectionFor(peer.id), Some(c2))
    assertEquals(keptActions, List(OverlayAction.Close(c1)))

    // the edge moves to the peer's dial: the broadcast layer restarts its handshake on the new connection
    val (switched, switchedActions) = withBoth(c1, c2)
    assertEquals(switched.connectionFor(peer.id), Some(c2))
    assertEquals(
      switchedActions,
      List(OverlayAction.Close(c1), OverlayAction.NeighborDown(peer.id), OverlayAction.NeighborUp(peer.id))
    )
  }

  test("Join is answered with the known peers; the answer makes the joiner connect to them") {
    val contact =
      PeerConnectInfo(Peer.Claimed(Uid.predefined("contact")), Set(ConnectionDescriptor.QueuedLocal("contact")))

    val (_, joinActions) = FullMeshOverlay(self, known = Map(other.id -> other))
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.Join(contact)))
    assertEquals(
      joinActions,
      List(OverlayAction.Send(c1, OverlayMessage.ShuffleReply(self.id, Set(self, other), Set.empty)))
    )

    val (_, infoActions) = FullMeshOverlay(self)
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.ShuffleReply(contact.id, Set(self, other), Set.empty)))
    assertEquals(infoActions, List(OverlayAction.Dial(AttemptId(0), other.channelConnectors)))
  }

  test("a closed neighbor connection reports NeighborDown and forgets the peer") {
    val active = FullMeshOverlay(self)
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.Neighbor(peer, highPriority = true)))._1
    assertEquals(active.known.get(peer.id), Some(peer))

    val (closed, actions) = active.on(OverlayEvent.Closed(c1))
    assertEquals(actions, List(OverlayAction.NeighborDown(peer.id)))
    assertEquals(closed.known.get(peer.id), None)
  }

  test("discovering a peer dials it once; bootstrap dials the contact and sends Join once connected") {
    val bootstrap = ConnectionDescriptor.QueuedLocal("bootstrap")

    val (discovered, discoverActions) = FullMeshOverlay(self).on(OverlayEvent.Discovered(Set(other)))
    assertEquals(discoverActions, List(OverlayAction.Dial(AttemptId(0), other.channelConnectors)))
    assertEquals(discovered.on(OverlayEvent.Discovered(Set(other)))._2, Nil, "already dialing")

    val (joining, bootstrapActions) = discovered.on(OverlayEvent.Bootstrap(bootstrap))
    assertEquals(bootstrapActions, List(OverlayAction.Dial(AttemptId(1), Set(bootstrap))))
    assertEquals(
      joining.on(OverlayEvent.Established(AttemptId(1), c1))._2,
      List(
        OverlayAction.Send(c1, OverlayMessage.Neighbor(self, highPriority = true)),
        OverlayAction.Send(c1, OverlayMessage.Join(self))
      )
    )
  }
}
