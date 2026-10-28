package channels

import channels.connection.{ConnectionDescriptor, PeerConnectInfo}
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage}
import channels.overlay.{AttemptId, ConnId, FullMeshOverlay}
import munit.FunSuite
import rdts.base.Uid

class FullMeshOverlayTest extends FunSuite {

  private val self  = PeerConnectInfo(Uid.predefined("self"), Set(ConnectionDescriptor.QueuedLocal("self")))
  private val peer  = PeerConnectInfo(Uid.predefined("peer"), Set(ConnectionDescriptor.QueuedLocal("peer")))
  private val other = PeerConnectInfo(Uid.predefined("other"), Set(ConnectionDescriptor.QueuedLocal("other")))

  private val c1 = ConnId(1)
  private val c2 = ConnId(2)

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
    assertEquals(up, List(OverlayAction.NeighborUp(peer.uid)))
    assertEquals(active.connectionFor(peer.uid), Some(c1))
  }

  test("of two connections to the same peer, both sides keep the one dialed by the smaller id") {
    // peer < self: the peer's dial (incoming c2) wins over our own dial (c1), regardless of announcement order
    def withBoth(firstAnnounced: ConnId, secondAnnounced: ConnId) =
      FullMeshOverlay(self)
        .on(OverlayEvent.Discovered(Set(peer)))._1
        .on(OverlayEvent.Established(AttemptId(0), c1))._1
        .on(OverlayEvent.Incoming(c2))._1
        .on(OverlayEvent.Received(firstAnnounced, OverlayMessage.Neighbor(peer, highPriority = true)))._1
        .on(OverlayEvent.Received(secondAnnounced, OverlayMessage.Neighbor(peer, highPriority = true)))

    val (kept, keptActions) = withBoth(c2, c1)
    assertEquals(kept.connectionFor(peer.uid), Some(c2))
    assertEquals(keptActions, List(OverlayAction.Close(c1)))

    // the edge moves to the peer's dial: the broadcast layer restarts its handshake on the new connection
    val (switched, switchedActions) = withBoth(c1, c2)
    assertEquals(switched.connectionFor(peer.uid), Some(c2))
    assertEquals(
      switchedActions,
      List(OverlayAction.Close(c1), OverlayAction.NeighborDown(peer.uid), OverlayAction.NeighborUp(peer.uid))
    )
  }

  test("Join is answered with the known peers; the answer makes the joiner connect to them") {
    val contact = PeerConnectInfo(Uid.predefined("contact"), Set(ConnectionDescriptor.QueuedLocal("contact")))

    val (_, joinActions) = FullMeshOverlay(self, known = Map(other.uid -> other))
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.Join(contact)))
    assertEquals(
      joinActions,
      List(OverlayAction.Send(c1, OverlayMessage.ShuffleReply(self.uid, Set(self, other), Set.empty)))
    )

    val (_, infoActions) = FullMeshOverlay(self)
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.ShuffleReply(contact.uid, Set(self, other), Set.empty)))
    assertEquals(infoActions, List(OverlayAction.Dial(AttemptId(0), other.channelConnectors)))
  }

  test("a closed neighbor connection reports NeighborDown and forgets the peer") {
    val active = FullMeshOverlay(self)
      .on(OverlayEvent.Incoming(c1))._1
      .on(OverlayEvent.Received(c1, OverlayMessage.Neighbor(peer, highPriority = true)))._1
    assertEquals(active.known.get(peer.uid), Some(peer))

    val (closed, actions) = active.on(OverlayEvent.Closed(c1))
    assertEquals(actions, List(OverlayAction.NeighborDown(peer.uid)))
    assertEquals(closed.known.get(peer.uid), None)
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
