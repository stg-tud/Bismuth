package channels

import channels.connection.Peer
import channels.PlumtreeIO
import channels.connection.{ConnectionDescriptor, LocalConnectionRegistry, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import channels.experiments.Aead
import channels.overlay.FullMeshOverlay
import channels.overlay.OverlayController.OverlayMessage
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import munit.FunSuite
import rdts.base.ReplicaId

/** Verifies the full-mesh discovery chain through the PlumtreeIO wiring:
  *
  *   1. `discover` a peer → overlay emits a `Dial`
  *   2. the driver establishes the connection and reports `Established` (the other side sees `Incoming`)
  *   3. both sides announce their own peer id via a `Neighbor`
  *   4. each side receives the other's `Neighbor` and reports `NeighborUp`
  *
  * The single overlay steps are covered by [[FullMeshOverlayTest]].
  */
class FullMeshDiscoveryChainTest extends FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  test("the whole chain: discover leads to a working mutual active link via broadcast protocol") {
    // node `listener` owns a queued connection it is listening on; node `discoverer` learns about it
    // and runs `discover`, which should drive steps 1-4 end to end.
    val queue    = LocalMessageQueue()
    val link     = QueuedLocalConnection("listener", queue) // id must match the node's QueuedLocal descriptor
    val resolver = LocalConnectionRegistry(Map("listener" -> link))

    final case class Node(id: String) {
      val uid: ReplicaId               = ReplicaId.gen()
      val selfInfo: PeerConnectInfo    = PeerConnectInfo(Peer.Claimed(uid.uid), Set(ConnectionDescriptor.QueuedLocal(id)))
      val io: PlumtreeIO.of[Set[String]] = PlumtreeIO[Set[String]](
        uid,
        _ => (),
        overlay = Some(FullMeshOverlay(selfInfo)),
        resolver = resolver,
      )
    }

    val listener   = Node("listener")
    val discoverer = Node("discoverer")

    // listener opens its server transport so it can accept the connection discoverer will dial.
    listener.io.addServerConnection(resolver.queuedServer(listener.selfInfo.channelConnectors.head).get)

    // Step 1+2: discoverer discovers a single peer → Dial → the driver establishes the connection.
    discoverer.io.discover(Set(listener.selfInfo))

    // debug prints removed

    // Steps 3+4: the connection was established synchronously (step 2) and both
    // sides announced themselves on activation, so before anything is delivered the queue must
    // already carry the Neighbor handshakes from both sides.
    val queuedNeighbors = queue.elements.flatMap(mb =>
      mb.toOption
        .flatMap(buf => PlumtreeIO.decodeEnvelope[Set[String]](buf, Aead.identity).toOption)
        .collect { case PlumtreeIO.Envelope.Membership(OverlayMessage.Neighbor(from, _)) => from.id.uid }
    )
    assertEquals(
      queuedNeighbors.toSet,
      Set(listener.uid.uid, discoverer.uid.uid),
      "multiple Neighbor requests must be sent (one from each side)",
    )

    // Deliver step by step until quiescent.
    var steps = 0
    while queue.nonEmpty && steps < 10 do
        queue.deliverOne()
        steps += 1
    assert(steps < 10, s"queue did not quiesce, remaining=${queue.size}")

    // All the Neighbor requests eventually reach the other side.
    val deliveredNeighbors = queue.delivered.flatMap(mb =>
      mb.toOption
        .flatMap(buf => PlumtreeIO.decodeEnvelope[Set[String]](buf, Aead.identity).toOption)
        .collect { case PlumtreeIO.Envelope.Membership(OverlayMessage.Neighbor(from, _)) => from.id.uid }
    )
    assertEquals(
      deliveredNeighbors.toSet,
      Set(listener.uid.uid, discoverer.uid.uid),
      "each side must have received the other's Neighbor request",
    )

    // 2: connectAndSend registered discoverer's connection to listener, and the Neighbor handshake ran
    //    3: listener's server-side activation emitted a Neighbor back, 4: ... which fully confirmed.
    val listenerOverlay   = listener.io.overlayController.asInstanceOf[FullMeshOverlay]
    val discovererOverlay = discoverer.io.overlayController.asInstanceOf[FullMeshOverlay]

    // Both sides report NeighborUp → both have the peer in the broadcast-protocol active view.
    val listenerPeers = listener.io.state.peerRoles.keySet
    val discovererPeers =
      discoverer.io.state.peerRoles.keySet
    assertEquals(listenerPeers.map(_.uid), Set(discoverer.uid.uid))
    assertEquals(discovererPeers.map(_.uid), Set(listener.uid.uid))

    // And each side has a live attached connection for the remote peer.
    assert(listenerOverlay.connectionFor(Peer.Claimed(discoverer.uid.uid)).isDefined)
    assert(discovererOverlay.connectionFor(Peer.Claimed(listener.uid.uid)).isDefined)
  }
}
