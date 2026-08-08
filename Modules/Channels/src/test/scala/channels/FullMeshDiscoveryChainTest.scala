package channels

import channels.BroadcastIO
import channels.connection.{Connection, ConnectionDescriptor, ConnectionInfo, LocalConnectionRegistry, LocalMessageQueue, MessageBuffer, PeerConnectInfo, QueuedLocalConnection}
import channels.experiments.Aead
import channels.overlay.FullMeshOverlay
import channels.overlay.OverlayController.{OverlayAction, OverlayMessage}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.{LocalUid, Uid}

import scala.util.{Failure, Success, Try}

/** Verifies the discovery chain the user hypothesised:
  *
  *   1. `discover` a peer → overlay emits a `SendJoin(Neighbor)` action
  *   2. `SendJoin` triggers `connectAndSend`, which registers the connection
  *   3. (On the other side the passive connection) activation triggers a Neighbor request
  *   4. the Neighbor carried by the original `SendJoin` is then delivered
  *
  * Each step is checked: the overlay action for step 1, and then through the BroadcastIO +
  * FullMeshOverlay wiring that steps 2-4 really establish a working active link in both directions.
  */
class FullMeshDiscoveryChainTest extends FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  final private case class TestConnection(name: String) extends Connection {
    override def info: ConnectionInfo                           = ConnectionInfo("name" -> name)
    override def send(message: MessageBuffer): Async[Any, Unit] = Async(())
    override def close(): Unit                                  = ()
    override def toString: String                               = s"TestConnection($name)"
  }

  private val self = PeerConnectInfo(Uid.predefined("self"), Set(ConnectionDescriptor.QueuedLocal("self")))
  private val peer = PeerConnectInfo(Uid.predefined("peer"), Set(ConnectionDescriptor.QueuedLocal("peer")))

  test("step 1: discoverPassive emits exactly one SendJoin carrying a Neighbor") {
    val (_, actions) = FullMeshOverlay(self).discoverPassive(Set(peer))
    assertEquals(
      actions,
      List(
        OverlayAction.SendJoin(
          peer.channelConnectors,
          peer.uid,
          OverlayMessage.Neighbor(self, highPriority = true),
        )
      )
    )
  }

  test("step 3: activating a passive (server) connection emits a Neighbor request") {
    val conn                     = TestConnection("incoming")
    val (_, activationActions)   = FullMeshOverlay(self).activateConnection(conn, None)
    assertEquals(
      activationActions,
      List(OverlayAction.Send(conn, OverlayMessage.Neighbor(self, highPriority = true)))
    )
  }

  test("the whole chain: discover leads to a working mutual active link via broadcast protocol") {
    // node `listener` owns a queued connection it is listening on; node `discoverer` learns about it
    // and runs `discover`, which should drive steps 1-4 end to end.
    val queue    = LocalMessageQueue()
    val link     = QueuedLocalConnection("listener", queue) // id must match the node's QueuedLocal descriptor
    val resolver = LocalConnectionRegistry(Map("listener" -> link))

    final case class Node(id: String) {
      val uid: LocalUid = LocalUid.gen()
      val selfInfo      = PeerConnectInfo(uid.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      val io            = BroadcastIO[Set[String]](
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

    // Step 1+2: discoverer discovers a single peer → SendJoin(Neighbor) → connectAndSend dials & registers.
    discoverer.io.discover(Set(listener.selfInfo))

    def describe(mb: Try[MessageBuffer]): String = mb match
        case Success(buf) =>
          BroadcastIO.decodeEnvelope[Set[String]](buf, Aead.identity) match
              case Success(env) => env.toString
              case Failure(err) => s"<undecodable: $err>"
        case Failure(err) => s"<failure: $err>"

    def observe(step: String): Unit =
        println(s"--- $step ---")
        println(s"  queued:    ${queue.elements.map(describe)}")
        println(s"  delivered: ${queue.delivered.map(describe)}")

    observe("after discover")

    // The connection was established synchronously by connectAndSend (step 2), so before anything is
    // delivered the queue must already carry the Neighbor handshakes from both sides (steps 3+4).
    val queuedNeighbors = queue.elements.flatMap(mb =>
      mb.toOption
        .flatMap(buf => BroadcastIO.decodeEnvelope[Set[String]](buf, Aead.identity).toOption)
        .collect { case BroadcastIO.Envelope.Membership(OverlayMessage.Neighbor(from, _)) => from.uid }
    )
    assertEquals(
      queuedNeighbors.toSet,
      Set(listener.uid.uid, discoverer.uid.uid),
      "multiple Neighbor requests must be sent (one from each side)",
    )

    // Deliver step by step and observe the queue after every delivery.
    var steps = 0
    while queue.nonEmpty && steps < 10 do
        queue.deliverOne()
        steps += 1
        observe(s"after deliverOne #$steps")
    assert(steps < 10, s"queue did not quiesce, remaining=${queue.size}")

    // All the Neighbor requests eventually reach the other side.
    val deliveredNeighbors = queue.delivered.flatMap(mb =>
      mb.toOption
        .flatMap(buf => BroadcastIO.decodeEnvelope[Set[String]](buf, Aead.identity).toOption)
        .collect { case BroadcastIO.Envelope.Membership(OverlayMessage.Neighbor(from, _)) => from.uid }
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

    // Both sides report ActiveConnectionAdded → both have the peer in the broadcast-protocol active view.
    val listenerPeers   = listener.io.plumtreeState.asInstanceOf[channels.broadcast.PlumtreeBroadcast[?]].peerRoles.keySet
    val discovererPeers = discoverer.io.plumtreeState.asInstanceOf[channels.broadcast.PlumtreeBroadcast[?]].peerRoles.keySet
    assertEquals(listenerPeers.map(_.uid), Set(discoverer.uid.uid))
    assertEquals(discovererPeers.map(_.uid), Set(listener.uid.uid))

    // And each side has a live attached connection for the remote peer.
    assert(listenerOverlay.connectionFor(discoverer.uid.uid).isDefined)
    assert(discovererOverlay.connectionFor(listener.uid.uid).isDefined)
  }
}
