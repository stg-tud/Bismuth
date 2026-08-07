package channels

import channels.broadcast.PlumtreeBroadcast
import channels.connection.{LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import channels.overlay.FullMeshOverlay
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{LocalUid, Uid}

/** Deterministic reproduction of the rare CI stall in
  * `ex2021encfxtodo.TodoSignalingIntegrationTest`, using the exact overlay setup the todo app's
  * `ConnectionManager` uses (FullMeshOverlay + BroadcastIO over a message queue).
  *
  * The plumtree edge and the overlay's active-connection table are decoupled: the `Graft` message
  * is a *broadcast* envelope that never touches the overlay, so a replica can hold an eager edge to
  * a peer whose overlay connection is still being established. A write broadcast in that window is
  * pushed eagerly into a route that does not exist yet, is silently dropped, and is never announced
  * as `IHave` either — so nothing can ever request it back. That is the CI failure: the first write
  * of a freshly connected replica is lost forever.
  *
  * The test drives that interleaving deterministically (the manual queue delivers LIFO, so b
  * processes a's graft before a's Neighbor — an ordering a reentrant transport can produce), shows
  * the write being lost, and then shows that the plumtree repair tick — which the todo app's
  * `ConnectionManager` now drives every 100 ms — recovers it via a graft request + history replay.
  */
class BroadcastIOOverlayDropTest extends munit.FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  private class Fixture {

    var receivedA: List[Set[String]] = Nil
    var receivedB: List[Set[String]] = Nil

    private def mkNode(cb: Set[String] => Unit): BroadcastIO[Set[String]] = {
      val uid = LocalUid.gen()
      BroadcastIO[Set[String]](
        replicaId = uid,
        receiveCallback = cb,
        // same overlay setup as used by ex2021encfxtodo.ConnectionManager
        overlay = Some(FullMeshOverlay(PeerConnectInfo(uid.uid))),
      )
    }

    val a: BroadcastIO[Set[String]] = mkNode(s => receivedA = receivedA :+ s)
    val b: BroadcastIO[Set[String]] = mkNode(s => receivedB = receivedB :+ s)

    val queue: LocalMessageQueue = LocalMessageQueue()
    private val link             = QueuedLocalConnection("a<->b", queue)

    def connect(): Unit = {
      a.addServerConnection(link.server)
      b.addClientConnection(link.client("b"))
    }

    def drain(): Unit = {
      var safety = 0
      while queue.nonEmpty && safety < 100 do
          queue.deliverAll()
          safety += 1
      assert(safety < 100, "message queue did not quiesce")
    }

    def eagerPeerUids(io: BroadcastIO[Set[String]]): Set[Uid] = io.plumtreeState match
        case pt: PlumtreeBroadcast[?] => pt.eagerPeers
        case _                        => Set.empty

    /** Drive the exact interleaving that loses a write:
      * b receives a's graft (promoting a to eager) before b's overlay is told about a's connection.
      */
    def raceOverlayNotification(): Unit = {
      // queue (LIFO) now holds: [b -> a: Neighbor(b), a -> b: Neighbor(a)]
      // deliver b's Neighbor to a first: a registers b and answers with Graft(a).
      assert(queue.deliverOne(), "expected Neighbor(b) to be delivered to a")
      // the fresh Graft(a) is now the newest queued message, ahead of a's own Neighbor(a):
      assert(queue.deliverOne(), "expected Graft(a) to be delivered to b before Neighbor(a)")
    }
  }

  test("a write racing the overlay setup is lost, and the repair tick recovers it") {
    val f = new Fixture
    import f.*

    connect()
    raceOverlayNotification()

    // b has promoted a to eager (from the graft) while b's overlay has no active
    // connection for a yet (Neighbor(a) is still queued) — the eager push below is dropped.
    assertEquals(eagerPeerUids(b), Set(a.replicaId.uid))
    assert(b.overlayController.connectionFor(a.replicaId.uid).isEmpty, "overlay should not know a yet")

    b.broadcast(Set("v1")) // dropped: no route to a

    // deliver a's Neighbor(a) and let the rest of the handshake settle.
    assert(queue.deliverOne(), "expected Neighbor(a) to be delivered to b")
    drain()

    // like in the CI failure, a -> b still works (v0 gets through) …
    a.broadcast(Set("v0"))
    drain()
    assertEquals(receivedB, List(Set("v0")))

    // … but b's write that raced the setup is gone: it was never announced as IHave,
    // so without a repair tick nothing can request it back.
    assertEquals(receivedA, Nil)

    // heal: the same tick the todo app's ConnectionManager now drives every 100 ms.
    // a has learned b's context (from b's graft); its tick turns that into a graft request,
    // and b's history replay delivers the lost write.
    (0 until 5).foreach { _ =>
      a.tick()
      b.tick()
      drain()
    }
    assertEquals(receivedA, List(Set("v1")))
  }
}
