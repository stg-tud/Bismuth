package channels

import channels.broadcast.PlumtreeBroadcast
import channels.connection.{LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import channels.overlay.FullMeshOverlay
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{LocalUid, Uid}

/** Regression tests for the handshake graft race that made `ex2021encfxtodo.TodoSignalingIntegrationTest`
  * flaky (updates to a replicated todo randomly stopped replicating right after connecting).
  *
  * Background: when a peer connection is established, both sides add each other as plumtree peers and
  * exchange `Graft(localContext)` to backfill missing history. A broadcast that races this exchange must
  * still be delivered exactly once and without any repair ticks.
  *
  * Previously [[PlumtreeBroadcast.addPeer]] added the peer as ''eager'', so a payload broadcast while the
  * peer's stale `Graft(∅)` was still in flight was delivered twice: once by the eager push and once by the
  * graft replay. The duplicate was misread as a redundant mesh edge, so both sides pruned the only edge to
  * lazy; subsequent broadcasts were announced as `IHave` only and — since nothing drives
  * [[BroadcastIO.tick]] in the todo app's `ConnectionManager` — never repaired.
  *
  * Peers are now added as ''lazy'' and promoted to eager when their graft is processed, which makes such an
  * overlap impossible: before promotion no payloads are pushed eagerly (only `IHave`s), and the graft
  * replay is the single source of payload data. These tests pin both race flavors deterministically using
  * a manually delivered message queue.
  */
class BroadcastIOHandshakeRaceTest extends munit.FunSuite {

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

    /** Drives the exact message interleaving that lost the handshake race:
      * `a` broadcasts `Set("initial")` while the handshake grafts are still in flight.
      */
    def driveHandshakeRace(): Unit = {
      // (1) Neighbor handshake: both sides add each other as plumtree peers and enqueue Graft(localContext).
      queue.deliverAll()

      // (2) the race: `a` broadcasts before the queued grafts are processed.
      a.broadcast(Set("initial"))

      // (3) the eager push / IHave and the stale grafts are delivered and processed.
      drain()
    }
  }

  test("update broadcast during the connection handshake is replicated exactly once") {
    val f = new Fixture
    import f.*

    connect()
    driveHandshakeRace()

    // the racing update is delivered exactly once (a duplicate would previously have pruned both edges)
    assertEquals(receivedB, List(Set("initial")))

    // the graft exchange promoted both edges to eager; no spurious prune happened
    assertEquals(eagerPeerUids(a), Set(b.replicaId.uid))
    assertEquals(eagerPeerUids(b), Set(a.replicaId.uid))

    // and subsequent updates keep replicating without any repair ticks
    a.broadcast(Set("edited by a"))
    drain()
    assertEquals(receivedB, List(Set("initial"), Set("edited by a")))

    b.broadcast(Set("edited by b"))
    drain()
    assertEquals(receivedA, List(Set("edited by b")))
  }

  test("update broadcast before the peer connects is backfilled by the handshake graft") {
    val f = new Fixture
    import f.*

    // broadcast with no peers at all: nothing is sent, the payload is only remembered
    a.broadcast(Set("initial"))

    connect()
    drain()

    // the handshake graft of the freshly connected peer backfills the missed update
    assertEquals(receivedB, List(Set("initial")))

    a.broadcast(Set("edited by a"))
    drain()
    assertEquals(receivedB, List(Set("initial"), Set("edited by a")))
  }
}
