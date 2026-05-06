package replication

import munit.FunSuite
import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event
import replication.PlumtreeBroadcast.Event.Send
import replication.PlumtreeBroadcast.{Peer, PeerRole}
import replication.PlumtreeMessage.*

class PlumtreeBroadcastTest extends FunSuite {

  private val self    = Uid.predefined("self")
  private val eagerA  = Peer(Uid.predefined("eager-a"))
  private val eagerB  = Peer(Uid.predefined("eager-b"))
  private val lazyA   = Peer(Uid.predefined("lazy-a"))
  private val sender  = Peer(Uid.predefined("sender"))
  private val missing = Peer(Uid.predefined("missing"))

  private def payload(from: Uid, time: Long, value: String): Payload[String] =
    Payload(Dots.single(from, time), value)

  test("broadcast sends payload on eager links and IHave on lazy links") {
    val state = PlumtreeBroadcast[String](
      self,
      deltaStorage = KeepAllHistory(),
      peerRoles = Map(
        eagerA -> PeerRole.Eager,
        eagerB -> PeerRole.Eager,
        lazyA -> PeerRole.Lazy,
      )
    )

    val msg                = payload(self, 0, "v1")
    val PlumtreeBroadcast.Result(next, events) = state.broadcast(msg)

    assert(next.localContext.contains(msg.dots))
    assertEquals(next.deltaStorage.getHistory, List(msg))
    assertEquals(
      events.toList,
      List(
        Send(List(eagerA, eagerB), msg),
        Send(List(lazyA), IHave(self, msg.dots))
      )
    )
  }

  test("first eager payload delivery keeps sender eager, delivers locally, and forwards using eager plus lazy edges") {
    val state = PlumtreeBroadcast[String](
      self,
      deltaStorage = KeepAllHistory(),
      peerRoles = Map(
        sender -> PeerRole.Lazy,
        eagerA -> PeerRole.Eager,
        lazyA -> PeerRole.Lazy,
      )
    )

    val msg                = payload(Uid.predefined("origin"), 0, "hello")
    val PlumtreeBroadcast.Result(next, events) = state.handleMessage(sender, msg)

    assert(next.localContext.contains(msg.dots))
    assertEquals(next.peerRoles(sender), PeerRole.Eager, "")
    assertEquals(
      events.toList,
      List(
        Event.Deliver(msg),
        Send(List(eagerA), msg),
        Send(List(lazyA), IHave(self, msg.dots))
      )
    )
  }

  test("duplicate eager payload is treated as a non-tree edge and answered with Prune") {
    val msg = payload(Uid.predefined("origin"), 0, "hello")
    val state = PlumtreeBroadcast[String](
      self,
      localContext = msg.dots,
      deltaStorage = KeepAllHistory[String]().remember(msg),
      peerRoles = Map(sender -> PeerRole.Eager)
    )

    val PlumtreeBroadcast.Result(next, events) = state.handleMessage(sender, msg)

    assertEquals(next.peerRoles(sender), PeerRole.Lazy, "")
    assertEquals(events.toList, List(Send(List(sender), Prune(self))))
  }

  test("Prune demotes the corresponding edge to lazy") {
    val state = PlumtreeBroadcast[String](
      self,
      deltaStorage = NoHistory[String](),
      peerRoles = Map(sender -> PeerRole.Eager)
    )

    val PlumtreeBroadcast.Result(next, events) = state.handleMessage(sender, Prune(sender.uid))

    assertEquals(next.peerRoles(sender), PeerRole.Lazy, "")
    assertEquals(events.toList, Nil)
  }

  test("IHave updates remote knowledge and a subsequent graft tick requests repair from a peer that knows more") {
    val local  = Dots.single(self, 0)
    val remote = local.merge(Dots.single(missing.uid, 0))
    val state = PlumtreeBroadcast[String](
      self,
      localContext = local,
      deltaStorage = NoHistory[String](),
      peerRoles = Map(missing -> PeerRole.Lazy),
      remoteContextSnapshot = Map(missing -> local)
    )

    val PlumtreeBroadcast.Result(afterIHave, noEvents) = state.handleMessage(missing, IHave(missing.uid, remote))
    assertEquals(noEvents.toList, Nil)
    assertEquals(afterIHave.remoteContext(missing), remote, "")

    val PlumtreeBroadcast.Result(afterFirstTick, firstTickEvents) = afterIHave.tickGrafts()
    assertEquals(afterFirstTick.peerRoles(missing), PeerRole.Lazy, "")
    assertEquals(firstTickEvents.toList, Nil)
    assertEquals(afterFirstTick.remoteContextSnapshot, afterIHave.remoteContext)

    val PlumtreeBroadcast.Result(afterSecondTick, secondTickEvents) = afterFirstTick.tickGrafts()
    assertEquals(afterSecondTick.peerRoles(missing), PeerRole.Eager, "")
    assertEquals(secondTickEvents.toList, List(Send(List(missing), Graft(self, local))))
  }

  test("Graft promotes sender back to eager and replays only the payloads missing from knows") {
    val p0 = payload(self, 0, "a")
    val p1 = payload(self, 1, "b")
    val state = PlumtreeBroadcast[String](
      self,
      localContext = p0.dots.merge(p1.dots),
      deltaStorage = KeepAllHistory[String](List(p1, p0)),
      peerRoles = Map(sender -> PeerRole.Lazy)
    )

    val PlumtreeBroadcast.Result(next, events) = state.handleMessage(sender, Graft(sender.uid, p0.dots))

    assertEquals(next.peerRoles(sender), PeerRole.Eager, "")
    assertEquals(next.remoteContext(sender), p0.dots, "")
    assertEquals(events.toList, List(Send(List(sender), p1)))
  }

  test("addPeer models NeighborUp by making the peer eager and immediately requesting missing history") {
    val local = Dots.single(self, 2)
    val state = PlumtreeBroadcast[String](self, localContext = local, deltaStorage = NoHistory[String]())

    val PlumtreeBroadcast.Result(next, events) = state.addPeer(eagerA)

    assertEquals(next.peerRoles(eagerA), PeerRole.Eager, "")
    assertEquals(events.toList, List(Send(List(eagerA), Graft(self, local))))
  }
}
