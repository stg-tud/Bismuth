package replication.overlay

import channels.ChannelConnectDescriptor
import munit.FunSuite
import rdts.base.Uid
import replication.overlay.HyParViewMultiplexed.PeerRef
import replication.overlay.HyParViewStateMachine.Action
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.overlay.HyParViewUnified.HyParViewMessage.*

class HyParViewStateMachineTest extends FunSuite {

  private val cfg = HyParViewConfig(
    activeViewSize = 3,
    passiveViewSize = 4,
    activeRandomWalkLength = 4,
    passiveRandomWalkLength = 2,
    shuffleRandomWalkLength = 2,
    shuffleActiveSample = 2,
    shufflePassiveSample = 2,
  )

  private def peer(name: String): PeerRef = PeerRef(Uid.predefined(name), Set(ChannelConnectDescriptor.QueuedLocal(name)))
  private def machine(selfName: String = "self", config: HyParViewConfig = cfg): HyParViewStateMachine =
    HyParViewStateMachine.empty(peer(selfName), config, (_, _) => 0, canConnectTo = _.channelConnectors.nonEmpty)

  private def sent(actions: List[Action]): List[(PeerRef, Any)] =
    actions.collect { case Action.Send(to, message) => (to, message) }

  test("receive join adds the new node to active view and forwards join through current active peers") {
    val sm = machine()
    val a = peer("a")
    val b = peer("b")
    val n = peer("new")

    val s1 = sm.discoverPeers(Set(a, b)).state
    val s2 = s1.receive(NeighborReply(a.uid, accepted = true)).state
    val s3 = s2.receive(NeighborReply(b.uid, accepted = true)).state
    val result = s3.receive(Join(n))

    assert(result.state.activeView.contains(n.uid))
    assert(sent(result.actions).exists { case (to, Neighbor(from, true)) => to == n && from.uid == sm.self.uid; case _ => false })
    val forwards = sent(result.actions).collect { case (to, ForwardJoin(`n`, ttl, sender)) => (to.uid, ttl, sender) }
    assertEquals(forwards.map(_._1).toSet, Set(a.uid, b.uid))
    assert(forwards.forall(_._2 == cfg.activeRandomWalkLength))
  }

  test("forward join at passive random walk length adds the node to passive and forwards to another active peer") {
    val sm = machine(config = cfg.copy(activeViewSize = 2))
    val a = peer("a")
    val b = peer("b")
    val n = peer("new")
    val s1 = sm.discoverPeers(Set(a, b)).state
    val s2 = s1.receive(NeighborReply(a.uid, accepted = true)).state
    val s3 = s2.receive(NeighborReply(b.uid, accepted = true)).state
    val result = s3.receive(ForwardJoin(n, cfg.passiveRandomWalkLength, a.uid))

    assert(result.state.passiveView.contains(n.uid))
    val forwards = sent(result.actions).collect { case (to, ForwardJoin(`n`, ttl, sender)) => (to.uid, ttl, sender) }
    assertEquals(forwards.head._1, b.uid)
    assertEquals(forwards.head._2, cfg.passiveRandomWalkLength - 1)
  }

  test("forward join with ttl zero promotes the node to active and sends neighbor") {
    val n = peer("new")
    val result = machine().receive(ForwardJoin(n, 0, peer("sender").uid))
    assert(result.state.activeView.contains(n.uid))
    assert(sent(result.actions).exists { case (to, Neighbor(_, true)) => to == n; case _ => false })
  }

  test("low priority neighbor is rejected when active view is full") {
    val a = peer("a")
    val b = peer("b")
    val c = peer("c")
    val d = peer("d")
    val sm = machine()
      .discoverPeers(Set(a, b, c)).state
      .receive(NeighborReply(a.uid, true)).state
      .receive(NeighborReply(b.uid, true)).state
      .receive(NeighborReply(c.uid, true)).state

    val result = sm.receive(Neighbor(d, highPriority = false))
    assert(!result.state.activeView.contains(d.uid))
    assert(sent(result.actions).exists { case (to, NeighborReply(_, false)) => to == d; case _ => false })
  }

  test("disconnect removes the peer from active, moves it to passive, and requests healing") {
    val a = peer("a")
    val b = peer("b")
    val c = peer("c")
    val sm = machine()
      .discoverPeers(Set(a, b, c)).state
      .receive(NeighborReply(a.uid, true)).state
      .receive(NeighborReply(b.uid, true)).state
      .receive(NeighborReply(c.uid, true)).state

    val result = sm.receive(Disconnect(a.uid))
    assert(!result.state.activeView.contains(a.uid))
    assert(result.state.passiveView.contains(a.uid))
    assert(sent(result.actions).exists { case (_, Neighbor(_, false)) => true; case _ => false })
  }

  test("peer loss removes the peer from membership state") {
    val a = peer("a")
    val sm = machine().discoverPeers(Set(a)).state
    val result = sm.peerLost(a.uid)
    assert(!result.state.activeView.contains(a.uid))
    assert(!result.state.passiveView.contains(a.uid))
  }

  test("shuffle tick sends a shuffle to an active peer and includes self in the sample") {
    val a = peer("a")
    val p = peer("p")
    val sm = machine().discoverPeers(Set(a, p)).state.receive(NeighborReply(a.uid, true)).state
    val result = sm.shuffleTick()
    val shuffle = sent(result.actions).collectFirst { case (to, msg @ Shuffle(_, _, _, _)) => (to, msg) }.get
    assertEquals(shuffle._1, a)
    assert(shuffle._2.sample.exists(_.uid == sm.self.uid))
  }

  test("shuffle forwarding hop does not merge the sample into passive state") {
    val a = peer("a")
    val b = peer("b")
    val x = peer("x")
    val sm = machine(config = cfg.copy(activeViewSize = 2))
      .discoverPeers(Set(a, b)).state
      .receive(NeighborReply(a.uid, true)).state
      .receive(NeighborReply(b.uid, true)).state
    val result = sm.receive(Shuffle(x, Set(x), 1, a.uid))
    assert(!result.state.passiveView.contains(x.uid))
    assert(sent(result.actions).exists { case (to, Shuffle(_, _, 0, _)) => to.uid == b.uid; case _ => false })
  }

  test("shuffle acceptance merges incoming peers and replies with a passive sample") {
    val a = peer("a")
    val p1 = peer("p1")
    val p2 = peer("p2")
    val x = peer("x")
    val sm = machine().discoverPeers(Set(a, p1, p2)).state.receive(NeighborReply(a.uid, true)).state
    val result = sm.receive(Shuffle(a, Set(x), 0, a.uid))
    assert(result.state.passiveView.contains(x.uid))
    assert(sent(result.actions).exists { case (to, ShuffleReply(_, _)) => to == a; case _ => false })
  }

  test("shuffle reply eviction prefers peers that were sent to the remote side") {
    val shuffleCfg = cfg.copy(activeViewSize = 1, passiveViewSize = 4, shuffleActiveSample = 1, shufflePassiveSample = 4)
    val a = peer("a")
    val p1 = peer("p1")
    val p2 = peer("p2")
    val p3 = peer("p3")
    val p4 = peer("p4")
    val p5 = peer("p5")
    val x = peer("x")
    val sm0 = machine(config = shuffleCfg)
      .discoverPeers(Set(a, p1, p2, p3, p4)).state
      .receive(NeighborReply(a.uid, true)).state
      .discoverPeers(Set(p5)).state
    val afterTick = sm0.shuffleTick().state
    val before = afterTick.passiveView
    val result = afterTick.receive(ShuffleReply(a.uid, Set(x)))
    assert(result.state.passiveView.contains(x.uid))
    assertEquals(result.state.passiveView.size, shuffleCfg.passiveViewSize)
    assert(before.diff(result.state.passiveView).nonEmpty)
  }

  test("undialable peers are filtered from passive learning") {
    val dialable = peer("dialable")
    val undialable = PeerRef(Uid.predefined("undialable"), Set.empty)
    val sm = machine().discoverPeers(Set(dialable, undialable)).state
    assert(sm.passiveView.contains(dialable.uid))
    assert(!sm.passiveView.contains(undialable.uid))
  }
}
