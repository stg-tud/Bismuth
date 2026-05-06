package replication.overlay

import channels.{ChannelConnectInfo, Connection, MessageBuffer, PeerConnectInfo}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid
import replication.overlay.OverlayController.OverlayAction
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.overlay.OverlayController.OverlayMessage.*

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

  /** Dummy connection for tests that don't care about the transport layer. */
  def dummyConn = new Connection {
    override def send(message: MessageBuffer): Async[Any, Unit] = Async {}
    override def close(): Unit = ()
  }

  private def peer(name: String): PeerConnectInfo =
    PeerConnectInfo(Uid.predefined(name), Set(ChannelConnectInfo.QueuedLocal(name)))
  private def machine(selfName: String = "self", config: HyParViewConfig = cfg): HyParViewStateMachine =
    HyParViewStateMachine.empty(peer(selfName), config, (_, _) => 0, canConnectTo = _.channelConnectors.nonEmpty)

  private def sent(actions: List[OverlayAction]): List[Any] =
    actions.collect { case OverlayAction.Send(_, message) => message }

  test("receive join adds the new node to active view and forwards join through current active peers") {
    val sm = machine()
    val a  = peer("a")
    val b  = peer("b")
    val n  = peer("new")

    val s1     = sm.discoverPeersResult(Set(a, b)).state
    val s2     = s1.receive(NeighborReply(a.uid, accepted = true), dummyConn).state
    val s3     = s2.receive(NeighborReply(b.uid, accepted = true), dummyConn).state
    val result = s3.receive(Join(n), dummyConn)

    assert(result.state.activeView.contains(n.uid))
    assert(sent(result.actions).exists { case Neighbor(from, true) => from.uid == sm.self.uid; case _ => false })
    val forwards = sent(result.actions).collect { case ForwardJoin(`n`, ttl, sender) => (ttl, sender) }
    assertEquals(forwards.size, 2)
    assert(forwards.forall(_._1 == cfg.activeRandomWalkLength))
  }

  test("forward join at passive random walk length adds the node to passive and forwards to another active peer") {
    val sm     = machine(config = cfg.copy(activeViewSize = 2))
    val a      = peer("a")
    val b      = peer("b")
    val n      = peer("new")
    val s1 = sm.discoverPeersResult(Set(a, b)).state
    val s2     = s1.receive(NeighborReply(a.uid, accepted = true), dummyConn).state
    val s3     = s2.receive(NeighborReply(b.uid, accepted = true), dummyConn).state
    val result = s3.receive(ForwardJoin(n, cfg.passiveRandomWalkLength, a.uid), dummyConn)

    assert(result.state.passiveView.contains(n.uid))
    val forwards = sent(result.actions).collect { case ForwardJoin(`n`, ttl, sender) => (ttl, sender) }
    assertEquals(forwards.head._1, cfg.passiveRandomWalkLength - 1)
  }

  test("forward join with ttl zero promotes the node to active and sends neighbor") {
    val n      = peer("new")
    val result = machine().receive(ForwardJoin(n, 0, peer("sender").uid), dummyConn)
    assert(result.state.activeView.contains(n.uid))
    assert(sent(result.actions).exists { case Neighbor(_, true) => true; case _ => false })
  }

  test("low priority neighbor is rejected when active view is full") {
    val a  = peer("a")
    val b  = peer("b")
    val c  = peer("c")
    val d  = peer("d")
    val sm = machine()
      .discoverPeersResult(Set(a, b, c)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
      .receive(NeighborReply(b.uid, true), dummyConn).state
      .receive(NeighborReply(c.uid, true), dummyConn).state

    val result = sm.receive(Neighbor(d, highPriority = false), dummyConn)
    assert(!result.state.activeView.contains(d.uid))
    assert(sent(result.actions).exists { case NeighborReply(_, false) => true; case _ => false })
  }

  test("disconnect removes the peer from active, moves it to passive, and requests healing") {
    val a  = peer("a")
    val b  = peer("b")
    val c  = peer("c")
    val sm = machine()
      .discoverPeersResult(Set(a, b, c)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
      .receive(NeighborReply(b.uid, true), dummyConn).state
      .receive(NeighborReply(c.uid, true), dummyConn).state

    val result = sm.receive(Disconnect(a.uid), dummyConn)
    assert(!result.state.activeView.contains(a.uid))
    assert(result.state.passiveView.contains(a.uid))
    assert(sent(result.actions).exists { case Neighbor(_, false) => true; case _ => false })
  }

  test("synthetic disconnect removes the peer from active membership state") {
    val a  = peer("a")
    val sm = machine().discoverPeersResult(Set(a)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
    val result = sm.receive(Disconnect(a.uid), dummyConn)
    assert(!result.state.activeView.contains(a.uid))
    assert(result.state.passiveView.contains(a.uid))
  }

  test("shuffle tick sends a shuffle to an active peer and includes self in the sample") {
    val a       = peer("a")
    val p       = peer("p")
    val sm = machine().discoverPeersResult(Set(a, p)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
    val result  = sm.shuffleTick()
    val shuffle = sent(result.actions).collectFirst { case msg @ Shuffle(_, _, _, _) => msg }.get
    assert(shuffle.sample.exists(_.uid == sm.self.uid))
  }

  test("shuffle forwarding hop does not merge the sample into passive state") {
    val a  = peer("a")
    val b  = peer("b")
    val x  = peer("x")
    val sm = machine(config = cfg.copy(activeViewSize = 2))
      .discoverPeersResult(Set(a, b)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
      .receive(NeighborReply(b.uid, true), dummyConn).state
    val result = sm.receive(Shuffle(x, Set(x), 1, a.uid), dummyConn)
    assert(!result.state.passiveView.contains(x.uid))
    assert(sent(result.actions).exists { case Shuffle(_, _, 0, _) => true; case _ => false })
  }

  test("shuffle acceptance merges incoming peers and replies with a passive sample") {
    val a      = peer("a")
    val p1     = peer("p1")
    val p2     = peer("p2")
    val x      = peer("x")
    val sm = machine().discoverPeersResult(Set(a, p1, p2)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
    val result = sm.receive(Shuffle(a, Set(x), 0, a.uid), dummyConn)
    assert(result.state.passiveView.contains(x.uid))
    assert(sent(result.actions).exists { case ShuffleReply(_, _) => true; case _ => false })
  }

  test("shuffle reply eviction prefers peers that were sent to the remote side") {
    val shuffleCfg =
      cfg.copy(activeViewSize = 1, passiveViewSize = 4, shuffleActiveSample = 1, shufflePassiveSample = 4)
    val a   = peer("a")
    val p1  = peer("p1")
    val p2  = peer("p2")
    val p3  = peer("p3")
    val p4  = peer("p4")
    val p5  = peer("p5")
    val x   = peer("x")
    val s1 = machine(config = shuffleCfg)
      .discoverPeersResult(Set(a, p1, p2, p3, p4)).state
      .receive(NeighborReply(a.uid, true), dummyConn).state
    val s2 = s1.discoverPeersResult(Set(p5)).state
    val afterTick = s2.shuffleTick().state
    val before    = afterTick.passiveView
    val result    = afterTick.receive(ShuffleReply(a.uid, Set(x)), dummyConn)
    assert(result.state.passiveView.contains(x.uid))
    assertEquals(result.state.passiveView.size, shuffleCfg.passiveViewSize)
    assert(before.diff(result.state.passiveView).nonEmpty)
  }

  test("undialable peers are filtered from passive learning") {
    val dialable   = peer("dialable")
    val undialable = PeerConnectInfo(Uid.predefined("undialable"), Set.empty)
    val sm    = machine().discoverPeersResult(Set(dialable, undialable)).state
    assert(sm.passiveView.contains(dialable.uid))
    assert(!sm.passiveView.contains(undialable.uid))
  }
}
