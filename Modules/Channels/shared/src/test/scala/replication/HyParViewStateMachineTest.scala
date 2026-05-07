package replication

import channels.{ChannelConnectInfo, Connection, ConnectionInfo, PeerConnectInfo}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid
import replication.overlay.HyParViewStateMachine.{ActivePeer, HyParViewConfig}
import replication.overlay.OverlayController.OverlayAction
import replication.overlay.OverlayController.OverlayMessage.*
import replication.overlay.{HyParViewStateMachine, OverlayController}

class HyParViewStateMachineTest extends FunSuite {

  final private case class TestConnection(name: String) extends Connection {
    override def info: ConnectionInfo                                    = ConnectionInfo("name" -> name)
    override def send(message: channels.MessageBuffer): Async[Any, Unit] = Async(())
    override def close(): Unit                                           = ()
    override def toString: String                                        = s"TestConnection($name)"
  }

  private val defaultSelf = peer("self")

  private val config = HyParViewConfig(
    activeViewSize = 2,
    passiveViewSize = 3,
    activeRandomWalkLength = 4,
    passiveRandomWalkLength = 2,
    shuffleRandomWalkLength = 3,
    shuffleActiveSample = 1,
    shufflePassiveSample = 1,
  )

  private def peer(name: String, connectable: Boolean = true): PeerConnectInfo =
    PeerConnectInfo(
      Uid.predefined(name),
      if connectable then Set(ChannelConnectInfo.QueuedLocal(name)) else Set(ChannelConnectInfo.Tcp(name, 42))
    )

  private def state(
      self: PeerConnectInfo = defaultSelf,
      random: (Int, Int) => Int = (_, _) => 0,
      canConnect: PeerConnectInfo => Boolean = _.channelConnectors.exists {
        case ChannelConnectInfo.QueuedLocal(_) => true
        case _                                 => false
      }
  ): HyParViewStateMachine =
    HyParViewStateMachine.empty(self, config, random, canConnect)

  private def withActive(machine: HyParViewStateMachine, peers: (PeerConnectInfo, Connection)*): HyParViewStateMachine =
    machine.copy(
      known = machine.known ++ peers.map((p, _) => p.uid -> p),
      active = peers.map((p, c) => ActivePeer(p, c)).toVector,
    )

  private def withPassive(machine: HyParViewStateMachine, peers: PeerConnectInfo*): HyParViewStateMachine =
    machine.copy(
      known = machine.known ++ peers.map(p => p.uid -> p),
      passive = peers.toVector,
    )

  private def sent(actions: List[OverlayAction]) = actions.collect { case OverlayAction.Send(conn, msg) => (conn, msg) }
  private def disconnects(actions: List[OverlayAction]) = actions.collect { case OverlayAction.Disconnect(conn) => conn }

  private def receive(machine: HyParViewStateMachine, msg: OverlayController.OverlayMessage, from: Connection)
      : HyParViewStateMachine.Result = {
    val (next, actions) = machine.receiveActions(msg, from)
    HyParViewStateMachine.Result(next.asInstanceOf[HyParViewStateMachine], actions)
  }

  test(
    "Join at the contact adds the newcomer to the active view and forwards ForwardJoin through existing active peers"
  ) {
    val existing  = peer("existing")
    val newcomer  = peer("newcomer")
    val existingC = TestConnection("existing")
    val joinC     = TestConnection("newcomer")

    val base = withActive(state(random = (_, _) => 0), existing -> existingC)
      .copy(known = Map(defaultSelf.uid -> defaultSelf, existing.uid -> existing, newcomer.uid -> newcomer))

    val HyParViewStateMachine.Result(next, actions) = receive(base, Join(newcomer), joinC)

    assertEquals(next.activeView, Set(existing.uid, newcomer.uid))
    assertEquals(
      sent(actions),
      List(
        (joinC, Neighbor(defaultSelf, highPriority = true)),
        (existingC, ForwardJoin(newcomer, config.activeRandomWalkLength, defaultSelf.uid))
      )
    )
  }

  test("ForwardJoin at passive random-walk length learns the peer passively and keeps forwarding") {
    val from   = peer("from")
    val nextP  = peer("next")
    val joined = peer("joined")
    val fromC  = TestConnection("from")
    val nextC  = TestConnection("next")

    val machine = withActive(state(random = (_, _) => 1), from -> fromC, nextP -> nextC)
    val HyParViewStateMachine.Result(next, actions) =
      receive(machine, ForwardJoin(joined, config.passiveRandomWalkLength, from.uid), fromC)

    assert(next.passiveView.contains(joined.uid), "peer should be inserted in passive view at PRWL")
    assertEquals(sent(actions), List((nextC, ForwardJoin(joined, config.passiveRandomWalkLength - 1, defaultSelf.uid))))
  }

  test("ForwardJoin stops at ttl zero and adds the joining peer to the active view") {
    val joined = peer("joined")
    val joinC  = TestConnection("joined")

    val HyParViewStateMachine.Result(next, actions) =
      receive(state(), ForwardJoin(joined, 0, Uid.predefined("sender")), joinC)

    assertEquals(next.activeView, Set(joined.uid))
    assertEquals(
      actions,
      List(
        OverlayAction.ActiveConnectionAdded(joined.uid),
        OverlayAction.Send(joinC, Neighbor(defaultSelf, highPriority = true))
      )
    )
  }

  test("low-priority Neighbor is only accepted when there is free active capacity") {
    val a  = peer("a")
    val b  = peer("b")
    val c  = peer("c")
    val aC = TestConnection("a")
    val bC = TestConnection("b")
    val cC = TestConnection("c")

    val full = withActive(state(), a -> aC, b -> bC)

    val HyParViewStateMachine.Result(rejected, rejectActions) = receive(full, Neighbor(c, highPriority = false), cC)
    assertEquals(rejected.activeView, Set(a.uid, b.uid))
    assertEquals(sent(rejectActions), List((cC, NeighborReply(defaultSelf.uid, accepted = false))))

    val withSpace                                             = withActive(state(), a -> aC)
    val HyParViewStateMachine.Result(accepted, acceptActions) =
      receive(withSpace, Neighbor(c, highPriority = false), cC)
    assertEquals(accepted.activeView, Set(a.uid, c.uid))
    assertEquals(sent(acceptActions), List((cC, NeighborReply(defaultSelf.uid, accepted = true))))
  }

  test("high-priority Neighbor is always accepted and may evict an existing active peer by closing the connection") {
    val a  = peer("a")
    val b  = peer("b")
    val c  = peer("c")
    val aC = TestConnection("a")
    val bC = TestConnection("b")
    val cC = TestConnection("c")

    val full                                        = withActive(state(random = (_, _) => 0), a -> aC, b -> bC)
    val HyParViewStateMachine.Result(next, actions) = receive(full, Neighbor(c, highPriority = true), cC)

    assertEquals(next.activeView, Set(b.uid, c.uid))
    assert(next.passiveView.contains(a.uid), "evicted active peer should become a passive backup")
    assertEquals(disconnects(actions), List(aC))
    assertEquals(sent(actions), List((cC, NeighborReply(defaultSelf.uid, accepted = true))))
  }

  test("removeConnection demotes an active peer to passive and triggers replacement promotion") {
    val activePeer = peer("active")
    val backup     = peer("backup")
    val activeC    = TestConnection("active")

    val machine = withPassive(withActive(state(), activePeer -> activeC), backup)
    val (next0, actions) = machine.removeConnection(activeC)
    val next             = next0.asInstanceOf[HyParViewStateMachine]

    assertEquals(next.activeView, Set.empty)
    assert(next.passiveView.contains(activePeer.uid))
    assert(next.passiveView.contains(backup.uid))
    assertEquals(
      actions,
      List(
        OverlayAction.ActiveConnectionRemoved(activePeer.uid),
        OverlayAction.SendJoin(backup.channelConnectors, backup.uid, Neighbor(defaultSelf, highPriority = true))
      )
    )
  }

  test("registerConnection attaches to existing pending peer entry instead of duplicating it") {
    val candidate = peer("candidate")
    val pendingC  = TestConnection("candidate")
    val started   = state().copy(
      known = Map(defaultSelf.uid -> defaultSelf, candidate.uid -> candidate),
      passive = Vector(candidate),
      pendingConnections = Vector(HyParViewStateMachine.PendingConnection(Some(candidate.uid), None))
    )

    val registered = started.registerConnection(pendingC, Some(candidate.uid))._1.asInstanceOf[HyParViewStateMachine]

    assertEquals(registered.pendingConnections.count(_.expectedPeer.contains(candidate.uid)), 1)
    assertEquals(registered.pendingConnections.count(pc => pc.expectedPeer.contains(candidate.uid) && pc.connection.contains(pendingC)), 1)
  }

  test("removeConnection for a failed pending promotion removes that peer from passive and clears pending state") {
    val candidate  = peer("candidate")
    val pendingC   = TestConnection("candidate")
    val base       = withPassive(state(), candidate)
    val registered = base.registerConnection(pendingC, Some(candidate.uid))._1.asInstanceOf[HyParViewStateMachine]
    val machine    = registered

    assert(machine.pendingConnections.exists(_.expectedPeer.contains(candidate.uid)))

    val (next0, actions) = machine.removeConnection(pendingC)
    val next             = next0.asInstanceOf[HyParViewStateMachine]

    assertEquals(actions, Nil)
    assert(!next.passiveView.contains(candidate.uid))
    assert(!next.pendingConnections.exists(_.expectedPeer.contains(candidate.uid)))
  }

  test("shuffle replies at the endpoint and merges connectable samples into the passive view") {
    val origin   = peer("origin")
    val activeP  = peer("active")
    val passiveP = peer("passive")
    val sampleA  = peer("sample-a")
    val sampleB  = peer("sample-b")
    val rejected = peer("rejected", connectable = false)
    val originC  = TestConnection("origin")
    val activeC  = TestConnection("active")

    val machine = withPassive(withActive(state(), origin -> originC, activeP -> activeC), passiveP)
    val HyParViewStateMachine.Result(next, actions) =
      receive(machine, Shuffle(origin, Set(sampleA, sampleB, rejected), 0, origin.uid), originC)

    assert(next.passiveView.contains(sampleA.uid))
    assert(next.passiveView.contains(sampleB.uid))
    assert(!next.passiveView.contains(rejected.uid), "undialable peers should be filtered out")
    assertEquals(sent(actions), List((originC, ShuffleReply(defaultSelf.uid, Set(passiveP)))))
  }
}
