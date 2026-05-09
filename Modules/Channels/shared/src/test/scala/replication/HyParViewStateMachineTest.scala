package replication

import channels.{ConnectionDescriptor, Connection, ConnectionInfo, PeerConnectInfo}
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
      if connectable then Set(ConnectionDescriptor.QueuedLocal(name)) else Set(ConnectionDescriptor.Tcp(name, 42))
    )

  private def state(
      self: PeerConnectInfo = defaultSelf,
      random: (Int, Int) => Int = (_, _) => 0,
      canConnect: PeerConnectInfo => Boolean = _.channelConnectors.exists {
        case ConnectionDescriptor.QueuedLocal(_) => true
        case _                                   => false
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
    )

  private def sent(actions: List[OverlayAction]) = actions.collect { case OverlayAction.Send(conn, msg) => (conn, msg) }
  private def sentJoin(actions: List[OverlayAction]) =
    actions.collect { case OverlayAction.SendJoin(details, expectedPeer, msg) => (details, expectedPeer, msg) }
  private def disconnects(actions: List[OverlayAction]) = actions.collect { case OverlayAction.Disconnect(conn) =>
    conn
  }

  private def receive(machine: HyParViewStateMachine, msg: OverlayController.OverlayMessage, from: Connection)
      : HyParViewStateMachine.Result = {
    val (next, actions) = machine.receiveActions(msg, from)
    HyParViewStateMachine.Result(next.asInstanceOf[HyParViewStateMachine], actions)
  }

  test("join(contact) emits a Join message to the contact") {
    val contact = peer("contact")

    val (next0, actions) = state().join(contact)
    val next             = next0.asInstanceOf[HyParViewStateMachine]

    assert(next.passiveView.contains(contact.uid))
    assertEquals(
      sentJoin(actions),
      List((contact.channelConnectors, contact.uid, Join(defaultSelf)))
    )
  }

  test("Join asks the newcomer to establish the contact edge and forwards ForwardJoin to existing active peers") {
    val a         = peer("a")
    val newcomer  = peer("newcomer")
    val aC        = TestConnection("a")
    val newcomerC = TestConnection("newcomer")

    val machine                                     = withActive(state(), a -> aC)
    val HyParViewStateMachine.Result(next, actions) = receive(machine, Join(newcomer), newcomerC)

    assertEquals(next.activeView, Set(a.uid))
    assert(next.passiveView.contains(newcomer.uid))
    assertEquals(
      sent(actions),
      List(
        (newcomerC, Neighbor(defaultSelf, false)),
        (aC, ForwardJoin(newcomer, config.activeRandomWalkLength, defaultSelf.uid))
      )
    )
  }

  test("ForwardJoin at ttl zero initiates direct Neighbor establishment with the newcomer") {
    val newNode = peer("new")
    val hopC    = TestConnection("hop")

    val HyParViewStateMachine.Result(next, actions) =
      receive(state(), ForwardJoin(newNode, 0, peer("sender").uid), hopC)

    assert(next.passiveView.contains(newNode.uid))
    assertEquals(
      sentJoin(actions),
      List((newNode.channelConnectors, newNode.uid, Neighbor(defaultSelf, highPriority = true)))
    )
  }

  test("ForwardJoin at PRWL remembers the newcomer passively and forwards to another active peer") {
    val a       = peer("a")
    val b       = peer("b")
    val newNode = peer("new")
    val aC      = TestConnection("a")
    val bC      = TestConnection("b")

    val machine                                     = withActive(state(), a -> aC, b -> bC)
    val HyParViewStateMachine.Result(next, actions) =
      receive(machine, ForwardJoin(newNode, config.passiveRandomWalkLength, sender = a.uid), aC)

    assert(next.passiveView.contains(newNode.uid))
    assertEquals(sent(actions), List((bC, ForwardJoin(newNode, config.passiveRandomWalkLength - 1, defaultSelf.uid))))
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

    val machine          = withPassive(withActive(state(), activePeer -> activeC), backup)
    val (next0, actions) = machine.removeConnection(activeC)
    val next             = next0.asInstanceOf[HyParViewStateMachine]

    assertEquals(next.activeView, Set.empty)
    assert(next.passiveView.contains(activePeer.uid))
    assert(next.passiveView.contains(backup.uid))
    assertEquals(
      actions,
      List(
        OverlayAction.ActiveConnectionRemoved(activePeer.uid),
        OverlayAction.SendJoin(activePeer.channelConnectors, activePeer.uid, Neighbor(defaultSelf, highPriority = true))
      )
    )
  }

  test("removeConnection for a failed pending promotion removes that peer from passive and clears pending state") {
    val candidate   = peer("candidate")
    val pendingC    = TestConnection("candidate")
    val connectInfo = candidate.channelConnectors.head
    val machine     = withPassive(state(), candidate).copy(
      pendingConnections = Vector(HyParViewStateMachine.PendingConnection(candidate))
    )

    assert(machine.pendingConnections.exists(_.peer.uid == candidate.uid))

    val (next0, actions) = machine.removeConnection(pendingC, Some(connectInfo))
    val next             = next0.asInstanceOf[HyParViewStateMachine]

    assertEquals(actions, Nil)
    assert(!next.passiveView.contains(candidate.uid))
    assert(!next.pendingConnections.exists(_.peer.uid == candidate.uid))
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
    assertEquals(
      sent(actions),
      List((originC, ShuffleReply(defaultSelf.uid, Set(passiveP, sampleA, sampleB))))
    )
  }
}
