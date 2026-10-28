package channels

import channels.connection.{ConnectionDescriptor, PeerConnectInfo}
import channels.overlay.HyParViewStateMachine.{HyParViewConfig, Link}
import channels.overlay.OverlayController.OverlayMessage.*
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayTimer}
import channels.overlay.{AttemptId, ConnId, HyParViewStateMachine}
import munit.FunSuite
import rdts.base.Uid

class HyParViewStateMachineTest extends FunSuite {

  private val self = peer("self")

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

  /** Deterministic machine; the shuffle timer counts as already running so it does not clutter the actions. */
  private def state(random: (Int, Int) => Int = (lo, _) => lo): HyParViewStateMachine =
    HyParViewStateMachine.empty(
      self,
      config,
      random,
      _.channelConnectors.exists { case ConnectionDescriptor.QueuedLocal(_) => true; case _ => false }
    ).copy(shuffleScheduled = true)

  extension (m: HyParViewStateMachine) {
    def withActive(peers: (PeerConnectInfo, ConnId)*): HyParViewStateMachine =
      m.copy(
        known = m.known ++ peers.map((p, _) => p.uid -> p),
        links = m.links ++ peers.map((p, c) => c -> Link.Active(p)),
      )
    def withPassive(peers: PeerConnectInfo*): HyParViewStateMachine =
      m.copy(known = m.known ++ peers.map(p => p.uid -> p))
    def on(event: OverlayEvent): (HyParViewStateMachine, List[OverlayAction]) =
        val (next, actions) = m.step(event)
        (next.asInstanceOf[HyParViewStateMachine], actions)
  }

  private def received(conn: ConnId, message: channels.overlay.OverlayController.OverlayMessage) =
    OverlayEvent.Received(conn, message)

  private val c1 = ConnId(1)
  private val c2 = ConnId(2)
  private val c3 = ConnId(3)

  test("bootstrap dials the contact, sends Join once connected, and accepts the contact's Neighbor") {
    val contactDescriptor = ConnectionDescriptor.QueuedLocal("contact")
    val contact           = peer("contact")

    val (dialing, dialActions) = state().on(OverlayEvent.Bootstrap(contactDescriptor))
    assertEquals(dialActions, List(OverlayAction.Dial(AttemptId(0), Set(contactDescriptor))))

    val (connected, connectedActions) = dialing.on(OverlayEvent.Established(AttemptId(0), c1))
    assertEquals(
      connectedActions,
      List(OverlayAction.Send(c1, Join(self)), OverlayAction.StartTimer(OverlayTimer.Handshake(c1)))
    )

    val (joined, joinedActions) = connected.on(received(c1, Neighbor(contact, highPriority = true)))
    assertEquals(joined.activeView, Set(contact.uid))
    assertEquals(
      joinedActions,
      List(OverlayAction.NeighborUp(contact.uid), OverlayAction.Send(c1, NeighborReply(self.uid, accepted = true)))
    )
  }

  test("Join makes the contact request the newcomer (high priority) and forwards ForwardJoin to its neighbors") {
    val a        = peer("a")
    val newcomer = peer("newcomer")

    val (pending, actions) = state().withActive(a -> c1).on(OverlayEvent.Incoming(c2)) match
        case (s, _) => s.on(received(c2, Join(newcomer)))

    assertEquals(
      actions,
      List(
        OverlayAction.Send(c2, Neighbor(self, highPriority = true)),
        OverlayAction.Send(c1, ForwardJoin(newcomer, config.activeRandomWalkLength, self.uid))
      )
    )
    assertEquals(pending.links(c2), Link.Pending(Some(newcomer), requestedByUs = true))

    val (joined, joinedActions) = pending.on(received(c2, NeighborReply(newcomer.uid, accepted = true)))
    assertEquals(joined.activeView, Set(a.uid, newcomer.uid))
    assertEquals(joinedActions, List(OverlayAction.NeighborUp(newcomer.uid)))
  }

  test("ForwardJoin at ttl zero dials the newcomer and sends a high-priority Neighbor once connected") {
    val a       = peer("a")
    val newNode = peer("new")

    val (dialing, actions) = state().withActive(a -> c1).on(received(c1, ForwardJoin(newNode, 0, a.uid)))
    assertEquals(actions, List(OverlayAction.Dial(AttemptId(0), newNode.channelConnectors)))

    val (_, connected) = dialing.on(OverlayEvent.Established(AttemptId(0), c2))
    assertEquals(
      connected,
      List(
        OverlayAction.Send(c2, Neighbor(self, highPriority = true)),
        OverlayAction.StartTimer(OverlayTimer.Handshake(c2))
      )
    )
  }

  test("ForwardJoin ending at a node that already has the newcomer active does not dial") {
    val a       = peer("a")
    val newNode = peer("new")

    val (_, actions) = state().withActive(a -> c1, newNode -> c2).on(received(c1, ForwardJoin(newNode, 0, a.uid)))
    assertEquals(actions, Nil)
  }

  test("ForwardJoin at PRWL remembers the newcomer and forwards neither to the sender nor to the newcomer") {
    val a       = peer("a")
    val b       = peer("b")
    val newNode = peer("new")

    val (next, actions) = state().withActive(a -> c1, b -> c2)
      .on(received(c1, ForwardJoin(newNode, config.passiveRandomWalkLength, sender = a.uid)))

    assert(next.passiveView.contains(newNode.uid))
    assertEquals(
      actions,
      List(OverlayAction.Send(c2, ForwardJoin(newNode, config.passiveRandomWalkLength - 1, self.uid)))
    )
  }

  test("low-priority Neighbor is refused when full and accepted with free capacity") {
    val a = peer("a")
    val b = peer("b")
    val c = peer("c")

    val (refused, refuseActions) = state().withActive(a -> c1, b -> c2).on(OverlayEvent.Incoming(c3)) match
        case (s, _) => s.on(received(c3, Neighbor(c, highPriority = false)))
    assertEquals(refused.activeView, Set(a.uid, b.uid))
    assertEquals(refuseActions, List(OverlayAction.Send(c3, NeighborReply(self.uid, accepted = false))))

    val (accepted, acceptActions) = state().withActive(a -> c1).on(OverlayEvent.Incoming(c3)) match
        case (s, _) => s.on(received(c3, Neighbor(c, highPriority = false)))
    assertEquals(accepted.activeView, Set(a.uid, c.uid))
    assertEquals(
      acceptActions,
      List(OverlayAction.NeighborUp(c.uid), OverlayAction.Send(c3, NeighborReply(self.uid, accepted = true)))
    )
  }

  test("high-priority Neighbor evicts a neighbor, which stays passive") {
    val a = peer("a")
    val b = peer("b")
    val c = peer("c")

    val (next, actions) = state().withActive(a -> c1, b -> c2).on(OverlayEvent.Incoming(c3)) match
        case (s, _) => s.on(received(c3, Neighbor(c, highPriority = true)))

    assertEquals(next.activeView, Set(b.uid, c.uid))
    assert(next.passiveView.contains(a.uid))
    assertEquals(
      actions,
      List(
        OverlayAction.Close(c1),
        OverlayAction.NeighborDown(a.uid),
        OverlayAction.NeighborUp(c.uid),
        OverlayAction.Send(c3, NeighborReply(self.uid, accepted = true))
      )
    )
  }

  test("a closed active connection reports NeighborDown and refills from the passive view") {
    val a      = peer("a")
    val b      = peer("b")
    val backup = peer("backup")

    val (next, actions) = state().withActive(a -> c1, b -> c2).withPassive(backup).on(OverlayEvent.Closed(c1))

    assertEquals(next.activeView, Set(b.uid))
    assert(next.passiveView.contains(a.uid))
    // a is still a passive candidate; the deterministic random picks the first one
    assertEquals(actions.head, OverlayAction.NeighborDown(a.uid))
    assertEquals(actions.collect { case d: OverlayAction.Dial => d }.size, 1)
  }

  test("a failed dial drops the peer from the passive view and tries the next candidate") {
    val a      = peer("a")
    val dead   = peer("dead")
    val backup = peer("backup")

    val (dialing, _) = state().withActive(a -> c1).withPassive(dead).on(OverlayEvent.Discovered(Set.empty))
    assertEquals(dialing.dialing.values.toList, List(HyParViewStateMachine.Purpose.Neighbor(dead, false)))

    val (next, actions) = dialing.withPassive(backup).on(OverlayEvent.DialFailed(AttemptId(0)))
    assert(!next.passiveView.contains(dead.uid))
    assertEquals(actions, List(OverlayAction.Dial(AttemptId(1), backup.channelConnectors)))
  }

  test("a refused Neighbor request closes the connection and skips the candidate until the next shuffle") {
    val a       = peer("a")
    val refuser = peer("refuser")

    val (dialing, _) = state().withActive(a -> c1).withPassive(refuser).on(OverlayEvent.Discovered(Set.empty))
    val (pending, _) = dialing.on(OverlayEvent.Established(AttemptId(0), c2))

    val (refused, actions) = pending.on(received(c2, NeighborReply(refuser.uid, accepted = false)))
    assertEquals(actions, List(OverlayAction.Close(c2)))
    assert(refused.passiveView.contains(refuser.uid))
    assertEquals(refused.links.get(c2), None)

    val (_, afterShuffle) = refused.on(OverlayEvent.TimerFired(OverlayTimer.Shuffle))
    assert(afterShuffle.contains(OverlayAction.Dial(AttemptId(1), refuser.channelConnectors)), afterShuffle)
  }

  test("an unfinished handshake is closed by its timer; stale timers are ignored") {
    val (pending, _) = state().on(OverlayEvent.Incoming(c1))

    val (timedOut, actions) = pending.on(OverlayEvent.TimerFired(OverlayTimer.Handshake(c1)))
    assertEquals(actions, List(OverlayAction.Close(c1)))
    assertEquals(timedOut.links, Map.empty)

    val (_, stale) = timedOut.on(OverlayEvent.TimerFired(OverlayTimer.Handshake(c1)))
    assertEquals(stale, Nil)
  }

  test("messages on unknown connections are ignored") {
    val (_, actions) = state().on(received(c1, Neighbor(peer("a"), highPriority = true)))
    assertEquals(actions, Nil)
  }

  test("the shuffle timer runs while there are neighbors and sends a shuffle through one of them") {
    val a = peer("a")

    val (started, startActions) = state().copy(shuffleScheduled = false).withActive(a -> c1)
      .on(OverlayEvent.Discovered(Set.empty))
    assertEquals(startActions, List(OverlayAction.StartTimer(OverlayTimer.Shuffle)))

    val (_, fired) = started.on(OverlayEvent.TimerFired(OverlayTimer.Shuffle))
    assertEquals(
      fired,
      List(
        OverlayAction.Send(c1, Shuffle(self, Set(self, a), config.shuffleRandomWalkLength, self.uid)),
        OverlayAction.StartTimer(OverlayTimer.Shuffle)
      )
    )
  }

  test("shuffle endpoint replies on the active connection and merges connectable samples") {
    val origin   = peer("origin")
    val activeP  = peer("active")
    val passiveP = peer("passive")
    val sampleA  = peer("sample-a")
    val sampleB  = peer("sample-b")
    val rejected = peer("rejected", connectable = false)

    val (next, actions) = state().withActive(origin -> c1, activeP -> c2).withPassive(passiveP)
      .on(received(c1, Shuffle(origin, Set(sampleA, sampleB, rejected), 0, origin.uid)))

    assert(next.passiveView.contains(sampleA.uid))
    assert(next.passiveView.contains(sampleB.uid))
    assert(!next.passiveView.contains(rejected.uid), "undialable peers should be filtered out")
    assertEquals(
      actions,
      List(OverlayAction.Send(
        c1,
        ShuffleReply(self.uid, Set(passiveP, sampleA, sampleB), Set(sampleA, sampleB, rejected))
      ))
    )
  }

  test("shuffle endpoint replies over a temporary connection if the origin is not a neighbor") {
    val origin = peer("origin")
    val hop    = peer("hop")
    val other  = peer("other")

    // the active view is full, so learning about origin does not trigger a refill dial
    val (dialing, actions) = state().withActive(hop -> c1, other -> c3)
      .on(received(c1, Shuffle(origin, Set(origin), 0, hop.uid)))
    assertEquals(actions, List(OverlayAction.Dial(AttemptId(0), origin.channelConnectors)))

    val (done, sendClose) = dialing.on(OverlayEvent.Established(AttemptId(0), c2))
    sendClose match
        case List(OverlayAction.Send(`c2`, ShuffleReply(from, _, answered)), OverlayAction.Close(`c2`)) =>
          assertEquals(from, self.uid)
          assertEquals(answered, Set(origin))
        case other => fail(s"expected Send(ShuffleReply) and Close, got $other")
    assertEquals(done.activeView, Set(hop.uid, other.uid))
  }

  test("simultaneous Neighbor requests keep the connection dialed by the smaller id") {
    val (aSelf, bSelf)                                       = (peer("a"), peer("b"))
    def machine(me: PeerConnectInfo, other: PeerConnectInfo) =
      HyParViewStateMachine.empty(me, config, (lo, _) => lo, _ => true).copy(shuffleScheduled = true)
        .withPassive(other).on(OverlayEvent.Discovered(Set.empty))._1
        .on(OverlayEvent.Established(AttemptId(0), c1))._1 // own outgoing request on c1
        .on(OverlayEvent.Incoming(c2))._1                  // the other's request arrives on c2

    // a < b: a keeps its own connection and refuses b's
    val (_, aActions) = machine(aSelf, bSelf).on(received(c2, Neighbor(bSelf, highPriority = true)))
    assertEquals(aActions, List(OverlayAction.Send(c2, NeighborReply(aSelf.uid, accepted = false))))

    // b > a: b accepts a's connection
    val (bNext, bActions) = machine(bSelf, aSelf).on(received(c2, Neighbor(aSelf, highPriority = true)))
    assertEquals(bNext.connectionFor(aSelf.uid), Some(c2))
    assert(bActions.contains(OverlayAction.Send(c2, NeighborReply(bSelf.uid, accepted = true))), bActions)
  }
}
