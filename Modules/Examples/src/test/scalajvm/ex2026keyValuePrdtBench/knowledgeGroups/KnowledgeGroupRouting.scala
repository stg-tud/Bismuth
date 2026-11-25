package test.rdts.protocols.knowledgeGroups

import channels.routing.PrdtSystem
import ex2026keyValuePrdtBench.KnowledgeGroups.*
import ex2026keyValuePrdtBench.{MultiPaxosReplica, Request}
import rdts.base.Lattice.syntax.merge
import rdts.base.{ReplicaId, Uid}
import rdts.protocols.Participants
import rdts.protocols.knowledgeGroups.MultiPaxos
import channels.connection.LocalMessageQueue

/** Characterization tests of the current knowledge group routing ([[channels.routing.PrdtSystem.matches]]) and of
  * [[ex2026keyValuePrdtBench.MultiPaxosReplica]]. They pin the existing behaviour, so a reimplementation can be checked
  * against it.
  */
object KnowledgeGroupFixtures {
  type MP = MultiPaxos[Request]

  val allIds: Set[Uid]    = Set(client, leader, follower1, follower2, follower3, follower4, proxy1, proxy2)
  val followers: Set[Uid] = Set(follower1, follower2, follower3, follower4)
  val acceptors: Set[Uid] = followers + leader
  given Participants      = Participants(acceptors)
  val req0: Request       = Request(Uid.predefined("r0"), "0")
  val req1: Request       = Request(Uid.predefined("r1"), "1")

  /** Everyone `sender` would send `delta` to (a replica is never its own receiver). */
  def receivers(system: PrdtSystem[MP], sender: Uid, delta: MP): Set[Uid] =
      given ReplicaId = ReplicaId(sender)
      (allIds - sender).filter(r => system.matches(delta, r))

  def as[T](id: Uid)(f: ReplicaId ?=> T): T = f(using ReplicaId(id))

  /** State after `leader` was elected in slot `index` (all acceptors voted). */
  def elected(base: MP = MultiPaxos(), index: Long = 0): MP = {
    val phase1a = as(leader)(base.startLeaderElection(index))
    val s1      = base.merge(phase1a)
    acceptors.foldLeft(s1)((s, id) => s.merge(as(id)(s.upkeep)))
  }

  /** Leader election delta for slot `index` (only the phase1a). */
  def electionDelta(base: MP = MultiPaxos(), index: Long = 0): MP = as(leader)(base.startLeaderElection(index))

  /** The leader's phase2a for `value` in slot `index`, as a delta. */
  def proposalDelta(state: MP, index: Long, value: Request): MP = as(leader)(state.proposeIfLeader(index, value))

  def decided(state: MP, index: Long, value: Request): MP = {
    val s = state.merge(proposalDelta(state, index, value))
    acceptors.foldLeft(s)((s, id) => s.merge(as(id)(s.upkeep)))
  }

  /** Replicas connected via local queued connections, full mesh between replicas sharing a group. */
  class Cluster(system: PrdtSystem[MP], ids: List[Uid], participants: Set[Uid]) {
    val replicas: Map[Uid, MultiPaxosReplica] = ids.map(i =>
      i -> MultiPaxosReplica(id = i, participants = participants, systemConfig = system, state = MultiPaxos())
    ).toMap

    val queue: LocalMessageQueue          = channels.connection.LocalMessageQueue()
    def deliver(): Unit                   = while queue.nonEmpty do queue.deliverAll(): Unit
    def apply(id: Uid): MultiPaxosReplica = replicas(id)

    /** Full mesh between all replicas that share at least one group. */
    val sharing: Set[(Uid, Uid)] =
      for
          g <- system.knowledgeGroups
          a <- g.ids
          b <- g.ids
          if a.toString < b.toString && replicas.contains(a) && replicas.contains(b)
      yield (a, b)

    sharing.foreach { (a, b) =>
      val connection = channels.connection.QueuedLocalConnection(s"$a-$b", queue)
      replicas(a).io.addServerConnection(connection.server)
      replicas(b).io.addClientConnection(connection.client(b.toString))
    }

    def settle(): Unit = deliver()

    settle()

    def reads(id: Uid): Seq[String] = replicas(id).state.read.map(_.payload)
  }

}

class KnowledgeGroupRouting extends munit.FunSuite {
  import KnowledgeGroupFixtures.{*, given}

  val empty: MP = MultiPaxos()

  test("fixtures: election, proposal and decision behave as expected") {
    val e = elected()
    assert(e.slots(0).currentRound.isDefined)
    assert(!e.slots(0).currentRoundHasProposal)
    val p = proposalDelta(e, 0, req0)
    assert(p.slots(0).currentRoundHasProposal, p)
    assertEquals(decided(e, 0, req0).log, Map(0L -> req0))
  }

  // ---------------------------------------------------------------------------------------------
  // generic edge cases
  // ---------------------------------------------------------------------------------------------

  test("empty delta is never sent") {
    for system <- List(clientServer, compartmentalized, occamsRazor); sender <- allIds do
        assertEquals(receivers(system, sender, empty), Set.empty[Uid], s"$sender")
  }

  test("replicas outside of all groups never send") {
    val delta = as(client)(empty.request(req0)).merge(elected()).merge(MultiPaxos(log = Map(0L -> req0)))
    assertEquals(receivers(clientServer, proxy1, delta), Set.empty[Uid])
    assertEquals(receivers(occamsRazor, proxy2, delta), Set.empty[Uid])
  }

  test("receivers outside of a shared group are not addressed, even if another group would match") {
    // follower1 is not in the requests group, so the client can not send requests to it
    val request = as(client)(empty.request(req0))
    assert(!as(client)(clientServer.matches(request, follower1)))
    // a follower can not send requests to anyone
    assertEquals(receivers(clientServer, follower1, as(follower1)(empty.request(req0))), Set.empty[Uid])
  }

  test("matches with a receiver set requires exactly that group") {
    given ReplicaId = ReplicaId(client)
    val request     = empty.request(req0)
    assert(clientServer.matches(request, Set(client, leader)))
    assert(!clientServer.matches(request, Set(leader)))
    assert(!clientServer.matches(request, Set(client, leader, follower1)))
  }

  // ---------------------------------------------------------------------------------------------
  // client server
  // ---------------------------------------------------------------------------------------------

  test("clientServer: routing table") {
    val e = elected()
    assertEquals(receivers(clientServer, client, as(client)(empty.request(req0))), Set(leader))
    assertEquals(receivers(clientServer, leader, electionDelta()), followers)
    assertEquals(receivers(clientServer, follower1, e), Set(leader, follower2, follower3, follower4))
    assertEquals(receivers(clientServer, leader, proposalDelta(e, 0, req0)), followers)
    assertEquals(receivers(clientServer, leader, MultiPaxos(log = Map(0L -> req0))), Set(client))
    // client is not part of the slots group
    assertEquals(receivers(clientServer, client, e), Set.empty[Uid])
    // mixed delta goes to the union
    val mixed = e.merge(MultiPaxos(log = Map(0L -> req0)))
    assertEquals(receivers(clientServer, leader, mixed), followers + client)
  }

  // ---------------------------------------------------------------------------------------------
  // compartmentalized
  // ---------------------------------------------------------------------------------------------

  test("compartmentalized: even slots go through proxy2, odd slots through proxy1") {
    val e0 = elected()
    val e1 = decided(e0, 0, req0)
    assertEquals(receivers(compartmentalized, leader, electionDelta()), Set(proxy2))
    assertEquals(receivers(compartmentalized, leader, electionDelta(e1, 1)), Set(proxy1))
    assertEquals(receivers(compartmentalized, leader, proposalDelta(e0, 0, req0)), Set(proxy2))
  }

  test("compartmentalized: proxies forward to their followers (and back to the leader)") {
    val d0 = electionDelta()
    assertEquals(receivers(compartmentalized, proxy2, d0), Set(leader, follower2, follower4))
    assertEquals(receivers(compartmentalized, proxy1, d0), Set.empty[Uid])
    val d1 = electionDelta(decided(elected(), 0, req0), 1)
    assertEquals(receivers(compartmentalized, proxy1, d1), Set(leader, follower1, follower3))
  }

  test("compartmentalized: follower votes go back to their proxy only") {
    val e     = elected()
    val vote2 = as(follower2)(e.merge(electionDelta()).upkeep)
    assertEquals(receivers(compartmentalized, follower2, vote2), Set(proxy2, follower4))
    // follower1 is in the odd group, an even-slot vote is not routed unless the log is non-empty
    val vote1 = as(follower1)(e.merge(electionDelta()).upkeep)
    assertEquals(receivers(compartmentalized, follower1, vote1), Set.empty[Uid])
  }

  test("compartmentalized: a vote covering slots of both parities is routed by the parity of its first slot") {
    // follower2 holds open slots 0 to 5, the leader starts a new election in slot 0. The vote also covers the open
    // slots above 0 (SlotElections.views). Routing only looks at the parity of `slots.headOption`, whose position
    // depends on the iteration order of the map, not on the slot of the election.
    val e   = elected()
    val s   = (0L until 6L).foldLeft(e)((s, i) => s.merge(proposalDelta(s, i, Request(Uid.predefined(s"r$i"), s"$i"))))
    val acc = s.merge(as(follower2)(s.upkeep))
    val st0   = acc.merge(as(leader)(acc.startLeaderElection(0)))
    val vote0 = as(follower2)(st0.upkeep)
    assertEquals(vote0.slots.keySet, Set(0L, 1L, 2L, 3L, 4L, 5L))
    assert(vote0.slots.forall((_, p) => !p.currentRoundHasProposal))
    assertEquals(vote0.slots.head._1, 0L) // happens to be the election slot
    assertEquals(receivers(compartmentalized, follower2, vote0), Set(proxy2, follower4))
    assertEquals(receivers(compartmentalized, proxy2, vote0), Set(leader, follower2, follower4))
  }

  /** Known bug (liveness). The leader has proposed values for slots 0 to 6, none is decided. It starts a new election
    * in slot 2, which goes to the even compartment (proxy2, follower2, follower4). A phase-1b vote for slot 2 also
    * covers every open slot above 2, so it holds slots 2 to 6. The routing picks the compartment by the parity of the
    * first slot in the map, which is slot 5 here (HashMap order). follower2 and follower4 are only in the even
    * compartment, so their votes match no group and are sent to no one. The odd followers never see the election, so
    * the leader gets no quorum and the election never completes.
    *
    * The setup is checked by the precondition test, the bug test is marked `.fail`. When the routing is fixed (e.g.
    * route by the lowest slot), munit reports that the `.fail` test passed; then remove `.fail`.
    */
  object evenVoteHeadedByOddSlot {
    val proposed: MP = (0L until 7L).foldLeft(elected()) { (state, i) =>
      state.merge(proposalDelta(state, i, Request(Uid.predefined(s"r$i"), s"$i")))
    }
    val election: MP = as(leader)(proposed.startLeaderElection(2))

    /** The phase-1b vote of `follower` in the new election, after it accepted the proposals (phase 2b). */
    def vote(follower: Uid): MP =
        val accepted = proposed.merge(as(follower)(proposed.upkeep))
        as(follower)(accepted.merge(election).upkeep)

    val voters: List[Uid] = List(follower2, follower4)
  }

  test("compartmentalized: precondition, an even-slot vote spanning both parities is headed by an odd slot") {
    import evenVoteHeadedByOddSlot.*
    assertEquals(receivers(compartmentalized, leader, election), Set(proxy2)) // the election itself is routed fine
    for follower <- voters do
        val v = vote(follower)
        assertEquals(v.slots.keySet, Set(2L, 3L, 4L, 5L, 6L), s"vote of $follower")
        val firstSlot = v.slots.head._1
        assert(firstSlot % 2 == 1, s"the vote of $follower starts with slot $firstSlot, the test needs an odd one")
  }

  test("compartmentalized: an even-slot vote headed by an odd slot reaches the even compartment".fail) {
    import evenVoteHeadedByOddSlot.*
    for follower <- voters do
        assertEquals(
          receivers(compartmentalized, follower, vote(follower)),
          Set(proxy2, follower2, follower4) - follower,
          s"vote of $follower"
        )
  }

  test("compartmentalized: requests only flow before any slot exists") {
    val request = as(client)(empty.request(req0))
    assertEquals(receivers(compartmentalized, client, request), Set(leader))
    assertEquals(receivers(compartmentalized, client, request.merge(electionDelta())), Set.empty[Uid])
  }

  test("compartmentalized: log goes from proxies to the client, never directly from the leader") {
    val log = MultiPaxos[Request](log = Map(0L -> req0))
    assertEquals(receivers(compartmentalized, proxy1, log), Set(client, proxy2))
    assertEquals(receivers(compartmentalized, leader, log), Set.empty[Uid])
  }

  // ---------------------------------------------------------------------------------------------
  // occams razor
  // ---------------------------------------------------------------------------------------------

  test("occamsRazor: election to followers, proposals to everyone including client") {
    val e = elected()
    assertEquals(receivers(occamsRazor, leader, electionDelta()), followers)
    assertEquals(receivers(occamsRazor, leader, proposalDelta(e, 0, req0)), followers + client)
    assertEquals(receivers(occamsRazor, client, as(client)(empty.request(req0))), Set(leader))
  }

  test("occamsRazor: the log itself is never sent, clients learn it from the votes") {
    assertEquals(receivers(occamsRazor, leader, MultiPaxos(log = Map(0L -> req0))), Set.empty[Uid])
  }

  test("occamsRazor: client relays proposal votes to all acceptors (source of the forwarding loop)") {
    val p = elected().merge(proposalDelta(elected(), 0, req0))
    assertEquals(receivers(occamsRazor, client, p), acceptors)
  }

  test("occamsRazor: a delta mixing open and proposed slots goes to the acceptors, not the client") {
    val e0    = decided(elected(), 0, req0)
    val open1 = electionDelta(e0, 1)
    val mixed = MultiPaxos[Request](slots = e0.slots).merge(open1)
    assertEquals(receivers(occamsRazor, leader, mixed), followers)
  }

  test("occamsRazor: a vote that reports a decided slot above still reaches the acceptors") {
    // follower1 accepts slots 0 and 1; slot 1 is decided, slot 0 is not
    val e  = elected()
    val s0 = e.merge(proposalDelta(e, 0, req0))
    val s1 = s0.merge(proposalDelta(s0, 1, req1))
    val a1 = s1.merge(as(follower1)(s1.upkeep))
    val a2 = (acceptors - follower1).foldLeft(a1) { (s, id) =>
      val d = as(id)(s.upkeep)
      s.merge(MultiPaxos[Request](slots = d.slots.filter(_._1 == 1L)))
    }
    val decided1 = a2.merge(as(follower1)(a2.upkeep))
    assertEquals(decided1.log.keySet, Set(1L))
    // follower2 starts a new election in slot 0; follower1's vote reports slot 1
    val st   = decided1.merge(as(follower2)(decided1.startLeaderElection(0)))
    val vote = as(follower1)(st.upkeep)
    assertEquals(vote.slots.keySet, Set(0L, 1L))
    assert(!vote.slots(0).currentRoundHasProposal)
    assert(vote.slots(1).currentRoundHasProposal)
    assertEquals(receivers(occamsRazor, follower1, vote), acceptors - follower1)
  }
}

/** End-to-end runs of [[ex2026keyValuePrdtBench.MultiPaxosReplica]] over local queued connections. */
class KnowledgeGroupReplicas extends munit.FunSuite {
  import KnowledgeGroupFixtures.*

  val compartmentIds: List[Uid] = List(leader, proxy1, proxy2, client, follower1, follower2, follower3, follower4)

  test("every replica is connected to all replicas it shares a group with") {
    val c                  = Cluster(compartmentalized, compartmentIds, acceptors)
    def neighbors(id: Uid) = c(id).io.state.peers.map(_.uid)
    assertEquals(neighbors(leader), Set(client, proxy1, proxy2))
    assertEquals(neighbors(follower1), Set(proxy1, follower3))
    assertEquals(neighbors(client), Set(leader, proxy1, proxy2))
  }

  test("compartmentalized: request without leader election is not decided") {
    val c = Cluster(compartmentalized, compartmentIds, acceptors)
    c(client).request("0")
    c.settle()
    assertEquals(c.reads(client), Seq.empty)
    assert(c(leader).state.slots.isEmpty)
  }

  test("compartmentalized: requests are decided in order and the client learns the log") {
    val c = Cluster(compartmentalized, compartmentIds, acceptors)
    c(leader).startLeaderElection()
    c.settle()
    for i <- 0 until 4 do
        c(client).request(i.toString)
        c.settle()
    assertEquals(c.reads(client), Seq("0", "1", "2", "3"))
    // all requests were taken up by the leader
    assert(c(leader).state.requests.elements.isEmpty)
  }

  test("compartmentalized: followers only see slots of their parity") {
    val c = Cluster(compartmentalized, compartmentIds, acceptors)
    c(leader).startLeaderElection()
    c.settle()
    c(client).request("0")
    c.settle()
    c(client).request("1")
    c.settle()
    assert(c(follower2).state.slots.keySet.contains(0L), c(follower2).state.slots.keySet)
    assert(c(follower1).state.slots.keySet.contains(1L), c(follower1).state.slots.keySet)
  }

  test("compartmentalized: requestWithResult completes once the client learns the decision") {
    val c = Cluster(compartmentalized, compartmentIds, acceptors)
    c(leader).startLeaderElection()
    c.settle()
    val f = c(client).requestWithResult(req0.id, req0.payload)
    c.settle()
    assertEquals(f.value.flatMap(_.toOption), Some("0"))
  }

  test("replicas outside of every group have no neighbors and ignore local requests") {
    val c = Cluster(clientServer, List(leader, client, proxy1) ++ followers.toList, acceptors)
    assert(c(proxy1).io.state.peers.isEmpty)
    c(proxy1).request("x")
    c.settle()
    assert(c(leader).state.requests.elements.isEmpty)
  }
}
