package test.rdts.protocols

import rdts.base.Lattice.syntax
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.protocols.Util.Agreement
import rdts.protocols.spanner.{FlexibleVoting, SimpSpan, twoPCMessages}
import rdts.protocols.*

/** Tests for SimpSpan with three partitions and three replicas per partition.
  *
  * Naming convention:
  *   - p1, p2, p3 — partition UIDs
  *   - id1a/id1b/id1c — the three replicas of p1 (id1a is elected leader)
  *   - id2a/id2b/id2c — the three replicas of p2 (id2a is elected leader)
  *   - id3a/id3b/id3c — the three replicas of p3 (id3a is elected leader)
  */
class ThreePartitionSimpSpanTest extends munit.FunSuite {

  // ============================================================
  // Helpers (mirror those in SimpSpanTest)
  // ============================================================

  /** Elect `leader` as leader of a fresh MultiPaxos for the given members. */
  def electLeader(members: Seq[LocalUid], leader: LocalUid): MultiPaxos[twoPCMessages] = {
    given Participants = Participants(members.map(_.uid).toSet)
    var paxos          = MultiPaxos[twoPCMessages]()
    paxos = paxos.merge(paxos.startLeaderElection(using leader))
    members.filterNot(_ == leader).foreach { m => paxos = paxos.merge(paxos.upkeep(using m)) }
    paxos = paxos.merge(paxos.upkeep(using leader))
    paxos
  }

  /** Run one full round of upkeep for all replicas in every partition. */
  def runUpkeepRound(state: SimpSpan[String], allReplicas: Seq[LocalUid]): SimpSpan[String] =
    allReplicas.foldLeft(state) { (s, m) => s.merge(s.upkeep(using m)) }

  /** Build the standard three-partition base state with elected leaders. */
  def buildThreePartitionState(): (
      SimpSpan[String],
      Uid,
      Uid,
      Uid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid,
      LocalUid
  ) = {
    val id1a = LocalUid.gen(); val id1b = LocalUid.gen(); val id1c = LocalUid.gen()
    val id2a = LocalUid.gen(); val id2b = LocalUid.gen(); val id2c = LocalUid.gen()
    val id3a = LocalUid.gen(); val id3b = LocalUid.gen(); val id3c = LocalUid.gen()
    val p1   = Uid.gen(); val p2        = Uid.gen(); val p3        = Uid.gen()

    val paxos1 = electLeader(Seq(id1a, id1b, id1c), id1a)
    val paxos2 = electLeader(Seq(id2a, id2b, id2c), id2a)
    val paxos3 = electLeader(Seq(id3a, id3b, id3c), id3a)

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> paxos1, p2 -> paxos2, p3 -> paxos3),
      partitionMembers = Map(
        p1 -> Set(id1a.uid, id1b.uid, id1c.uid),
        p2 -> Set(id2a.uid, id2b.uid, id2c.uid),
        p3 -> Set(id3a.uid, id3b.uid, id3c.uid)
      )
    )
    (state, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)
  }

  // ============================================================
  // localPartitionId
  // ============================================================

  test("localPartitionId identifies all nine replicas across three partitions") {
    val id1a = LocalUid.gen(); val id1b = LocalUid.gen(); val id1c = LocalUid.gen()
    val id2a = LocalUid.gen(); val id2b = LocalUid.gen(); val id2c = LocalUid.gen()
    val id3a = LocalUid.gen(); val id3b = LocalUid.gen(); val id3c = LocalUid.gen()
    val p1   = Uid.gen(); val p2        = Uid.gen(); val p3        = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(
        p1 -> Set(id1a.uid, id1b.uid, id1c.uid),
        p2 -> Set(id2a.uid, id2b.uid, id2c.uid),
        p3 -> Set(id3a.uid, id3b.uid, id3c.uid)
      )
    )

    // p1 replicas
    assertEquals(state.localPartitionId(using id1a), Some(p1))
    assertEquals(state.localPartitionId(using id1b), Some(p1))
    assertEquals(state.localPartitionId(using id1c), Some(p1))
    // p2 replicas
    assertEquals(state.localPartitionId(using id2a), Some(p2))
    assertEquals(state.localPartitionId(using id2b), Some(p2))
    assertEquals(state.localPartitionId(using id2c), Some(p2))
    // p3 replicas
    assertEquals(state.localPartitionId(using id3a), Some(p3))
    assertEquals(state.localPartitionId(using id3b), Some(p3))
    assertEquals(state.localPartitionId(using id3c), Some(p3))
  }

  test("localPartitionId returns None for an outsider when three partitions exist") {
    val id1a     = LocalUid.gen()
    val outsider = LocalUid.gen()
    val p1       = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(
        p1 -> Set(id1a.uid),
        p2 -> Set(LocalUid.gen().uid),
        p3 -> Set(LocalUid.gen().uid)
      )
    )

    assertEquals(state.localPartitionId(using outsider), None)
  }

  // ============================================================
  // Independent leader elections
  // ============================================================

  test("each partition can elect its own leader independently") {
    val id1a = LocalUid.gen(); val id1b = LocalUid.gen(); val id1c = LocalUid.gen()
    val id2a = LocalUid.gen(); val id2b = LocalUid.gen(); val id2c = LocalUid.gen()
    val id3a = LocalUid.gen(); val id3b = LocalUid.gen(); val id3c = LocalUid.gen()
    val p1   = Uid.gen(); val p2        = Uid.gen(); val p3        = Uid.gen()

    val paxos1 = electLeader(Seq(id1a, id1b, id1c), id1a)
    val paxos2 = electLeader(Seq(id2a, id2b, id2c), id2b) // different member is leader
    val paxos3 = electLeader(Seq(id3a, id3b, id3c), id3c) // yet another member

    given p1Participants: Participants = Participants(Set(id1a.uid, id1b.uid, id1c.uid))
    given p2Participants: Participants = Participants(Set(id2a.uid, id2b.uid, id2c.uid))
    given p3Participants: Participants = Participants(Set(id3a.uid, id3b.uid, id3c.uid))

    // Each partition has its own independent leader
    assertEquals(paxos1.leader(using p1Participants), Some(id1a.uid))
    assertEquals(paxos2.leader(using p2Participants), Some(id2b.uid))
    assertEquals(paxos3.leader(using p3Participants), Some(id3c.uid))

    // No cross-partition leader confusion: p1's paxos doesn't know about p2's members
    val _ = SimpSpan[String](
      paxosPartitions = Map(p1 -> paxos1, p2 -> paxos2, p3 -> paxos3),
      partitionMembers = Map(
        p1 -> Set(id1a.uid, id1b.uid, id1c.uid),
        p2 -> Set(id2a.uid, id2b.uid, id2c.uid),
        p3 -> Set(id3a.uid, id3b.uid, id3c.uid)
      )
    )
    // p2's leader is id2b.uid, which is unrelated to p1 or p3
    assertNotEquals(paxos2.leader(using p2Participants), Some(id1a.uid))
    assertNotEquals(paxos2.leader(using p2Participants), Some(id3c.uid))
  }

  test("upkeep drives leader election across all three partitions simultaneously via SimpSpan") {
    val id1a = LocalUid.gen(); val id1b = LocalUid.gen(); val id1c = LocalUid.gen()
    val id2a = LocalUid.gen(); val id2b = LocalUid.gen(); val id2c = LocalUid.gen()
    val id3a = LocalUid.gen(); val id3b = LocalUid.gen(); val id3c = LocalUid.gen()
    val p1   = Uid.gen(); val p2        = Uid.gen(); val p3        = Uid.gen()

    // Kick off leader elections by merging startLeaderElection deltas
    val rawPaxos1 = MultiPaxos[twoPCMessages]()
    val rawPaxos2 = MultiPaxos[twoPCMessages]()
    val rawPaxos3 = MultiPaxos[twoPCMessages]()

    val electionDelta1 = rawPaxos1.startLeaderElection(using id1a)
    val electionDelta2 = rawPaxos2.startLeaderElection(using id2a)
    val electionDelta3 = rawPaxos3.startLeaderElection(using id3a)

    var state = SimpSpan[String](
      paxosPartitions = Map(
        p1 -> rawPaxos1.merge(electionDelta1),
        p2 -> rawPaxos2.merge(electionDelta2),
        p3 -> rawPaxos3.merge(electionDelta3)
      ),
      partitionMembers = Map(
        p1 -> Set(id1a.uid, id1b.uid, id1c.uid),
        p2 -> Set(id2a.uid, id2b.uid, id2c.uid),
        p3 -> Set(id3a.uid, id3b.uid, id3c.uid)
      )
    )

    // Followers vote; leaders conclude
    val allReplicas = Seq(id1b, id1c, id2b, id2c, id3b, id3c, id1a, id2a, id3a)
    state = runUpkeepRound(state, allReplicas)

    given Participants = Participants(Set(id1a.uid, id1b.uid, id1c.uid))
    assertEquals(state.paxosPartitions(p1).leader, Some(id1a.uid))
    assertEquals(
      state.paxosPartitions(p2).leader(using Participants(Set(id2a.uid, id2b.uid, id2c.uid))),
      Some(id2a.uid)
    )
    assertEquals(
      state.paxosPartitions(p3).leader(using Participants(Set(id3a.uid, id3b.uid, id3c.uid))),
      Some(id3a.uid)
    )
  }

  // ============================================================
  // upkeep isolation
  // ============================================================

  test("upkeep for p1 replicas does not mutate p2 or p3 paxos") {
    val (state, p1, p2, p3, id1a, id1b, id1c, _, _, _, _, _, _) = buildThreePartitionState()

    val before2 = state.paxosPartitions(p2)
    val before3 = state.paxosPartitions(p3)

    var s = state
    s = s.merge(s.upkeep(using id1a))
    s = s.merge(s.upkeep(using id1b))
    s = s.merge(s.upkeep(using id1c))

    // p2 and p3 paxos must not have changed
    assertEquals(s.paxosPartitions(p2), before2)
    assertEquals(s.paxosPartitions(p3), before3)
  }

  test("upkeep for p2 replicas does not mutate p1 or p3 paxos") {
    val (state, p1, p2, p3, _, _, _, id2a, id2b, id2c, _, _, _) = buildThreePartitionState()

    val before1 = state.paxosPartitions(p1)
    val before3 = state.paxosPartitions(p3)

    var s = state
    s = s.merge(s.upkeep(using id2a))
    s = s.merge(s.upkeep(using id2b))
    s = s.merge(s.upkeep(using id2c))

    assertEquals(s.paxosPartitions(p1), before1)
    assertEquals(s.paxosPartitions(p3), before3)
  }

  // ============================================================
  // startTransaction preconditions
  // ============================================================

  test("only the leader of each partition can start a transaction for that partition") {
    val (state, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    // Each partition's leader can start a transaction for its own partition
    assertNotEquals(state.startTransaction(p1, "tx")(using id1a), SimpSpan[String]())
    assertNotEquals(state.startTransaction(p2, "tx")(using id2a), SimpSpan[String]())
    assertNotEquals(state.startTransaction(p3, "tx")(using id3a), SimpSpan[String]())

    // Non-leaders cannot start a transaction
    assertEquals(state.startTransaction(p1, "tx")(using id1b), SimpSpan[String]())
    assertEquals(state.startTransaction(p1, "tx")(using id1c), SimpSpan[String]())
    assertEquals(state.startTransaction(p2, "tx")(using id2b), SimpSpan[String]())
    assertEquals(state.startTransaction(p3, "tx")(using id3b), SimpSpan[String]())
  }

  test("a leader of one partition cannot start a transaction for a different partition") {
    val (state, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()

    // id1a is a member of p1, not p2 – validate2PC for p2 requires p2 membership
    // startTransaction(p2, ...) passes precondition only when caller is p2's leader
    assertEquals(state.startTransaction(p2, "tx")(using id1a), SimpSpan[String]())
    assertEquals(state.startTransaction(p3, "tx")(using id2a), SimpSpan[String]())
    assertEquals(state.startTransaction(p1, "tx")(using id3a), SimpSpan[String]())
  }

  test("each partition leader independently starts separate transactions") {
    val (state, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()

    val d1 = state.startTransaction(p1, "tx-from-p1")(using id1a)
    val d2 = state.startTransaction(p2, "tx-from-p2")(using id2a)
    val d3 = state.startTransaction(p3, "tx-from-p3")(using id3a)

    val merged = state.merge(d1).merge(d2).merge(d3)

    assertEquals(merged.transactions.size, 3)

    val coordinators: Set[Option[Uid]] = merged.transactions.values.map(_.coordinator).toSet
    assertEquals(coordinators, Set[Option[Uid]](Some(p1), Some(p2), Some(p3)))

    val txValues: Set[Option[String]] = merged.transactions.values.map(_.transaction).toSet
    assertEquals(txValues, Set[Option[String]](Some("tx-from-p1"), Some("tx-from-p2"), Some("tx-from-p3")))
  }

  // ============================================================
  // decision with three partitions
  // ============================================================

  test("decision is undecided when only one of three partitions has voted in prepare") {
    val p1   = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val txId = Uid.gen()

    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("pending"),
      prepare = FlexibleVoting(Set(Vote(p1, true))) // only p1 voted
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))
  }

  test("decision is undecided when two of three partitions voted true") {
    val p1   = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val txId = Uid.gen()

    // FullQuorum requires all three; only two have voted
    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("partial"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true)))
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))
  }

  test("decision is committed when all three partitions voted true in prepare") {
    val p1   = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val txId = Uid.gen()

    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("all-yes"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true), Vote(p3, true))),
      commit = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true), Vote(p3, true)))
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map(txId -> true)))
  }

  test("decision is aborted when the first of three partitions votes false") {
    val p1   = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val txId = Uid.gen()

    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("p1-veto"),
      prepare = FlexibleVoting(Set(Vote(p1, false), Vote(p2, true), Vote(p3, true))),
      commit = FlexibleVoting(Set(Vote(p1, false), Vote(p2, false), Vote(p3, false)))
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map(txId -> false)))
  }

  test("decision is aborted when the last of three partitions votes false") {
    val p1   = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val txId = Uid.gen()

    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("p3-veto"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true), Vote(p3, false))),
      commit = FlexibleVoting(Set(Vote(p1, false), Vote(p2, false), Vote(p3, false)))
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map(txId -> false)))
  }

  test("decision reports mix of committed, aborted, and pending across three partitions") {
    val p1          = Uid.gen(); val p2 = Uid.gen(); val p3 = Uid.gen()
    val committedId = Uid.gen()
    val abortedId   = Uid.gen()
    val pendingId   = Uid.gen()

    val committed = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("commit"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true), Vote(p3, true))),
      commit = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true), Vote(p3, true)))
    )
    val aborted = TwoPhaseCommit[String](
      coordinator = Some(p2),
      transaction = Some("abort"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, false), Vote(p3, true))),
      commit = FlexibleVoting(Set(Vote(p1, false), Vote(p2, false), Vote(p3, false)))
    )
    // Only two partitions have voted → FullQuorum not satisfied → Undecided
    val pending = TwoPhaseCommit[String](
      coordinator = Some(p3),
      transaction = Some("pending"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true)))
    )

    val state = SimpSpan[String](
      paxosPartitions = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos(), p3 -> MultiPaxos()),
      transactions = Map(committedId -> committed, abortedId -> aborted, pendingId -> pending)
    )

    assertEquals(
      state.decision,
      Agreement.Decided(Map(committedId -> true, abortedId -> false))
    )
  }

  // ============================================================
  // Lattice properties with three partitions
  // ============================================================

  test("merge is idempotent for a three-partition SimpSpan state") {
    val (state, _, _, _, _, _, _, _, _, _, _, _, _) = buildThreePartitionState()
    assertEquals(state.merge(state), state)
  }

  test("merge is commutative for startTransaction deltas across three partitions") {
    val (state, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()

    val d1 = state.startTransaction(p1, "alpha")(using id1a)
    val d2 = state.startTransaction(p2, "beta")(using id2a)
    val d3 = state.startTransaction(p3, "gamma")(using id3a)

    val s123 = state.merge(d1).merge(d2).merge(d3)
    val s321 = state.merge(d3).merge(d2).merge(d1)
    val s213 = state.merge(d2).merge(d1).merge(d3)

    assertEquals(s123, s321)
    assertEquals(s123, s213)
  }

  test("merge accumulates transactions from all three partitions") {
    val (state, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()

    val d1 = state.startTransaction(p1, "tx1")(using id1a)
    val d2 = state.startTransaction(p2, "tx2")(using id2a)
    val d3 = state.startTransaction(p3, "tx3")(using id3a)

    val merged = state.merge(d1).merge(d2).merge(d3)

    assertEquals(merged.transactions.size, 3)
    assert(merged.transactions.values.exists(_.coordinator == Some(p1)))
    assert(merged.transactions.values.exists(_.coordinator == Some(p2)))
    assert(merged.transactions.values.exists(_.coordinator == Some(p3)))
  }

  // ============================================================
  // validate2PC with three partitions
  // ============================================================

  test("validate2PC can be called independently from each partition's leader") {
    val (baseState, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()
    val txId                                                        = Uid.gen()

    val stateWithTx = baseState.merge(
      SimpSpan[String](transactions =
        Map(txId -> TwoPhaseCommit[String](coordinator = Some(p1), transaction = Some("multi-part-tx")))
      )
    )

    val delta1 = stateWithTx.validate2PC(p1, txId, valid = true)(using id1a)
    val delta2 = stateWithTx.validate2PC(p2, txId, valid = true)(using id2a)
    val delta3 = stateWithTx.validate2PC(p3, txId, valid = true)(using id3a)

    // Each delta carries a non-empty paxos update for its own partition
    assert(delta1.paxosPartitions.contains(p1) && delta1.paxosPartitions(p1) != MultiPaxos())
    assert(delta2.paxosPartitions.contains(p2) && delta2.paxosPartitions(p2) != MultiPaxos())
    assert(delta3.paxosPartitions.contains(p3) && delta3.paxosPartitions(p3) != MultiPaxos())

    // Merging all three deltas accumulates proposals in all three partition paxos instances
    val updated = stateWithTx.merge(delta1).merge(delta2).merge(delta3)
    assert(updated.paxosPartitions(p1) != baseState.paxosPartitions(p1))
    assert(updated.paxosPartitions(p2) != baseState.paxosPartitions(p2))
    assert(updated.paxosPartitions(p3) != baseState.paxosPartitions(p3))
  }

  test("validate2PC returns empty delta when replica does not belong to the target partition") {
    val (state, p1, p2, p3, id1a, _, _, id2a, _, _, id3a, _, _) = buildThreePartitionState()
    val txId                                                    = Uid.gen()
    val stateWithTx                                             = state.merge(
      SimpSpan[String](transactions =
        Map(txId -> TwoPhaseCommit[String](coordinator = Some(p1), transaction = Some("tx")))
      )
    )

    // id1a is a member of p1, not p2 or p3
    assertEquals(stateWithTx.validate2PC(p2, txId, valid = true)(using id1a), SimpSpan[String]())
    assertEquals(stateWithTx.validate2PC(p3, txId, valid = true)(using id1a), SimpSpan[String]())
    // id2a is a member of p2, not p1 or p3
    assertEquals(stateWithTx.validate2PC(p1, txId, valid = true)(using id2a), SimpSpan[String]())
    assertEquals(stateWithTx.validate2PC(p3, txId, valid = true)(using id2a), SimpSpan[String]())
  }

  // ============================================================
  // End-to-end: three partitions, three replicas, unanimous commit
  // ============================================================

  test("end-to-end commit: three partitions vote yes, transaction fully decided as committed") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)

    // Step 1: p1 leader starts a cross-partition transaction
    var state   = baseState
    val txDelta = state.startTransaction(p1, "cross-partition-commit")(using id1a)
    assertNotEquals(txDelta, SimpSpan[String]())
    state = state.merge(txDelta)

    assertEquals(state.transactions.size, 1)
    val (txId, tx) = state.transactions.head
    assertEquals(tx.coordinator, Some(p1))
    assertEquals(tx.transaction, Some("cross-partition-commit"))

    // No decision yet – no partition has voted
    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))

    // Step 2: Each partition leader validates with true (logs a Prepare in its paxos)
    state = state.merge(state.validate2PC(p1, txId, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId, valid = true)(using id3a))

    // Step 3: Round 1 upkeep – commits Prepare in each partition's paxos
    //         and transfers prepare votes into the TwoPhaseCommit state
    state = runUpkeepRound(state, allReplicas)

    // Each partition's paxos should have one committed log entry (Prepare)
    assertEquals(state.paxosPartitions(p1).log.size, 1)
    assertEquals(state.paxosPartitions(p2).log.size, 1)
    assertEquals(state.paxosPartitions(p3).log.size, 1)

    // All three partitions have voted true → SimpSpan.decision already knows
    assertEquals(state.decision, Agreement.Decided(Map(txId -> true)))

    // The 2PC transaction itself is not yet fully decided (Commit not yet acknowledged)
    assertNotEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )

    // Step 4: Round 2 upkeep – each partition leader calls acknowledge2PC (Commit proposal),
    //         followers vote, Commit committed, and the 2PC commit phase is transferred
    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )

    // Step 5: All partitions are fully settled – upkeep is a no-op for all replicas
    allReplicas.foreach { m =>
      assertEquals(state.upkeep(using m), SimpSpan[String](), s"Expected empty upkeep for $m")
    }
  }

  // ============================================================
  // End-to-end: three partitions, one partition vetoes (abort)
  // ============================================================

  test("end-to-end abort: p3 vetoes the transaction, all partitions reach Decided(false)") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)

    // Step 1: Start transaction from p1
    var state = baseState
    state = state.merge(state.startTransaction(p1, "will-be-aborted")(using id1a))
    val (txId, _) = state.transactions.head

    // Step 2: p1 and p2 vote yes, p3 votes no
    state = state.merge(state.validate2PC(p1, txId, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId, valid = false)(using id3a)) // veto

    // Step 3: Round 1 upkeep – commits each Prepare and transfers prepare votes
    state = runUpkeepRound(state, allReplicas)

    // p3's veto is now reflected: decision reports abort
    assertEquals(state.decision, Agreement.Decided(Map(txId -> false)))

    // The 2PC transaction is not yet acknowledged (Abort not yet committed)
    assertNotEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(false)
    )

    // Step 4: Round 2 upkeep – each partition leader calls acknowledge2PC (Abort proposal),
    //         followers vote, Abort committed, and the 2PC abort phase is transferred
    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(false)
    )

    // Step 5: Fully converged
    allReplicas.foreach { m =>
      assertEquals(state.upkeep(using m), SimpSpan[String](), s"Expected empty upkeep for $m")
    }
  }

  // ============================================================
  // End-to-end: multiple concurrent transactions, all committed
  // ============================================================

  test("two sequential transactions in the same three-partition setup both commit") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)
    var state       = baseState

    // ---- Transaction 1 ----
    state = state.merge(state.startTransaction(p1, "tx-one")(using id1a))
    val (txId1, _) = state.transactions.head

    state = state.merge(state.validate2PC(p1, txId1, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId1, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId1, valid = true)(using id3a))

    // Two rounds of upkeep to fully decide tx1
    state = runUpkeepRound(state, allReplicas)
    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId1).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )

    // ---- Transaction 2 ----
    state = state.merge(state.startTransaction(p1, "tx-two")(using id1a))
    val txId2 = state.transactions.keys.find(_ != txId1).get

    state = state.merge(state.validate2PC(p1, txId2, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId2, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId2, valid = true)(using id3a))

    state = runUpkeepRound(state, allReplicas)
    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId2).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )

    // Both transactions coexist in the state
    assertEquals(state.transactions.size, 2)
    assertEquals(
      state.decision,
      Agreement.Decided(Map(txId1 -> true, txId2 -> true))
    )
  }

  // ============================================================
  // End-to-end: transactions started by different partition leaders
  // ============================================================

  test("transactions started by different partition leaders are correctly routed") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)
    var state       = baseState

    // p2's leader starts a transaction (coordinator is p2, not p1)
    state = state.merge(state.startTransaction(p2, "p2-originated")(using id2a))
    val (txId, tx) = state.transactions.head
    assertEquals(tx.coordinator, Some(p2))

    // All three partitions validate
    state = state.merge(state.validate2PC(p1, txId, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId, valid = true)(using id3a))

    state = runUpkeepRound(state, allReplicas)
    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )
  }

  // ============================================================
  // End-to-end: middle partition (p2) vetoes
  // ============================================================

  test("end-to-end abort: p2 (middle partition) vetoes, all partitions reach Decided(false)") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)
    var state       = baseState

    state = state.merge(state.startTransaction(p1, "middle-veto")(using id1a))
    val (txId, _) = state.transactions.head

    state = state.merge(state.validate2PC(p1, txId, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId, valid = false)(using id2a)) // p2 vetoes
    state = state.merge(state.validate2PC(p3, txId, valid = true)(using id3a))

    state = runUpkeepRound(state, allReplicas)

    assertEquals(state.decision, Agreement.Decided(Map(txId -> false)))

    state = runUpkeepRound(state, allReplicas)

    assertEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(false)
    )
  }

  // ============================================================
  // Upkeep returns empty delta after convergence
  // ============================================================

  test("upkeep is idempotent after full convergence of a committed transaction") {
    val (baseState, p1, p2, p3, id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c) =
      buildThreePartitionState()

    val allReplicas = Seq(id1a, id1b, id1c, id2a, id2b, id2c, id3a, id3b, id3c)
    var state       = baseState

    state = state.merge(state.startTransaction(p1, "idempotent-test")(using id1a))
    val (txId, _) = state.transactions.head

    state = state.merge(state.validate2PC(p1, txId, valid = true)(using id1a))
    state = state.merge(state.validate2PC(p2, txId, valid = true)(using id2a))
    state = state.merge(state.validate2PC(p3, txId, valid = true)(using id3a))

    state = runUpkeepRound(state, allReplicas)
    state = runUpkeepRound(state, allReplicas)

    // Fully decided
    assertEquals(
      state.transactions(txId).decision(using Participants(state.paxosPartitions.keySet)),
      Agreement.Decided(true)
    )

    // Extra upkeep rounds must produce empty deltas
    val snapshot = state
    state = runUpkeepRound(state, allReplicas)
    assertEquals(state, snapshot)

    state = runUpkeepRound(state, allReplicas)
    assertEquals(state, snapshot)
  }

  // ============================================================
  // Merge of independent election and transaction deltas
  // ============================================================

  test("election delta and transaction delta for different partitions can be merged independently") {
    val id1a = LocalUid.gen(); val id1b = LocalUid.gen(); val id1c = LocalUid.gen()
    val id2a = LocalUid.gen(); val id2b = LocalUid.gen(); val id2c = LocalUid.gen()
    val id3a = LocalUid.gen(); val id3b = LocalUid.gen(); val id3c = LocalUid.gen()
    val p1   = Uid.gen(); val p2        = Uid.gen(); val p3        = Uid.gen()

    // Only p1 has an elected leader; p2 and p3 are fresh
    val paxos1 = electLeader(Seq(id1a, id1b, id1c), id1a)

    val state = SimpSpan[String](
      paxosPartitions = Map(
        p1 -> paxos1,
        p2 -> MultiPaxos[twoPCMessages](),
        p3 -> MultiPaxos[twoPCMessages]()
      ),
      partitionMembers = Map(
        p1 -> Set(id1a.uid, id1b.uid, id1c.uid),
        p2 -> Set(id2a.uid, id2b.uid, id2c.uid),
        p3 -> Set(id3a.uid, id3b.uid, id3c.uid)
      )
    )

    // Start a transaction from p1 (already has a leader)
    val txDelta = state.startTransaction(p1, "early-tx")(using id1a)
    assertNotEquals(txDelta, SimpSpan[String]())

    // Meanwhile, p2 kicks off its election independently
    val p2ElectionDelta = state.paxosPartitions(p2).startLeaderElection(using id2a)
    val p2StateDelta    = SimpSpan[String](paxosPartitions = Map(p2 -> p2ElectionDelta))

    // Both deltas can be merged into the state without conflict
    val merged = state.merge(txDelta).merge(p2StateDelta)

    assertEquals(merged.transactions.size, 1)
    // p1's paxos is untouched by p2's election delta
    assertEquals(merged.paxosPartitions(p1), state.paxosPartitions(p1))
    // p3's paxos is untouched by both deltas
    assertEquals(merged.paxosPartitions(p3), state.paxosPartitions(p3))
    // p2 has an election in progress
    assertNotEquals(merged.paxosPartitions(p2), state.paxosPartitions(p2))
  }
}
