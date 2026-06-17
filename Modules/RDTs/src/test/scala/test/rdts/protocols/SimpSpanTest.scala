package test.rdts.protocols

import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.protocols.Quorum.FullQuorum
import rdts.protocols.Util.Agreement
import rdts.protocols.spanner.{FlexibleVoting, SimpSpan, twoPCMessages}
import rdts.protocols.*

class SimpSpanTest extends munit.FunSuite {

  // ============================================================
  // Helpers
  // ============================================================

  /** Elect `leader` as leader of a fresh MultiPaxos for the given members. */
  def electLeader(members: Seq[LocalUid], leader: LocalUid): MultiPaxos[twoPCMessages] = {
    given Participants = Participants(members.map(_.uid).toSet)
    var paxos          = MultiPaxos[twoPCMessages]()
    paxos = paxos.merge(paxos.startLeaderElection(using leader))
    // non-leaders reply with 1b votes
    members.filterNot(_ == leader).foreach { m => paxos = paxos.merge(paxos.upkeep(using m)) }
    // leader counts the 1b votes and establishes itself
    paxos = paxos.merge(paxos.upkeep(using leader))
    paxos
  }

  /** Commit `value` into the log of `paxos` (assumes leader is already elected). */
  def proposeAndCommit(
      paxos: MultiPaxos[twoPCMessages],
      value: twoPCMessages,
      leader: LocalUid,
      members: Seq[LocalUid]
  ): MultiPaxos[twoPCMessages] = {
    given Participants = Participants(members.map(_.uid).toSet)
    var p              = paxos.merge(paxos.proposeIfLeader(value)(using leader))
    // collect votes from all members so the value is committed
    members.foreach { m => p = p.merge(p.upkeep(using m)) }
    p
  }

  // ============================================================
  // Initial / empty state
  // ============================================================

  test("empty SimpSpan has no decided transactions") {
    val empty = SimpSpan[String]()
    assertEquals(empty.decision, Agreement.Decided(Map.empty[Uid, Boolean]))
  }

  test("upkeep on empty SimpSpan returns empty delta") {
    given id: LocalUid = LocalUid.gen()
    assertEquals(SimpSpan[String]().upkeep, SimpSpan[String]())
  }

  // ============================================================
  // localPartitionId
  // ============================================================

  test("localPartitionId returns Some for a member replica") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val partitionId = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(partitionId -> Set(id1.uid, id2.uid))
    )

    assertEquals(state.localPartitionId(using id1), Some(partitionId))
    assertEquals(state.localPartitionId(using id2), Some(partitionId))
  }

  test("localPartitionId returns None for a non-member replica") {
    val id1         = LocalUid.gen()
    val outsider    = LocalUid.gen()
    val partitionId = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(partitionId -> Set(id1.uid))
    )

    assertEquals(state.localPartitionId(using outsider), None)
  }

  test("localPartitionId distinguishes between multiple partitions") {
    val id1 = LocalUid.gen()
    val id2 = LocalUid.gen()
    val p1  = Uid.gen()
    val p2  = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(
        p1 -> Set(id1.uid),
        p2 -> Set(id2.uid)
      )
    )

    assertEquals(state.localPartitionId(using id1), Some(p1))
    assertEquals(state.localPartitionId(using id2), Some(p2))
  }

  // ============================================================
  // startTransaction – precondition failures
  // ============================================================

  test("startTransaction returns empty delta when partition not in paxosPrepare") {
    val id          = LocalUid.gen()
    val partitionId = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(partitionId -> Set(id.uid))
      // paxosPrepare is empty – no Paxos instance for partitionId
    )

    assertEquals(state.startTransaction(partitionId, "tx")(using id), SimpSpan[String]())
  }

  test("startTransaction returns empty delta when partition not in partitionMembers") {
    val id          = LocalUid.gen()
    val partitionId = Uid.gen()

    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> MultiPaxos[twoPCMessages]())
      // partitionMembers is empty
    )

    assertEquals(state.startTransaction(partitionId, "tx")(using id), SimpSpan[String]())
  }

  test("startTransaction returns empty delta when there is no leader yet") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    // fresh Paxos with no leader elected
    val paxos = MultiPaxos[twoPCMessages]()
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    assertEquals(state.startTransaction(partitionId, "tx")(using id1), SimpSpan[String]())
  }

  test("startTransaction returns empty delta when replica is not the leader") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1) // id1 is the leader

    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    // id2 is not the leader
    assertEquals(state.startTransaction(partitionId, "tx")(using id2), SimpSpan[String]())
  }

  // ============================================================
  // startTransaction – success
  // ============================================================

  test("startTransaction creates a new transaction when leader") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    val delta = state.startTransaction(partitionId, "hello")(using id1)

    assertNotEquals(delta, SimpSpan[String]())
    assertEquals(delta.transactions.size, 1)

    val (_, tx) = delta.transactions.head
    assertEquals(tx.coordinator, Some(partitionId))
    assertEquals(tx.transaction, Some("hello"))
  }

  test("multiple startTransaction deltas can be merged and accumulate monotonically") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos     = electLeader(members, id1)
    val baseState = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    val d1     = baseState.startTransaction(partitionId, "tx1")(using id1)
    val d2     = baseState.startTransaction(partitionId, "tx2")(using id1)
    val merged = baseState.merge(d1).merge(d2)

    assertEquals(merged.transactions.size, 2)
    assert(merged.transactions.values.map(_.transaction).toSet == Set(Some("tx1"), Some("tx2")))
  }

  // ============================================================
  // Lattice properties
  // ============================================================

  test("merge is idempotent for empty SimpSpan") {
    val empty = SimpSpan[String]()
    assertEquals(empty.merge(empty), empty)
  }

  test("merge with Bottom (empty) is identity") {
    val id1         = LocalUid.gen()
    val partitionId = Uid.gen()

    val state  = SimpSpan[String](partitionMembers = Map(partitionId -> Set(id1.uid)))
    val bottom = Bottom[SimpSpan[String]].empty

    assertEquals(state.merge(bottom), state)
    assertEquals(bottom.merge(state), state)
  }

  test("merge is idempotent for a non-empty SimpSpan") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    assertEquals(state.merge(state), state)
  }

  test("merge is commutative for startTransaction deltas") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos     = electLeader(members, id1)
    val baseState = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    val d1 = baseState.startTransaction(partitionId, "alpha")(using id1)
    val d2 = baseState.startTransaction(partitionId, "beta")(using id1)

    val ab = baseState.merge(d1).merge(d2)
    val ba = baseState.merge(d2).merge(d1)

    assertEquals(ab, ba)
  }

  // ============================================================
  // upkeep
  // ============================================================

  test("upkeep returns empty delta for a non-member replica") {
    val id1         = LocalUid.gen()
    val outsider    = LocalUid.gen()
    val partitionId = Uid.gen()

    val state = SimpSpan[String](
      partitionMembers = Map(partitionId -> Set(id1.uid))
    )

    assertEquals(state.upkeep(using outsider), SimpSpan[String]())
  }

  test("upkeep progresses Paxos leader election for partition members") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    given Participants = Participants(members.map(_.uid).toSet)

    // Paxos with leader election started but not yet concluded
    val initialPaxos = MultiPaxos[twoPCMessages]()
    val withElection = initialPaxos.merge(initialPaxos.startLeaderElection(using id1))

    var state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> withElection),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    // id2 and id3 vote in the leader election via upkeep
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))
    // id1 concludes the election
    state = state.merge(state.upkeep(using id1))

    assertEquals(state.paxosPrepare(partitionId).leader, Some(id1.uid))
  }

  test("upkeep transfers a committed Prepare log entry into the 2PC prepare phase") {
    val id1     = LocalUid.gen()
    val id2     = LocalUid.gen()
    val id3     = LocalUid.gen()
    val p1      = Uid.gen()
    val p2      = Uid.gen() // second partition (used as a second 2PC voter)
    val txId    = Uid.gen()
    val members = Seq(id1, id2, id3)

    // Build p1's paxos with Prepare(txId, true) committed in the log
    var paxos = electLeader(members, id1)
    paxos = proposeAndCommit(paxos, twoPCMessages.Prepare(txId, true), id1, members)
    assertEquals(paxos.read, List(twoPCMessages.Prepare(txId, true)))

    // Pre-seed prepare with a vote from p2 so that p1 can still vote and
    // prepare.votes is non-empty (required by TwoPhaseCommit.prepare precondition)
    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("my-tx"),
      prepare = FlexibleVoting(Set(Vote(p2, true)))
    )

    // id1 belongs to p1; no member of p2 is id1
    var state = SimpSpan[String](
      paxosPrepare = Map(p1 -> paxos, p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      partitionMembers = Map(
        p1 -> members.map(_.uid).toSet,
        p2 -> Set(Uid.gen()) // p2 members are different replicas
      ),
      transactions = Map(txId -> twoPC)
    )

    // upkeep by id1 (member of p1) processes the Prepare log entry
    val delta = state.upkeep(using id1)
    state = state.merge(delta)

    // p1 should have added its vote to the prepare phase
    val updatedTx = state.transactions(txId)
    val voterIds  = updatedTx.prepare.votes.map(_.voter)
    assert(voterIds.contains(p1), s"Expected p1 to vote in prepare; got voters: $voterIds")

    // With votes from both p1 and p2, prepare is fully decided
    given Participants = Participants(Set(p1, p2))
    given Quorum       = FullQuorum
    assertEquals(updatedTx.prepare.decision, Agreement.Decided(true))
  }

  // ============================================================
  // decision
  // ============================================================

  test("decision is empty when no transactions are present") {
    assertEquals(SimpSpan[String]().decision, Agreement.Decided(Map.empty[Uid, Boolean]))
  }

  test("decision is empty when transactions are still pending (not all partitions voted)") {
    val p1   = Uid.gen()
    val p2   = Uid.gen()
    val txId = Uid.gen()

    // Only p1 has voted in prepare; FullQuorum requires both p1 and p2
    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("pending"),
      prepare = FlexibleVoting(Set(Vote(p1, true)))
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      transactions = Map(txId -> twoPC)
    )

    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))
  }

  test("decision reports a committed transaction") {
    val p1   = Uid.gen()
    val p2   = Uid.gen()
    val txId = Uid.gen()

    val committed = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("committed-tx"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true))),
      commit = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true)))
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      transactions = Map(txId -> committed),
      partitionMembers = Map(p1 -> Set(), p2 -> Set()),
    )

    assertEquals(state.decision, Agreement.Decided(Map(txId -> true)))
  }

  test("decision reports an aborted transaction (false vote in prepare)") {
    val p1   = Uid.gen()
    val p2   = Uid.gen()
    val txId = Uid.gen()

    // p1 vetoed in prepare → outcome is abort
    val aborted = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("aborted-tx"),
      prepare = FlexibleVoting(Set(Vote(p1, false), Vote(p2, true))),
      commit = FlexibleVoting(Set(Vote(p1, false), Vote(p2, false)))
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      transactions = Map(txId -> aborted)
    )

    assertEquals(state.decision, Agreement.Decided(Map(txId -> false)))
  }

  test("decision reports mix of committed, aborted, and pending transactions") {
    val p1            = Uid.gen()
    val p2            = Uid.gen()
    val committedTxId = Uid.gen()
    val abortedTxId   = Uid.gen()
    val pendingTxId   = Uid.gen()

    val committed = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("committed"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true))),
      commit = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true)))
    )
    val aborted = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("aborted"),
      prepare = FlexibleVoting(Set(Vote(p1, false), Vote(p2, true))),
      commit = FlexibleVoting(Set(Vote(p1, false), Vote(p2, false)))
    )
    val pending = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("pending"),
      prepare = FlexibleVoting(Set(Vote(p1, true))) // only one partition voted
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> MultiPaxos(), p2 -> MultiPaxos()),
      transactions = Map(committedTxId -> committed, abortedTxId -> aborted, pendingTxId -> pending),
      partitionMembers = Map(p1 -> Set(), p2 -> Set())
    )

    assertEquals(state.decision, Agreement.Decided(Map(committedTxId -> true, abortedTxId -> false)))
  }

  // ============================================================
  // validate2PC and acknowledge2PC
  // ============================================================

  test("validate2PC returns empty SimpSpan when transaction does not exist") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
      // no transactions
    )

    val delta = state.validate2PC(partitionId, Uid.gen(), valid = true)(using id1)
    assertEquals(delta, SimpSpan[String]())
  }

  test("validate2PC returns empty SimpSpan when replica is not in partitionMembers") {
    val id1         = LocalUid.gen()
    val outsider    = LocalUid.gen()
    val partitionId = Uid.gen()
    val txId        = Uid.gen()

    val paxos = electLeader(Seq(id1), id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> Set(id1.uid)), // outsider is not a member
      transactions = Map(txId -> TwoPhaseCommit[String](transaction = Some("tx")))
    )

    val delta = state.validate2PC(partitionId, txId, valid = true)(using outsider)
    assertEquals(delta, SimpSpan[String]())
  }

  test("validate2PC proposes a Prepare message into the partition paxosPrepare when leader") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val txId        = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet),
      transactions = Map(txId -> TwoPhaseCommit[String](
        coordinator = Some(partitionId),
        transaction = Some("test-tx")
      ))
    )

    val delta = state.validate2PC(partitionId, txId, valid = true)(using id1)

    // The delta carries an updated paxosPrepare entry for the partition
    assert(delta.paxosPrepare.contains(partitionId))
    assertNotEquals(delta.paxosPrepare(partitionId), paxos)

    // Because validate2PC now returns SimpSpan, the delta can be merged back
    val updated = state.merge(delta)
    assertNotEquals(updated.paxosPrepare(partitionId), paxos)
  }

  test("acknowledge2PC returns empty SimpSpan when transaction has no prepare decision yet") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val txId        = Uid.gen()
    val members     = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)
    val state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> paxos),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet),
      transactions = Map(txId -> TwoPhaseCommit[String](
        coordinator = Some(partitionId),
        transaction = Some("test-tx")
        // prepare.votes is empty → decision is Undecided → precondition fails
      ))
    )

    val delta = state.acknowledge2PC(partitionId, txId)(using id1)
    assertEquals(delta, SimpSpan[String]())
  }

  test("acknowledge2PC proposes Commit when all partitions voted true in prepare") {
    val id1     = LocalUid.gen()
    val id2     = LocalUid.gen()
    val id3     = LocalUid.gen()
    val p1      = Uid.gen()
    val p2      = Uid.gen()
    val txId    = Uid.gen()
    val members = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)

    // Both partitions voted true in prepare → decision is Decided(true)
    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("commit-me"),
      prepare = FlexibleVoting(Set(Vote(p1, true), Vote(p2, true)))
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> paxos, p2 -> paxos),
      paxosAcknowledge = Map(p1 -> paxos, p2 -> paxos),
      partitionMembers = Map(p1 -> members.map(_.uid).toSet, p2 -> Set(Uid.gen())),
      transactions = Map(txId -> twoPC)
    )

    val delta = state.acknowledge2PC(p1, txId)(using id1)

    // The delta carries an updated paxosAcknowledge entry with the Commit message proposed
    assert(delta.paxosAcknowledge.contains(p1), "paxosAcknowledge should contain p1")
    assertNotEquals(delta.paxosAcknowledge(p1), MultiPaxos(), "paxosAcknowledge should be updated")

    // Delta is a SimpSpan and can be merged back
    val updated = state.merge(delta)
    assertNotEquals(
      updated.paxosAcknowledge(p1),
      MultiPaxos(),
      "paxosAcknowledge should be updated after merge"
    )
  }

  test("acknowledge2PC proposes Abort when any partition voted false in prepare") {
    val id1     = LocalUid.gen()
    val id2     = LocalUid.gen()
    val id3     = LocalUid.gen()
    val p1      = Uid.gen()
    val p2      = Uid.gen()
    val txId    = Uid.gen()
    val members = Seq(id1, id2, id3)

    val paxos = electLeader(members, id1)

    // One partition voted false, one true → prepare.decision = Decided(false).
    val twoPC = TwoPhaseCommit[String](
      coordinator = Some(p1),
      transaction = Some("veto-me"),
      prepare = FlexibleVoting(Set(Vote(p1, false), Vote(p2, true)))
    )

    val state = SimpSpan[String](
      paxosPrepare = Map(p1 -> paxos, p2 -> MultiPaxos()),
      paxosAcknowledge = Map(p1 -> paxos, p2 -> MultiPaxos()),
      partitionMembers = Map(p1 -> members.map(_.uid).toSet, p2 -> Set(Uid.gen())),
      transactions = Map(txId -> twoPC)
    )

    val delta = state.acknowledge2PC(p1, txId)(using id1)

    // The delta carries an updated paxosAcknowledge entry (Abort proposed)
    assert(delta.paxosAcknowledge.contains(p1), "paxosAcknowledge should contain p1")
    assertNotEquals(delta.paxosAcknowledge(p1), MultiPaxos(), "paxosAcknowledge should be updated")

    // the delta carries the abort proposal
    assert(
      delta.paxosAcknowledge(p1).rounds.value.rounds.find(_._2.proposals.votes.contains(Vote(
        id1.uid,
        twoPCMessages.Abort(txId)
      ))).isDefined,
      "abort proposal should be in paxos rounds"
    )

    // Delta is a SimpSpan and can be merged back
    val updated = state.merge(delta)
    assertNotEquals(
      updated.paxosAcknowledge(p1),
      MultiPaxos(),
      "paxosAcknowledge should be updated after merge"
    )
  }

  // ============================================================
  // End-to-end: leader election → start transaction → upkeep
  // ============================================================

  test("end-to-end: leader election, startTransaction, and state accumulation") {
    val id1         = LocalUid.gen()
    val id2         = LocalUid.gen()
    val id3         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1, id2, id3)

    given Participants = Participants(members.map(_.uid).toSet)

    // 1. Start with empty paxos for the partition
    var state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    // 2. Trigger leader election for id1 and process via upkeep
    val electionDelta  = state.paxosPrepare(partitionId).startLeaderElection(using id1)
    val electionDelta2 = state.paxosAcknowledge(partitionId).startLeaderElection(using id1)
    state = state.merge(SimpSpan(paxosPrepare = Map(partitionId -> electionDelta)))
    state = state.merge(SimpSpan(paxosAcknowledge = Map(partitionId -> electionDelta2)))
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))
    state = state.merge(state.upkeep(using id1))

    assertEquals(state.paxosPrepare(partitionId).leader, Some(id1.uid))

    // 3. id1 starts a transaction
    val txDelta = state.startTransaction(partitionId, "important-tx")(using id1)
    assertNotEquals(txDelta, SimpSpan[String]())

    state = state.merge(txDelta)
    assertEquals(state.transactions.size, 1)

    val (txId, tx) = state.transactions.head
    assertEquals(tx.coordinator, Some(partitionId))
    assertEquals(tx.transaction, Some("important-tx"))

    // 4. Transaction is pending; no decisions yet
    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))

    // 5. Merging the same delta again is idempotent
    assertEquals(state.merge(txDelta).transactions.size, 1)

    // 6. validate transaction as leader
    state = state.merge(state.validate2PC(localPartitionId = partitionId, txId, true)(using id1))
    // paxosPrepare log should still be empty
    assertEquals(state.paxosPrepare(partitionId).log.size, 0)

    // 7. upkeep with all partition members
    state = state.merge(state.upkeep(using id1))
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))

    // paxosPrepare log size should be 1 after upkeep
    assertEquals(state.paxosPrepare(partitionId).log.size, 1)
    assertEquals(state.paxosPrepare(partitionId).phase, MultipaxosPhase.Idle)

    // we already know that this spanner transaction will be accepted
    assertEquals(state.decision, Agreement.Decided(Map(txId -> true)))

    // the transaction itself is not marked as fully decided because it was not acknowledged
    assertNotEquals(
      state.transactions(txId).decision(using Participants(state.partitionIds)),
      Agreement.Decided(true)
    )

    // 8. upkeep again
    state = state.merge(state.upkeep(using id1))
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))

    // now the 2PC transaction is acknowledged and fully decided
    assertEquals(
      state.transactions(txId).decision(using Participants(state.partitionIds)),
      Agreement.Decided(true)
    )

    // upkeep does not change knowledge anymore
    assertEquals(state.upkeep(using id1), SimpSpan())
    assertEquals(state.upkeep(using id2), SimpSpan())
    assertEquals(state.upkeep(using id3), SimpSpan())

  }

  test("end-to-end with one node: leader election, startTransaction, and state accumulation") {
    val id1         = LocalUid.gen()
    val partitionId = Uid.gen()
    val members     = Seq(id1)

    given Participants = Participants(members.map(_.uid).toSet)

    // 1. Start with empty paxos for the partition
    var state = SimpSpan[String](
      paxosPrepare = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      paxosAcknowledge = Map(partitionId -> MultiPaxos[twoPCMessages]()),
      partitionMembers = Map(partitionId -> members.map(_.uid).toSet)
    )

    // 2. Trigger leader election for id1 and process via upkeep
    val electionDelta  = state.paxosPrepare(partitionId).startLeaderElection(using id1)
    val electionDelta2 = state.paxosAcknowledge(partitionId).startLeaderElection(using id1)
    state = state.merge(SimpSpan(paxosPrepare = Map(partitionId -> electionDelta)))
    state = state.merge(SimpSpan(paxosAcknowledge = Map(partitionId -> electionDelta2)))
    state = state.merge(state.upkeep(using id1))

    assertEquals(state.paxosPrepare(partitionId).leader, Some(id1.uid))

    // 3. id1 starts a transaction
    val txDelta = state.startTransaction(partitionId, "important-tx")(using id1)
    assertNotEquals(txDelta, SimpSpan[String]())

    state = state.merge(txDelta)
    assertEquals(state.transactions.size, 1)

    val (txId, tx) = state.transactions.head
    assertEquals(tx.coordinator, Some(partitionId))
    assertEquals(tx.transaction, Some("important-tx"))

    // 4. Transaction is pending; no decisions yet
    assertEquals(state.decision, Agreement.Decided(Map.empty[Uid, Boolean]))

    // 5. Merging the same delta again is idempotent
    assertEquals(state.merge(txDelta).transactions.size, 1)

    // 6. validate transaction as leader
    state = state.merge(state.validate2PC(localPartitionId = partitionId, txId, true)(using id1))
    // paxosPrepare log should still be empty
    assertEquals(state.paxosPrepare(partitionId).log.size, 0)

    // 7. upkeep with all partition members
    state = state.merge(state.upkeep(using id1))

    // paxosPrepare log size should be 1 after upkeep
    assertEquals(state.paxosPrepare(partitionId).log.size, 1)
    assertEquals(state.paxosPrepare(partitionId).phase, MultipaxosPhase.Idle)

    // we already know that this spanner transaction will be accepted
    assertEquals(state.decision, Agreement.Decided(Map(txId -> true)))

    // the transaction itself is not marked as fully decided because it was not acknowledged
    assertNotEquals(
      state.transactions(txId).decision(using Participants(state.partitionIds)),
      Agreement.Decided(true)
    )

    // 8. upkeep again
    // this causes the partition to acknowledge the outcome in its paxosAcknowledge log, not yet in 2PC
    state = state.merge(state.upkeep(using id1))
    assertEquals(
      state.transactions(txId).decision(using Participants(state.partitionIds)),
      Agreement.Undecided
    )

    // 9. upkeep again
    // this causes the partition to persist the outcome in 2PC
    state = state.merge(state.upkeep(using id1))
    assertEquals(state.paxosPrepare(partitionId).log.size, 1)
    assertEquals(state.paxosAcknowledge(partitionId).log.size, 1)

    // now the 2PC transaction is acknowledged and fully decided
    assertEquals(
      state.transactions(txId).decision(using Participants(state.partitionIds)),
      Agreement.Decided(true)
    )

  }
}
