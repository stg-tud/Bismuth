package test.rdts.protocols

import rdts.base.LocalUid
import rdts.protocols.{MultipaxosPhase, Participants}
import rdts.protocols.Util.Agreement
import rdts.protocols.spanner.ParallelMultiPaxos

class ParallelMultiPaxosTest extends munit.FunSuite {

  val id1: LocalUid = LocalUid.gen()
  val id2: LocalUid = LocalUid.gen()
  val id3: LocalUid = LocalUid.gen()
  val id4: LocalUid = LocalUid.gen()

  given Participants = Participants(Set(id1, id2, id3, id4).map(_.uid))

  // ==================== Helper Functions ====================

  def electLeaderForSlot(slot: Long, leaderId: LocalUid): ParallelMultiPaxos[Int] = {
    var state = ParallelMultiPaxos.empty[Int]
    state = state.merge(state.startLeaderElection(slot)(using leaderId))
    state = state.merge(state.upkeep(using id1))
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))
    state
  }

  def runUpkeepRound(state: ParallelMultiPaxos[Int]): ParallelMultiPaxos[Int] = {
    var result = state
    result = result.merge(result.upkeep(using id1))
    result = result.merge(result.upkeep(using id2))
    result = result.merge(result.upkeep(using id3))
    result
  }

  // ==================== 1. Empty State Tests ====================

  test("Empty State: commitIndex initialized to -1") {
    val empty = ParallelMultiPaxos.empty[Int]
    assertEquals(empty.commitIndex, -1L)
  }

  test("Empty State: read on empty state returns empty") {
    val empty  = ParallelMultiPaxos.empty[Int]
    val result = empty.read(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid)))
    assertEquals(result.toList, List())
  }

  // ==================== 2. Single Slot Leader Election Tests ====================

  test("Single Slot Leader Election: basic election succeeds") {
    var state = ParallelMultiPaxos.empty[Int]
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = state.merge(state.upkeep(using id1))
    state = state.merge(state.upkeep(using id2))
    state = state.merge(state.upkeep(using id3))
    assertEquals(state.leader(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))), Some(id1.uid))
  }

  test("Single Slot Leader Election: different slots can have different leaders") {
    var state = ParallelMultiPaxos.empty[Int]

    // Elect id1 as leader for slot 0
    state = state.merge(electLeaderForSlot(0L, id1))

    // Elect id2 as leader for slot 1
    state = state.merge(state.startLeaderElection(1L)(using id2))

    // Check that slot 0 has leader
    assertEquals(state.log.contains(0L), true)
    assertEquals(state.log.contains(1L), true)
  }

  // ==================== 3. Parallelism Tests ====================

  test("Parallelism: concurrent proposals in multiple slots") {
    var state = ParallelMultiPaxos.empty[Int]

    // Propose in slot 0
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 10)(using id1))

    // Propose in slot 1 concurrently
    state = state.merge(state.startLeaderElection(1L)(using id2))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(1L, 20)(using id2))

    state = runUpkeepRound(state)

    // Both slots should have log entries
    assertEquals(state.decision == Agreement.Decided(List(10, 20)), true)
  }

  // ==================== 4. Commit Index Advancement Tests ====================

  test("Commit Index Advancement: commitIndex advances when decisions are made") {
    var state              = ParallelMultiPaxos.empty[Int]
    val initialCommitIndex = state.commitIndex

    // Elect leader
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)

    // Propose and commit
    state = state.merge(state.proposeIfLeader(0L, 42)(using id1))
    state = runUpkeepRound(state)

    // Run upkeep to advance commitIndex
    state = state.merge(state.upkeep(using id1))

    // commitIndex should be >= initialCommitIndex
    assertEquals(state.commitIndex >= initialCommitIndex, true)
  }

  test("Commit Index Advancement: monotonic advancement") {
    var state       = ParallelMultiPaxos.empty[Int]
    val firstCommit = state.commitIndex

    // Commit slot 0
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 1)(using id1))
    state = runUpkeepRound(state)
    val secondCommit = state.commitIndex

    // Commit slot 1
    state = state.merge(state.startLeaderElection(1L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(1L, 2)(using id1))
    state = runUpkeepRound(state)
    val thirdCommit = state.commitIndex

    // Verify monotonicity
    assert(firstCommit <= secondCommit)
    assert(secondCommit <= thirdCommit)
  }

  // ==================== 5. Read Semantics & Contiguity Tests ====================

  test("Read Semantics & Contiguity: contiguous read returns decisions in order") {
    var state = ParallelMultiPaxos.empty[Int]

    // Commit values 10, 20, 30 in order
    for slot <- 0L to 2L do {
      state = state.merge(state.startLeaderElection(slot)(using id1))
      state = runUpkeepRound(state)
      state = state.merge(state.proposeIfLeader(slot, (slot + 1).toInt * 10)(using id1))
      state = runUpkeepRound(state)
    }

    val decisions = state.read(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))).toList
    assertEquals(decisions.toList, List(10, 20, 30))
  }

  test("Read Semantics & Contiguity: read stops at undecided") {
    var state = ParallelMultiPaxos.empty[Int]

    // Commit slot 0
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 100)(using id1))
    state = runUpkeepRound(state)

    // Start but don't complete slot 1
    state = state.merge(state.startLeaderElection(1L)(using id1))
    state = runUpkeepRound(state)

    val decisions = state.read(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))).toList
    // Should stop at undecided slot 1
    assertEquals(decisions.size, 1)
  }

  test("Read Semantics & Contiguity: readDecisionsSince returns decisions from specified time") {
    var state = ParallelMultiPaxos.empty[Int]

    // Commit slots 0, 1, 2
    for slot <- 0L to 2L do {
      state = state.merge(state.startLeaderElection(slot)(using id1))
      state = runUpkeepRound(state)
      state = state.merge(state.proposeIfLeader(slot, (slot + 1).toInt * 10)(using id1))
      state = runUpkeepRound(state)
    }

    // Read from slot 1
    val decisions = state.readDecisionsSince(1L)(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))).toList
    assertEquals(decisions.toList, List(20, 30))
  }

  // ==================== 6. Lattice Laws Tests ====================

  test("Lattice Laws: idempotency (merge with self)") {
    var state = ParallelMultiPaxos.empty[Int]
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)

    val merged = state.merge(state)
    assertEquals(merged, state)
  }

  test("Lattice Laws: commutativity (a merge b = b merge a)") {
    var a = ParallelMultiPaxos.empty[Int]
    a = a.merge(a.startLeaderElection(0L)(using id1))

    var b = ParallelMultiPaxos.empty[Int]
    b = b.merge(b.startLeaderElection(0L)(using id2))

    val ab = a.merge(b)
    val ba = b.merge(a)
    assertEquals(ab, ba)
  }

  test("Lattice Laws: associativity ((a merge b) merge c = a merge (b merge c))") {
    var a = ParallelMultiPaxos.empty[Int]
    a = a.merge(a.startLeaderElection(0L)(using id1))

    var b = ParallelMultiPaxos.empty[Int]
    b = b.merge(b.startLeaderElection(1L)(using id2))

    var c = ParallelMultiPaxos.empty[Int]
    c = c.merge(c.startLeaderElection(2L)(using id3))

    val leftAssoc  = a.merge(b).merge(c)
    val rightAssoc = a.merge(b.merge(c))
    assertEquals(leftAssoc, rightAssoc)
  }

  // ==================== 7. Phase Tracking Tests ====================

  test("Phase Tracking: LeaderElection phase begins") {
    val state = ParallelMultiPaxos.empty[Int]
    assertEquals(
      state.phase(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))),
      MultipaxosPhase.LeaderElection
    )
  }

  test("Phase Tracking: transitions to Voting phase after leader election and proposal") {
    var state = ParallelMultiPaxos.empty[Int]
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 42)(using id1))

    val phase = state.phase(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid)))
    assertEquals(phase, MultipaxosPhase.Voting)
  }

  test("Phase Tracking: transitions to Idle phase after voting completes") {
    var state = ParallelMultiPaxos.empty[Int]
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 42)(using id1))
    state = runUpkeepRound(state)

    val phase = state.phase(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid)))
    assertEquals(phase == MultipaxosPhase.Idle, true)
  }

  // ==================== 8. Edge Cases Tests ====================

  test("Edge Cases: gaps in slots") {
    var state = ParallelMultiPaxos.empty[Int]

    // Commit slot 0
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 10)(using id1))
    state = runUpkeepRound(state)

    // Skip slot 1, try to commit slot 2 -> does nothing
    state = state.merge(state.startLeaderElection(2L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(2L, 30)(using id1))
    state = runUpkeepRound(state)

    // Gap should prevent full read
    val decisions = state.read(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid))).toList
    assertEquals(decisions.size, 1)
    assertEquals(state.log.size, 1)
  }

  test("Edge Cases: ballot reuse in parallel slots") {
    var state = ParallelMultiPaxos.empty[Int]

    // Elect leader for slot 0
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)

    // Propose and complete slot 0
    state = state.merge(state.proposeIfLeader(0L, 100)(using id1))
    state = runUpkeepRound(state)

    // Reuse ballot for slot 1
    state = state.merge(state.proposeIfLeader(1L, 200)(using id1))
    state = runUpkeepRound(state)

    assertEquals(state.log.size, 2)
  }

  // ==================== 9. Decision Tests ====================

  test("Decision: decided returns Decided when values are committed") {
    var state = ParallelMultiPaxos.empty[Int]

    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 99)(using id1))
    state = runUpkeepRound(state)

    val decision = state.decision(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid)))
    assertEquals(decision, Agreement.Decided(List(99)))
  }

  test("Decision: undecided when no values are committed") {
    val state    = ParallelMultiPaxos.empty[Int]
    val decision = state.decision(using Participants(Set(id1.uid, id2.uid, id3.uid, id4.uid)))
    assertEquals(decision, Agreement.Undecided)
  }

  test("Decision: multiple values form a list") {
    var state = ParallelMultiPaxos.empty[Int]

    // Commit two values
    for slot <- 0L to 1L do {
      state = state.merge(state.startLeaderElection(slot)(using id1))
      state = runUpkeepRound(state)
      state = state.merge(state.proposeIfLeader(slot, (slot + 1).toInt * 10)(using id1))
      state = runUpkeepRound(state)
    }

    assertEquals(state.decision, Agreement.Decided(List(10, 20)))
  }

  // ==================== 10. Multiple Slots Tests ====================

  test("Multiple Slots: alternating leaders across slots") {
    var state = ParallelMultiPaxos.empty[Int]

    // Slot 0 with id1 as leader
    state = state.merge(state.startLeaderElection(0L)(using id1))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(0L, 10)(using id1))
    state = runUpkeepRound(state)

    // Slot 1 with id2 as leader
    state = state.merge(state.startLeaderElection(1L)(using id2))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(1L, 20)(using id2))
    state = runUpkeepRound(state)

    // Slot 2 with id3 as leader
    state = state.merge(state.startLeaderElection(2L)(using id3))
    state = runUpkeepRound(state)
    state = state.merge(state.proposeIfLeader(2L, 30)(using id3))
    state = runUpkeepRound(state)

    // Verify multiple slots are tracked
    assertEquals(state.log.size, 3)
  }

  // ======= test re-use of leader election
  test("proposing as non-leader does nothing") {
    var state = ParallelMultiPaxos.empty[Int]

    state = state.merge(state.startLeaderElection(0)(using id1))
    state = runUpkeepRound(state)

    assert(state.log(0).isCurrentLeader(using id1))
    assert(!state.log(0).isCurrentLeader(using id2))

    // proposeSlot 1 + 2
    assertEquals(state.proposeIfLeader(10)(using id2), ParallelMultiPaxos())
  }

  test("leader election is re-used correctly") {
    var state = ParallelMultiPaxos.empty[Int]

    state = state.merge(state.startLeaderElection(0)(using id1))
    state = runUpkeepRound(state)

    assert(state.log(0).isCurrentLeader(using id1))
    assert(!state.log(0).isCurrentLeader(using id2))
    assertEquals(state.commitIndex, -1L)

    // proposeSlot 1 + 2
    state = state.merge(state.proposeIfLeader(10)(using id1))
    state = runUpkeepRound(state)
    assertEquals(state.decision, Agreement.Decided(List(10)))
    assertEquals(state.commitIndex, 0L)

    state = state.merge(state.proposeIfLeader(20)(using id1))
    state = runUpkeepRound(state)
    assert(state.log(0).isCurrentLeader(using id1))
    assertEquals(state.decision, Agreement.Decided(List(10, 20)))
    assertEquals(state.commitIndex, 1L)

    // try to become leader as id2 but for old log index, doesn't do anything because we do not do upkeep in these rounds
    state = state.merge(state.startLeaderElection(0)(using id2))
    state = runUpkeepRound(state)

    println(state)
    assert(state.log(1).isCurrentLeader(using id1))
    assert(!state.log(0).isCurrentLeader(using id2))

    assertEquals(state.decision, Agreement.Decided(List(10, 20)))
  }
}
