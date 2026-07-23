package rdts.test.propertybased

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.{Lattice, LocalUid}
import rdts.protocols.MultipaxosPhase.LeaderElection
import rdts.protocols.Participants

import scala.util.Try
import rdts.protocols.spanner.ParallelMultiPaxos

class ParallelMultiPaxosSuite extends munit.ScalaCheckSuite {
  override def scalaCheckInitialSeed                = "T1dyTdzixur4RPmBXyJPMc4b097AtKu25e_AKKUKdQO="
  override def scalaCheckTestParameters: Parameters =
    StateBasedTestParameters.update(
      super.scalaCheckTestParameters
    ).withMinSize(100).withMaxSize(4000).withMinSuccessfulTests(200)

  property("ParallelMultiPaxos")(ParallelMultiPaxosSpec[Int](
    logging = true,
    minDevices = 3,
    maxDevices = 5,
    proposeFreq = 5,
    startElectionFreq = 5,
    mergeFreq = 80
  ).property())
}

class ParallelMultiPaxosSpec[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    proposeFreq: Int,
    startElectionFreq: Int,
    mergeFreq: Int
) extends CommandsARDTs[ParallelMultiPaxos[A]] {

  override def genInitialState: Gen[State] =
    for
        numDevices <- Gen.choose(minDevices, maxDevices)
        ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, ParallelMultiPaxos())).toMap

  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (mergeFreq, genMerge(state)),
      (proposeFreq, genPropose(state)),
      (startElectionFreq, genStartElection(state))
    )

  def genPropose(state: State): Gen[Propose] =
    for
        id    <- genId(state)
        value <- arbitrary[A]
        slot  <- Gen.chooseNum(0, 5)
    yield Propose(id, value, slot)

  def genStartElection(state: State): Gen[StartElection] =
    for
        id   <- genId(state)
        slot <- Gen.chooseNum(0, 5)
    yield StartElection(id, slot)

  def genMerge(state: State): Gen[Merge] =
    for
        (left, right) <- genId2(state)
    yield Merge(left, right)

  // commands: (merge), upkeep, write, addMember, removeMember
  case class Merge(left: LocalUid, right: LocalUid) extends ACommand(left):
      override def nextLocalState(states: Map[LocalUid, ParallelMultiPaxos[A]]): ParallelMultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          val merged = states(left).merge(states(right))
          val result = merged.merge(merged.upkeep(using left))

          if logging then
              if result.read.length > 4 then
                  println(result)
          result

      override def postCondition(state: State, result: Try[Result]): Prop =
          given Participants(state.keySet.map(_.uid))
          val res: Map[LocalUid, ParallelMultiPaxos[A]] = result.get
          Prop.forAll(genId2(res)) {
            (index1, index2) =>
              (state(index1), state(index2), res(index1), res(index2)) match
                  case (oldMultipaxos1, oldMultipaxos2, multipaxos1, multipaxos2) =>
                    val (log1, log2)       = (multipaxos1.read, multipaxos2.read)
                    val (oldLog1, oldLog2) = (oldMultipaxos1.read, oldMultipaxos2.read)
                    (log1.isPrefix(log2) || log2.isPrefix(
                      log1
                    )) :| s"every log is a prefix of another log or vice versa, but we had:\nleft:${multipaxos1.read}\nright:${multipaxos2.read}" &&
                    // ((multipaxos1.rounds.counter != multipaxos2.rounds.counter) || multipaxos1.leader.isEmpty || multipaxos2.leader.isEmpty || (multipaxos1.leader == multipaxos2.leader)) :| s"There can only ever be one leader for a given epoch but we got:\n${multipaxos1.leader}\n${multipaxos2.leader}" &&
                    (log1.isPrefix(oldLog1) && log2.isPrefix(oldLog2)) :| "logs never shrink"
          }

  case class Propose(proposer: LocalUid, value: A, slot: Long) extends ACommand(proposer):
      override def nextLocalState(states: Map[LocalUid, ParallelMultiPaxos[A]]): ParallelMultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          val delta    = states(proposer).proposeIfLeader(slot, value)(using proposer)
          val proposed = Lattice.merge(states(proposer), delta)
          Lattice.merge(proposed, proposed.upkeep(using proposer))

  case class StartElection(initiator: LocalUid, slot: Long) extends ACommand(initiator):
      override def nextLocalState(states: Map[LocalUid, ParallelMultiPaxos[A]]): ParallelMultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          val delta  = states(initiator).startLeaderElection(slot)(using initiator)
          val merged = Lattice.merge(states(initiator), delta)
          Lattice.merge(merged, merged.upkeep(using initiator))
}
