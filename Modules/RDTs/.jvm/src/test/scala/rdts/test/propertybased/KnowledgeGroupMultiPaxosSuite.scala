package rdts.test.propertybased

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.{Lattice, LocalUid}
import rdts.protocols.Participants
import rdts.protocols.knowledgeGroups.MultiPaxos
import rdts.protocols.spanner.ParallelMultiPaxos

import scala.util.Try

class KnowledgeGroupMultiPaxosSuite extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters: Parameters =
    StateBasedTestParameters.update(
      super.scalaCheckTestParameters
    ).withMinSize(100).withMaxSize(3000).withMinSuccessfulTests(300)

  override def scalaCheckInitialSeed = "NGDQ-T-3we8WnId8_CzemO-ytU9xi2nRJrYyIPk8yfL="

  property("Knowledge Group MultiPaxos")(KnowledgeGroupMultiPaxosSpec[Int](
    logging = false,
    minDevices = 3,
    maxDevices = 5,
    requestFreq = 5,
    startElectionFreq = 2,
    upkeepFreq = 80
  ).property())
}

class KnowledgeGroupMultiPaxosSpec[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    requestFreq: Int,
    startElectionFreq: Int,
    upkeepFreq: Int
) extends CommandsARDTs[MultiPaxos[A]] {

  override def genInitialState: Gen[State] =
    for
        numDevices <- Gen.choose(minDevices, maxDevices)
        ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, MultiPaxos())).toMap

  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (requestFreq, genRequest(state)),
      (upkeepFreq, genUpkeep(state)),
      (startElectionFreq, genStartElection(state))
    )

  def genStartElection(state: State): Gen[StartElection] =
    for
        id   <- genId(state)
        slot <- Gen.chooseNum(0, 5)
        receivers <- genIdSubset(state)
    yield StartElection(id, slot, receivers + id)

  def genRequest(state: State): Gen[Request] =
    for
        id    <- genId(state)
        value <- arbitrary[A]
        receivers <- genIdSubset(state)
    yield Request(id, value, receivers + id)

  def genUpkeep(state: State): Gen[Upkeep] =
    for
        id <- genId(state)
        receivers <- genIdSubset(state)
    yield Upkeep(id, receivers + id)

  case class Request(proposer: LocalUid, value: A, receivers: Set[LocalUid])
      extends BroadCastCommand(proposer, receivers):
      override def delta(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          states(proposer).request(value)(using proposer)

  case class Upkeep(id: LocalUid, receivers: Set[LocalUid]) extends BroadCastCommand(id, receivers):
      override def delta(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          given LocalUid = id
          states(id).upkeep

      override def postCondition(state: State, result: Try[Result]): Prop =
          given Participants(state.keySet.map(_.uid))
          val res: Map[LocalUid, MultiPaxos[A]] = result.get
          Prop.forAll(genId2(res)) {
            (index1, index2) =>
              (state(index1), state(index2), res(index1), res(index2)) match
                  case (oldMultipaxos1, oldMultipaxos2, multipaxos1, multipaxos2) =>
                    val (oldLog1, oldLog2) = (oldMultipaxos1.read, oldMultipaxos2.read)
                    val (log1, log2)       = (multipaxos1.read, multipaxos2.read)
                    (log1.isPrefix(log2) || log2.isPrefix(
                      log1
                    )) :| s"every log is a prefix of another log or vice versa, but we had:\nleft:$log1\nright:$log2" &&
                    (log1.isPrefix(oldLog1) && log2.isPrefix(oldLog2)) :| s"logs never shrink but we had $oldLog1 -> $log1, $oldLog2 -> $log2\n$oldMultipaxos2\n$multipaxos2"
//                    (log1.size < 5) :| s"logs are small but we had: $log1"
          }

  case class StartElection(initiator: LocalUid, slot: Long, receivers: Set[LocalUid])
      extends BroadCastCommand(initiator, receivers):
      override def delta(states: Map[LocalUid, MultiPaxos[A]]): MultiPaxos[A] =
          given Participants(states.keySet.map(_.uid))
          given LocalUid = initiator
          states(initiator).startLeaderElection(slot)
}
