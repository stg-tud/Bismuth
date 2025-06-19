package test.rdts

import org.scalacheck.Test.Parameters
import rdts.protocols.Paxos
import rdts.protocols.paper.Paxos as PaperPaxos

class PaperPaxosSuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters: Parameters = StateBasedTestParameters.update(super.scalaCheckTestParameters)

  property("Paxos")(ConsensusPropertySpec[Int, Paxos](
    logging = false,
    minDevices = 3,
    maxDevices = 7,
    writeFreq = 20,
    mergeFreq = 70
  ).property())

  property("Paper Paxos")(ConsensusPropertySpec[Int, PaperPaxos](
    logging = false,
    minDevices = 3,
    maxDevices = 7,
    writeFreq = 20,
    mergeFreq = 70
  ).property())
