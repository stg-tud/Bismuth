package rdts.test.propertybased

import org.scalacheck.Test.Parameters
import rdts.protocols.Paxos

class PaxosSuite extends munit.ScalaCheckSuite:

    override def scalaCheckTestParameters: Parameters = StateBasedTestParameters.update(super.scalaCheckTestParameters)

    property("Paxos")(ConsensusPropertySpec[Int, Paxos](
      logging = false,
      minDevices = 3,
      maxDevices = 7,
      writeFreq = 20,
      mergeFreq = 70
    ).property())
