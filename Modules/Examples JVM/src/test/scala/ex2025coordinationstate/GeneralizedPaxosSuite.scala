package ex2025coordinationstate

import org.scalacheck.Test.Parameters
import rdts.protocols.old.simplified.GeneralizedPaxos

object StateBasedTestParameters {
  def update(param: Parameters): Parameters = param
    .withMinSize(30)
    .withMaxSize(200)
}

class GeneralizedPaxosSuite extends munit.ScalaCheckSuite {

//  override def scalaCheckInitialSeed = "ZcBq5Oa3t8-hWG0Snkx22h6nivxFRCvp27NO4tFKzbN="

  override def scalaCheckTestParameters: Parameters = StateBasedTestParameters.update(super.scalaCheckTestParameters)

  property("Generalized Paxos")(ConsensusPropertySpec[Int, GeneralizedPaxos](
    logging = false,
    minDevices = 3,
    maxDevices = 7,
    writeFreq = 20,
    mergeFreq = 70
  ).property())
}
