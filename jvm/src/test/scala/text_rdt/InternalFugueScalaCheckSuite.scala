package text_rdt

import munit.ScalaCheckSuite
import org.scalacheck.Test
import text_rdt.helper.scalacheck

class OTAlgorithmScalaCheckSuite extends InternalFugueScalaCheckSuite(replicaId => OTAlgorithm(replicaId, Vector.empty)) {

}

object OTAlgorithmScalaCheckSuite {}

class SimpleInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => Replica(ReplicaState(replicaId)(using SimpleFugueFactory.simpleFugueFactory), StringEditory())) {}

class ComplexInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => Replica(ReplicaState(replicaId)(using ComplexFugueFactory.complexFugueFactory), StringEditory())) {}

class SimpleAVLInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => Replica(ReplicaState(replicaId)(using SimpleAVLFugueFactory.simpleAVLFugueFactory), StringEditory())) {}

class ComplexAVLInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => Replica(ReplicaState(replicaId)(using ComplexAVLFugueFactory.complexAVLFugueFactory), StringEditory())) {

  override protected def scalaCheckInitialSeed: String =
    "EW9fMG9YY_yS7U_xrPdYhnQaD0XdHdBj2miRsRsemXC="

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(500_000)
      .withWorkers(16)
      .withMaxSize(
        50
      )
      .withMaxDiscardRatio(0.00001)

}

abstract class InternalFugueScalaCheckSuite[A](
    val factoryConstructor: String => A
)(using algorithm: CollaborativeTextEditingAlgorithm[A]) extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(20_000)
      .withWorkers(16)
      .withMaxSize(
        50
      )
      .withMaxDiscardRatio(0.00001)

  property(
    "A replica should be able to do arbitrary insertions and deletions".tag(
      scalacheck
    )
  ) {
    InternalSingleReplicaInsertDeleteTest(factoryConstructor).property()
  }

  property("All replicas should converge".tag(scalacheck)) {
    assume(this.getClass().getName() != "text_rdt.OTAlgorithmScalaCheckSuite")
    InternalMultiReplicaConvergenceTest(factoryConstructor).property()
  }

  /*property("All replicas should follow the strong list specification".tag(scalacheck)) {
    InternalMultiReplicaStrongListSpecificationTest()(using factoryConstructor).property()
  }*/
}

class InternalFugueTreeEqualityCheckSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(10_000)
      .withWorkers(16)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  property(
    "Different Fugue implementations create the same tree (simple, complex)"
      .tag(scalacheck)
  ) {
    InternalFugueTreeEqualityTest(
      Array(
        () => {
          SimpleFugueFactory.simpleFugueFactory
        },
        () => {
          ComplexFugueFactory.complexFugueFactory
        }
      )
    ).property()
  }

  property(
    "Different Fugue implementations create the same tree (simple, simpleavl)"
      .tag(scalacheck)
  ) {
    InternalFugueTreeEqualityTest(
      Array(
        () => {
          SimpleFugueFactory.simpleFugueFactory
        },
        () => {
          SimpleAVLFugueFactory.simpleAVLFugueFactory
        }
      )
    ).property()
  }

  property(
    "Different Fugue implementations create the same tree (simple, complexavl)"
      .tag(scalacheck)
  ) {
    InternalFugueTreeEqualityTest(
      Array(
        () => {
          SimpleFugueFactory.simpleFugueFactory
        },
        () => {
          ComplexAVLFugueFactory.complexAVLFugueFactory
        }
      )
    ).property()
  }
}
