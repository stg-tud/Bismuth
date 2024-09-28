package text_rdt

import munit.ScalaCheckSuite
import org.scalacheck.Test
import text_rdt.helper.scalacheck

class SimpleInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(using SimpleFugueFactory.simpleFugueFactory) {}

class ComplexInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(using ComplexFugueFactory.complexFugueFactory) {}

class SimpleAVLInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(using SimpleAVLFugueFactory.simpleAVLFugueFactory) {}

class ComplexAVLInternalFugueScalaCheckSuite
    extends InternalFugueScalaCheckSuite(using ComplexAVLFugueFactory.complexAVLFugueFactory) {

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

abstract class InternalFugueScalaCheckSuite(
    using val factoryConstructor: FugueFactory
) extends ScalaCheckSuite {

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
    InternalSingleReplicaInsertDeleteTest()(using factoryConstructor).property()
  }

  property("All replicas should converge".tag(scalacheck)) {
    InternalMultiReplicaConvergenceTest()(using factoryConstructor).property()
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
