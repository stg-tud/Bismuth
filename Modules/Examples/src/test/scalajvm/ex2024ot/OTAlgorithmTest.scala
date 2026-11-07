package ex2024ot

import ex2024textRDT.fugue.{InternalFugueScalaCheckSuite, InternalFugueTestSuite}

class OTAlgorithmTestSuite extends InternalFugueTestSuite(replicaId => OTAlgorithm(replicaId, Vector.empty)) {
  override protected def convergesForArbitraryHistories: Boolean = false
}

class OTAlgorithmScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => OTAlgorithm(replicaId, Vector.empty)) {
  override protected def convergesForArbitraryHistories: Boolean = false
}
