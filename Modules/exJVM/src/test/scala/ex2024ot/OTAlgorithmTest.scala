package ex2024ot

import text_rdt.{InternalFugueScalaCheckSuite, InternalFugueTestSuite}

class OTAlgorithmTestSuite extends InternalFugueTestSuite(replicaId => OTAlgorithm(replicaId, Vector.empty)) {
  override protected def convergesForArbitraryHistories: Boolean = false
}

class OTAlgorithmScalaCheckSuite
    extends InternalFugueScalaCheckSuite(replicaId => OTAlgorithm(replicaId, Vector.empty)) {
  override protected def convergesForArbitraryHistories: Boolean = false
}
