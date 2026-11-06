package text_rdt.avl2

import munit.ScalaCheckSuite
import org.scalacheck.Test
import text_rdt.helper.scalacheck

class AVL2TreeScalaCheckSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(10_000)
      .withWorkers(16)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  property(
    "avl".tag(scalacheck)
  ) {
    AVL2TreeBasicTest().property()
  }
}
