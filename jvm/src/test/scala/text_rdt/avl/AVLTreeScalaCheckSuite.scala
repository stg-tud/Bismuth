package text_rdt.avl

import munit.ScalaCheckSuite
import org.scalacheck.Test
import text_rdt.helper.scalacheck

class AVLTreeScalaCheckSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(2_000)
      .withWorkers(16)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  property(
    "avl".tag(scalacheck)
  ) {
    AVLTreeBasicTest().property()
  }
}
