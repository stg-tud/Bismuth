package ex2024textRDT.fugue.avl2

import munit.ScalaCheckSuite
import org.scalacheck.Test
import ex2024textRDT.helper.scalacheck

class AVL2TreeScalaCheckSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withWorkers(16)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  property(
    "avl".tag(scalacheck)
  ) {
    AVL2TreeBasicTest().property()
  }
}
