package ex2024textRDT.fugue.avl

import munit.ScalaCheckSuite
import org.scalacheck.Test
import ex2024textRDT.helper.scalacheck

class AVLTreeScalaCheckSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withWorkers(16)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  property(
    "avl".tag(scalacheck)
  ) {
    AVLTreeBasicTest().property()
  }
}
