package text_rdt

import munit.ScalaCheckSuite
import org.scalacheck.Test
import text_rdt.helper.{WebDriverFixture, browser, scalacheck}

class SimpleBrowserFugueScalaCheckSuite
    extends BrowserFugueScalaCheckSuite("simple") {}

class ComplexBrowserFugueScalaCheckSuite
    extends BrowserFugueScalaCheckSuite("complex") {}

class SimpleAVLBrowserFugueScalaCheckSuite
    extends BrowserFugueScalaCheckSuite("simpleavl") {}

class ComplexAVLBrowserFugueScalaCheckSuite
    extends BrowserFugueScalaCheckSuite("complexavl") {}

abstract class BrowserFugueScalaCheckSuite(algorithm: String)
    extends ScalaCheckSuite {

  val webDriverFixture = new WebDriverFixture()

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(20)
      .withMaxSize(100)
      .withMaxDiscardRatio(0.00001)

  override def munitFixtures: Seq[WebDriverFixture] = List(webDriverFixture)

  property(
    "A browser replica should be able to do arbitrary insertions and deletions"
      .tag(browser)
      .tag(scalacheck)
  ) {
    BrowserSingleReplicaInsertDeleteTest(webDriverFixture, algorithm).property()
  }

  property(
    "All browser replicas should converge".tag(browser).tag(scalacheck)
  ) {
    BrowserMultiReplicaConvergenceTest(webDriverFixture, algorithm).property()
  }
}
