package text_rdt

import munit.FunSuite
import text_rdt.helper.{WebDriverFixture, browser}

import java.nio.file.Paths
import com.microsoft.playwright.Page
import java.util.UUID
import scala.concurrent.duration.Duration
import com.microsoft.playwright.Locator

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import upickle.default._
import com.microsoft.playwright.Locator.ScreenshotOptions

class ThesisTestSuite extends FunSuite {

  override val munitTimeout = Duration(300, "s")

  val webDriverFixture = new WebDriverFixture()
  override def munitFixtures: Seq[WebDriverFixture] = List(webDriverFixture)

  def exportGraph(
      data: MyD3TreeNode,
      filename: String
  ): Unit = {
    val page = webDriverFixture.getOrCreateWebDriver()
    try {
      val _ = page.navigate(
        s"http://localhost:5173/?tree=${URLEncoder.encode(write(data), StandardCharsets.UTF_8)}"
      )
      val _ = page.pdf(
        new Page.PdfOptions()
          .setPreferCSSPageSize(true)
          .nn
          .setTagged(true)
          .nn
          .setOutline(true)
          .nn
          .setPath(Paths.get(s"target/pdfs/$filename.pdf"))
      )
    } finally {
      webDriverFixture.giveBack(page, UUID.randomUUID().nn)
    }
  }

  test(
    "empty".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    exportGraph(replicaState.rootTreeNode.buildTree(), "empty")
  }

  test(
    "root-right-a".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'a')
    exportGraph(replicaState.rootTreeNode.buildTree(), "root-right-a")
  }

  test(
    "root-right-ac".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    exportGraph(replicaState.rootTreeNode.buildTree(), "root-right-ac")
  }

  test(
    "root-right-ac-left-b".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    replicaState.insert(1, 'b')
    exportGraph(replicaState.rootTreeNode.buildTree(), "root-right-ac-left-b")
  }

  test(
    "traversal-example".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 't')
    replicaStateA.insert(1, 'r')
    replicaStateA.insert(2, 'e')
    replicaStateA.insert(3, 'e')
    replicaStateA.insert(4, 's')
    replicaStateB.insert(0, '␣')
    replicaStateB.insert(1, 'g')
    replicaStateB.insert(2, 'r')
    replicaStateB.insert(3, 'o')
    replicaStateB.insert(4, 'w')
    replicaA.sync(replicaB)
    replicaStateA.insert(0, 's')
    replicaStateA.insert(1, 'm')
    replicaStateA.insert(2, 'a')
    replicaStateA.insert(3, 'l')
    replicaStateA.insert(4, 'l')
    replicaStateA.insert(5, '␣')
    exportGraph(replicaStateA.rootTreeNode.buildTree(), "traversal-example")
  }

  test(
    "concurrent-insert-example".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'a')
    replicaStateA.insert(1, 'l')
    replicaStateA.insert(2, 'i')
    replicaStateA.insert(3, 'c')
    replicaStateA.insert(4, 'e')
    replicaStateB.insert(0, 'b')
    replicaStateB.insert(1, 'o')
    replicaStateB.insert(2, 'b')
    exportGraph(replicaStateA.rootTreeNode.buildTree(), "concurrent-insert-a")
    exportGraph(replicaStateB.rootTreeNode.buildTree(), "concurrent-insert-b")
    replicaA.sync(replicaB)
    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "concurrent-insert-both"
    )
  }

  test(
    "shopping".tag(browser)
  ) {
    val replicaStateC =
      ReplicaState("C")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaC = Replica(replicaStateC, NoopEditory())
    val replicaStateA =
      ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateC.insert(0, 'S')
    replicaStateC.insert(1, 'h')
    replicaStateC.insert(2, 'o')
    replicaStateC.insert(3, 'p')
    replicaStateC.insert(4, 'p')
    replicaStateC.insert(5, 'i')
    replicaStateC.insert(6, 'n')
    replicaStateC.insert(7, 'g')

    replicaC.sync(replicaA)
    replicaC.sync(replicaB)

    replicaStateA.insert(8, '*')
    replicaStateA.insert(9, ' ')
    replicaStateA.insert(10, 'a')
    replicaStateA.insert(11, 'p')
    replicaStateA.insert(12, 'p')
    replicaStateA.insert(13, 'l')
    replicaStateA.insert(14, 'e')
    replicaStateA.insert(15, 's')

    replicaStateA.insert(8, 'F')
    replicaStateA.insert(9, 'r')
    replicaStateA.insert(10, 'u')
    replicaStateA.insert(11, 'i')
    replicaStateA.insert(12, 't')
    replicaStateA.insert(13, ':')

    replicaStateB.insert(8, '*')
    replicaStateB.insert(9, ' ')
    replicaStateB.insert(10, 'b')
    replicaStateB.insert(11, 'r')
    replicaStateB.insert(12, 'e')
    replicaStateB.insert(13, 'a')
    replicaStateB.insert(14, 'd')

    replicaStateB.insert(8, 'B')
    replicaStateB.insert(9, 'a')
    replicaStateB.insert(10, 'k')
    replicaStateB.insert(11, 'e')
    replicaStateB.insert(12, 'r')
    replicaStateB.insert(13, 'y')
    replicaStateB.insert(14, ':')

    replicaA.sync(replicaB)

    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "shopping"
    )
  }

  test(
    "delete".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    replicaState.insert(1, 'b')
    replicaState.delete(1)
    exportGraph(replicaState.rootTreeNode.buildTree(), "delete")
  }

  test(
    "sequential-inserts".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    replicaState.insert(2, 'b')
    exportGraph(replicaState.rootTreeNode.buildTree(), "sequential-inserts")
  }

  test(
    "reverse-sequential-inserts".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'c')
    replicaState.insert(0, 'b')
    replicaState.insert(0, 'a')
    exportGraph(
      replicaState.rootTreeNode.buildTree(),
      "reverse-sequential-inserts"
    )
  }

  test(
    "efficiency-avl-before".tag(browser)
  ) {
    val replicaState =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    replicaState.insert(0, 'l')
    replicaState.insert(1, 'o')
    replicaState.insert(2, 'n')
    replicaState.insert(3, 'g')
    replicaState.insert(4, 't')
    replicaState.insert(5, 'e')
    replicaState.insert(6, 'x')
    replicaState.insert(7, 't')
    exportGraph(replicaState.rootTreeNode.buildTree(), "efficiency-avl-before")
  }

  test(
    "efficiency-avl".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'l')
    replicaStateA.insert(1, 'o')
    replicaStateA.insert(2, 'n')
    replicaStateA.insert(3, 'g')
    replicaStateA.insert(4, 't')
    replicaStateA.insert(5, 'e')
    replicaStateA.insert(6, 'x')
    replicaStateA.insert(7, 't')
    replicaStateB.insert(0, '.')
    replicaA.sync(replicaB)
    exportGraph(replicaStateA.rootTreeNode.buildTree(), "efficiency-avl")
  }

  test(
    "fugue-is-broken".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'S')
    replicaStateA.insert(1, 'h')
    replicaStateA.insert(2, 'o')
    replicaStateA.insert(3, 'p')
    replicaStateA.insert(4, 'p')
    replicaStateA.insert(5, 'i')
    replicaStateA.insert(6, 'n')
    replicaStateA.insert(7, 'g')
    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "fugue-is-broken-0"
    )
    replicaA.sync(replicaB)
    replicaStateB.insert(8, '*')
    replicaStateB.insert(9, 'b')
    replicaStateB.insert(10, 'r')
    replicaStateB.insert(11, 'e')
    replicaStateB.insert(12, 'a')
    replicaStateB.insert(13, 'd')
    replicaStateB.delete(7)
    replicaStateB.insert(7, 'g')
    replicaStateB.insert(8, 'B')
    replicaStateB.insert(9, 'a')
    replicaStateB.insert(10, 'k')
    replicaStateB.insert(11, 'e')
    replicaStateB.insert(12, 'r')
    replicaStateB.insert(13, 'y')
    replicaStateB.insert(14, ':')

    replicaStateA.insert(8, '*')
    replicaStateA.insert(9, 'a')
    replicaStateA.insert(10, 'p')
    replicaStateA.insert(11, 'p')
    replicaStateA.insert(12, 'l')
    replicaStateA.insert(13, 'e')
    replicaStateA.insert(14, 's')
    replicaStateA.insert(8, 'F')
    replicaStateA.insert(9, 'r')
    replicaStateA.insert(10, 'u')
    replicaStateA.insert(11, 'i')
    replicaStateA.insert(12, 't')
    replicaStateA.insert(13, ':')

    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "fugue-is-broken-1a"
    )
    exportGraph(
      replicaStateB.rootTreeNode.buildTree(),
      "fugue-is-broken-1b"
    )

    replicaA.sync(replicaB)

    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "fugue-is-broken-2"
    )
  }

  test(
    "2023-weidner-minimizing-interleaving-figure-1".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, NoopEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, NoopEditory())
    replicaStateA.insert(0, 'm')
    replicaStateA.insert(1, 'i')
    replicaStateA.insert(2, 'l')
    replicaStateA.insert(3, 'k')
    replicaA.sync(replicaB)
    replicaStateA.insert(4, 'e')
    replicaStateA.insert(5, 'g')
    replicaStateA.insert(6, 'g')
    replicaStateA.insert(7, 's')
    replicaStateB.insert(4, 'b')
    replicaStateB.insert(5, 'r')
    replicaStateB.insert(6, 'e')
    replicaStateB.insert(7, 'a')
    replicaStateB.insert(8, 'd')
    replicaA.sync(replicaB)
    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "2023-weidner-minimizing-interleaving-figure-1"
    )
  }

  test(
    "real-world".tag(browser)
  ) {
    val size = 1000
    val replicaStateA =
      ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    MyRealWorldBenchmark.operations.view
      .slice(0, size)
      .foreach {
        case FixtureOperation.Insert(position, character) =>
          replicaStateA.insert(position, character)
        case FixtureOperation.Delete(position) =>
          replicaStateA.delete(position)
      }
    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "real-world"
    )
  }

  test(
    "evil-children".tag(browser)
  ) {
    val bench = MyTerribleBenchmarkEvilInsert()

    {
      bench.until = 2
      bench.count = 3
      val replica = bench.evilChildrenComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-children-before"
      )
    }

    {
      bench.until = 3
      bench.count = 3
      val replica = bench.evilChildrenComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-children-after"
      )
    }
  }

  test(
    "evil-insert-1".tag(browser)
  ) {
    val bench = MyTerribleBenchmarkEvilInsert()

    {
      bench.until = 2
      bench.count = 3
      val replica = bench.evilInsert1ComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-insert-1-before"
      )
    }

    {
      bench.until = 3
      bench.count = 3
      val replica = bench.evilInsert1ComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-insert-1-after"
      )
    }
  }

  test(
    "evil-insert-2".tag(browser)
  ) {
    val bench = MyTerribleBenchmarkEvilInsert()

    {
      bench.until = 0
      bench.count = 3
      val replica = bench.evilInsert2ComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-insert-2-before"
      )
    }

    {
      bench.until = 2
      bench.count = 3
      val replica = bench.evilInsert2ComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-insert-2-after"
      )
    }
  }

  test(
    "evil-split".tag(browser)
  ) {
    val bench = MyTerribleBenchmarkEvilInsert()

    {
      bench.until = 0
      bench.count = 3
      val replica = bench.evilSplitComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-split-before"
      )
    }

    {
      bench.until = 3
      bench.count = 3
      val replica = bench.evilSplitComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-split-after"
      )
    }
  }

  test(
    "evil-split-many-right-children".tag(browser)
  ) {
    val bench = MyTerribleBenchmarkEvilInsert()

    {
      bench.until = 0
      bench.count = 2
      val replica =
        bench.evilSplitManyRightChildrenComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-split-many-right-children-before"
      )
    }

    {
      bench.until = 2
      bench.count = 2
      val replica =
        bench.evilSplitManyRightChildrenComplexAVL(text_rdt.Counters.Counters())
      exportGraph(
        complexAVLTreeNodeD3Tree.buildTree(
          replica.state.rootTreeNode
            .asInstanceOf[ComplexAVLTreeNodeSingle]
        )(),
        "evil-split-many-right-children-after"
      )
    }
  }

  test("html-to-pdf".tag(browser)) {
    val driver = webDriverFixture.getOrCreateWebDriver()

    try {
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyTerribleBenchmarkSequentialInserts.sequentialInserts-Throughput-count-20000-factoryConstructor-simple-shouldMeasureMemory-false/cpu.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/simple-sequential-inserts-cpu.pdf")
          )
        )
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyTerribleBenchmarkSequentialInserts.sequentialInserts-Throughput-count-10000000-factoryConstructor-complex-shouldMeasureMemory-false/cpu.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/complex-sequential-inserts-cpu.pdf")
          )
        )
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyTerribleBenchmarkSequentialInserts.sequentialInserts-Throughput-count-10000000-factoryConstructor-complex-shouldMeasureMemory-false/alloc.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/complex-sequential-inserts-alloc.pdf")
          )
        )
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyRealWorldBenchmarkLocal.local-Throughput-count-20000-factoryConstructor-complex-shouldMeasureMemory-false/cpu.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/complex-real-world-cpu.pdf")
          )
        )
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyRealWorldBenchmarkLocal.local-Throughput-count-259778-factoryConstructor-simpleavl-shouldMeasureMemory-false/cpu.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/simpleavl-real-world-cpu.pdf")
          )
        )
      driver.navigate(
        Paths
          .get(
            "jvm/figure-benchmark-results/text_rdt.MyRealWorldBenchmarkLocal.local-Throughput-count-259778-factoryConstructor-simpleavl-shouldMeasureMemory-false/alloc.html"
          )
          .nn
          .toUri
          .toString()
      )
      driver
        .locator("#canvas")
        .nn
        .screenshot(
          ScreenshotOptions().setPath(
            Paths.get(s"target/pdfs/simpleavl-real-world-alloc.pdf")
          )
        )
    } finally {
      webDriverFixture.giveBack(driver, UUID.randomUUID().nn)
    }
  }
}
