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
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replicaState.insert(0, 'a')
    exportGraph(replicaState.state.rootTreeNode.buildTree(), "root-right-a")
  }

  test(
    "root-right-ac".tag(browser)
  ) {
    val replicaState =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    exportGraph(replicaState.state.rootTreeNode.buildTree(), "root-right-ac")
  }

  test(
    "root-right-ac-left-b".tag(browser)
  ) {
    val replicaState =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replicaState.insert(0, 'a')
    replicaState.insert(1, 'c')
    replicaState.insert(1, 'b')
    exportGraph(replicaState.state.rootTreeNode.buildTree(), "root-right-ac-left-b")
  }

  test(
    "traversal-example".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 't')
    replicaA.insert(1, 'r')
    replicaA.insert(2, 'e')
    replicaA.insert(3, 'e')
    replicaA.insert(4, 's')
    replicaB.insert(0, '␣')
    replicaB.insert(1, 'g')
    replicaB.insert(2, 'r')
    replicaB.insert(3, 'o')
    replicaB.insert(4, 'w')
    replicaA.sync(replicaB)
    replicaA.insert(0, 's')
    replicaA.insert(1, 'm')
    replicaA.insert(2, 'a')
    replicaA.insert(3, 'l')
    replicaA.insert(4, 'l')
    replicaA.insert(5, '␣')
    exportGraph(replicaStateA.rootTreeNode.buildTree(), "traversal-example")
  }

  test(
    "concurrent-insert-example".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'a')
    replicaA.insert(1, 'l')
    replicaA.insert(2, 'i')
    replicaA.insert(3, 'c')
    replicaA.insert(4, 'e')
    replicaB.insert(0, 'b')
    replicaB.insert(1, 'o')
    replicaB.insert(2, 'b')
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
    val replicaC = Replica(replicaStateC, StringEditory())
    val replicaStateA =
      ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaC.insert(0, 'S')
    replicaC.insert(1, 'h')
    replicaC.insert(2, 'o')
    replicaC.insert(3, 'p')
    replicaC.insert(4, 'p')
    replicaC.insert(5, 'i')
    replicaC.insert(6, 'n')
    replicaC.insert(7, 'g')

    replicaC.sync(replicaA)
    replicaC.sync(replicaB)

    replicaA.insert(8, '*')
    replicaA.insert(9, ' ')
    replicaA.insert(10, 'a')
    replicaA.insert(11, 'p')
    replicaA.insert(12, 'p')
    replicaA.insert(13, 'l')
    replicaA.insert(14, 'e')
    replicaA.insert(15, 's')

    replicaA.insert(8, 'F')
    replicaA.insert(9, 'r')
    replicaA.insert(10, 'u')
    replicaA.insert(11, 'i')
    replicaA.insert(12, 't')
    replicaA.insert(13, ':')

    replicaB.insert(8, '*')
    replicaB.insert(9, ' ')
    replicaB.insert(10, 'b')
    replicaB.insert(11, 'r')
    replicaB.insert(12, 'e')
    replicaB.insert(13, 'a')
    replicaB.insert(14, 'd')

    replicaB.insert(8, 'B')
    replicaB.insert(9, 'a')
    replicaB.insert(10, 'k')
    replicaB.insert(11, 'e')
    replicaB.insert(12, 'r')
    replicaB.insert(13, 'y')
    replicaB.insert(14, ':')

    replicaA.sync(replicaB)

    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "shopping"
    )
  }

  test(
    "delete".tag(browser)
  ) {
    val replica =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replica.insert(0, 'a')
    replica.insert(1, 'c')
    replica.insert(1, 'b')
    replica.delete(1)
    exportGraph(replica.state.rootTreeNode.buildTree(), "delete")
  }

  test(
    "sequential-inserts".tag(browser)
  ) {
    val replica =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replica.insert(0, 'a')
    replica.insert(1, 'c')
    replica.insert(2, 'b')
    exportGraph(replica.state.rootTreeNode.buildTree(), "sequential-inserts")
  }

  test(
    "reverse-sequential-inserts".tag(browser)
  ) {
    val replica =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replica.insert(0, 'c')
    replica.insert(0, 'b')
    replica.insert(0, 'a')
    exportGraph(
      replica.state.rootTreeNode.buildTree(),
      "reverse-sequential-inserts"
    )
  }

  test(
    "efficiency-avl-before".tag(browser)
  ) {
    val replica =
      Replica(ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      ), StringEditory())
    replica.insert(0, 'l')
    replica.insert(1, 'o')
    replica.insert(2, 'n')
    replica.insert(3, 'g')
    replica.insert(4, 't')
    replica.insert(5, 'e')
    replica.insert(6, 'x')
    replica.insert(7, 't')
    exportGraph(replica.state.rootTreeNode.buildTree(), "efficiency-avl-before")
  }

  test(
    "efficiency-avl".tag(browser)
  ) {
    val replicaStateA =
      ReplicaState("A")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'l')
    replicaA.insert(1, 'o')
    replicaA.insert(2, 'n')
    replicaA.insert(3, 'g')
    replicaA.insert(4, 't')
    replicaA.insert(5, 'e')
    replicaA.insert(6, 'x')
    replicaA.insert(7, 't')
    replicaB.insert(0, '.')
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
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'S')
    replicaA.insert(1, 'h')
    replicaA.insert(2, 'o')
    replicaA.insert(3, 'p')
    replicaA.insert(4, 'p')
    replicaA.insert(5, 'i')
    replicaA.insert(6, 'n')
    replicaA.insert(7, 'g')
    exportGraph(
      replicaStateA.rootTreeNode.buildTree(),
      "fugue-is-broken-0"
    )
    replicaA.sync(replicaB)
    replicaB.insert(8, '*')
    replicaB.insert(9, 'b')
    replicaB.insert(10, 'r')
    replicaB.insert(11, 'e')
    replicaB.insert(12, 'a')
    replicaB.insert(13, 'd')
    replicaB.delete(7)
    replicaB.insert(7, 'g')
    replicaB.insert(8, 'B')
    replicaB.insert(9, 'a')
    replicaB.insert(10, 'k')
    replicaB.insert(11, 'e')
    replicaB.insert(12, 'r')
    replicaB.insert(13, 'y')
    replicaB.insert(14, ':')

    replicaA.insert(8, '*')
    replicaA.insert(9, 'a')
    replicaA.insert(10, 'p')
    replicaA.insert(11, 'p')
    replicaA.insert(12, 'l')
    replicaA.insert(13, 'e')
    replicaA.insert(14, 's')
    replicaA.insert(8, 'F')
    replicaA.insert(9, 'r')
    replicaA.insert(10, 'u')
    replicaA.insert(11, 'i')
    replicaA.insert(12, 't')
    replicaA.insert(13, ':')

    exportGraph(
      replicaA.state.rootTreeNode.buildTree(),
      "fugue-is-broken-1a"
    )
    exportGraph(
      replicaB.state.rootTreeNode.buildTree(),
      "fugue-is-broken-1b"
    )

    replicaA.sync(replicaB)

    exportGraph(
      replicaA.state.rootTreeNode.buildTree(),
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
    val replicaA = Replica(replicaStateA, StringEditory())
    val replicaStateB =
      ReplicaState("B")(using
        SimpleAVLFugueFactory.simpleAVLFugueFactory
      )
    val replicaB = Replica(replicaStateB, StringEditory())
    replicaA.insert(0, 'm')
    replicaA.insert(1, 'i')
    replicaA.insert(2, 'l')
    replicaA.insert(3, 'k')
    replicaA.sync(replicaB)
    replicaA.insert(4, 'e')
    replicaA.insert(5, 'g')
    replicaA.insert(6, 'g')
    replicaA.insert(7, 's')
    replicaB.insert(4, 'b')
    replicaB.insert(5, 'r')
    replicaB.insert(6, 'e')
    replicaB.insert(7, 'a')
    replicaB.insert(8, 'd')
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
      Replica(ReplicaState("A")(using
        ComplexAVLFugueFactory.complexAVLFugueFactory
      ), StringEditory())
    MyRealWorldBenchmark.operations.view
      .slice(0, size)
      .foreach {
        case FixtureOperation.Insert(position, character) =>
          replicaStateA.insert(position, character)
        case FixtureOperation.Delete(position) =>
          replicaStateA.delete(position)
      }
    exportGraph(
      replicaStateA.state.rootTreeNode.buildTree(),
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
