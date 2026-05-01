package ex2026overlaydemo

import replication.research.OverlayConnectionDirectory

/** vibecoded. dont trust 😉 */
class OverlayDemoTest extends munit.FunSuite {

  private def eventually(timeoutMs: Long = 10000)(cond: => Boolean): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while System.currentTimeMillis() < deadline && !cond do
      Thread.sleep(20)
    assert(cond)
  }

  test("nodes replicate self connection info and payload state through plumtree") {
    val node1 = new OverlayDemo.NodeApp()
    val node2 = new OverlayDemo.NodeApp(seeds = List(node1.details))

    try {
      eventually() {
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty
      }

      eventually() {
        val directory1 = node1.node.connectionDirectory
        val directory2 = node2.node.connectionDirectory
        Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory1.keySet) &&
        Set(node1.node.localUid.uid, node2.node.localUid.uid).subsetOf(directory2.keySet) &&
        directory1.get(node1.node.localUid.uid).exists(_.selfDetails.elements.contains(node1.details)) &&
        directory1.get(node2.node.localUid.uid).exists(_.selfDetails.elements.contains(node2.details)) &&
        directory2.get(node1.node.localUid.uid).exists(_.selfDetails.elements.contains(node1.details)) &&
        directory2.get(node2.node.localUid.uid).exists(_.selfDetails.elements.contains(node2.details))
      }

      node1.node.publishAdd("server-value")
      node2.node.publishAdd("client-value")

      eventually() {
        val expected = Set("server-value", "client-value")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected
      }

      node2.node.publishRemove("server-value")

      eventually() {
        val expected = Set("client-value")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected
      }
    } finally {
      node2.stop()
      node1.stop()
    }
  }

  test("nodes learn the whole network from replicated local views and can use that for further overlay connectivity") {
    val node1 = new OverlayDemo.NodeApp()
    val node2 = new OverlayDemo.NodeApp(seeds = List(node1.details))
    val node3 = new OverlayDemo.NodeApp(seeds = List(node2.details))

    try {
      eventually() {
        node1.node.activeView.nonEmpty && node2.node.activeView.nonEmpty && node3.node.activeView.nonEmpty
      }

      eventually(15000) {
        val expected = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
        val directories = List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
        directories.forall { directory =>
          expected.subsetOf(directory.keySet) &&
          expected.forall { uid =>
            directory.get(uid).exists(_.selfDetails.elements.nonEmpty)
          }
        }
      }

      eventually(15000) {
        val directory = node3.node.connectionDirectory
        directory.get(node1.node.localUid.uid).exists(_.selfDetails.elements.contains(node1.details)) &&
        directory.get(node2.node.localUid.uid).exists(_.selfDetails.elements.contains(node2.details)) &&
        directory.get(node3.node.localUid.uid).exists(_.selfDetails.elements.contains(node3.details))
      }

      eventually(20000) {
        val allNodes = Set(node1.node.localUid.uid, node2.node.localUid.uid, node3.node.localUid.uid)
        List(node1, node2, node3).forall { app =>
          val knownActiveOrPassive = app.node.activeView ++ app.node.passiveView + app.node.localUid.uid
          allNodes.subsetOf(knownActiveOrPassive)
        }
      }

      node1.handleInputLine("alpha")
      node2.handleInputLine("beta")
      assert(node3.handleInputLine("gamma"))
      assert(!node3.handleInputLine("q"))

      eventually() {
        val expected = Set("alpha", "beta", "gamma")
        node1.node.state.values.elements == expected &&
        node2.node.state.values.elements == expected &&
        node3.node.state.values.elements == expected
      }

      val directories = List(node1.node.connectionDirectory, node2.node.connectionDirectory, node3.node.connectionDirectory)
      assert(directories.forall(_.entries.forall((_, info) => info.selfDetails.elements.nonEmpty)))
      assert(directories.exists(_.entries.exists((_, info) => info.peers.elements.exists(_.state == OverlayConnectionDirectory.LinkState.Active))))
    } finally {
      node3.stop()
      node2.stop()
      node1.stop()
    }
  }
}
