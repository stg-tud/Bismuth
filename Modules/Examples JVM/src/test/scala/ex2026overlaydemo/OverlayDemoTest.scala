package ex2026overlaydemo

class OverlayDemoTest extends munit.FunSuite {

  private def eventually(timeoutMs: Long = 5000)(cond: => Boolean): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while System.currentTimeMillis() < deadline && !cond do
      Thread.sleep(20)
    assert(cond)
  }

  test("server and client can exchange replicated set updates over multiplexed hyparview/plumtree") {
    val server = new OverlayDemo.ServerApp("demo-topic")
    val client = new OverlayDemo.ClientApp(server.coordinationDetails, "demo-topic")

    try {
      client.start()

      eventually() {
        server.node.activeView.nonEmpty && client.node.activeView.nonEmpty
      }

      server.node.publishAdd("server-value")
      client.node.publishAdd("client-value")

      eventually() {
        server.node.state.elements == Set("server-value", "client-value") &&
        client.node.state.elements == Set("server-value", "client-value")
      }

      client.node.publishRemove("server-value")

      eventually() {
        server.node.state.elements == Set("client-value") &&
        client.node.state.elements == Set("client-value")
      }
    } finally {
      client.stop()
      server.stop()
    }
  }

  test("overlay demo supports multiple clients and stdin-style commands") {
    val server  = new OverlayDemo.ServerApp("demo-topic-many")
    val client1 = new OverlayDemo.ClientApp(server.coordinationDetails, "demo-topic-many")
    val client2 = new OverlayDemo.ClientApp(server.coordinationDetails, "demo-topic-many")

    try {
      client1.start()
      client2.start()

      eventually() {
        server.node.activeView.nonEmpty && client1.node.activeView.nonEmpty && client2.node.activeView.nonEmpty
      }

      server.node.publishAdd("warmup")
      eventually() {
        val expected = Set("warmup")
        server.node.state.elements == expected &&
        client1.node.state.elements == expected &&
        client2.node.state.elements == expected
      }
      server.node.publishRemove("warmup")
      eventually() {
        server.node.state.elements.isEmpty && client1.node.state.elements.isEmpty && client2.node.state.elements.isEmpty
      }

      assert(client1.handleInputLine("alpha"))
      eventually() {
        val expected = Set("alpha")
        server.node.state.elements == expected && client1.node.state.elements == expected && client2.node.state.elements == expected
      }

      assert(client2.handleInputLine("beta"))
      eventually() {
        val expected = Set("alpha", "beta")
        server.node.state.elements == expected && client1.node.state.elements == expected && client2.node.state.elements == expected
      }

      assert(client1.handleInputLine("-alpha"))
      eventually() {
        val expected = Set("beta")
        server.node.state.elements == expected && client1.node.state.elements == expected && client2.node.state.elements == expected
      }

      assert(client2.handleInputLine("gamma"))
      assert(!client2.handleInputLine("q"))

      eventually(10000) {
        val expected = Set("beta", "gamma")
        server.node.state.elements == expected &&
        client1.node.state.elements == expected &&
        client2.node.state.elements == expected
      }
    } finally {
      client2.stop()
      client1.stop()
      server.stop()
    }
  }
}
