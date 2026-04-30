package ex2026overlaydemo

/** vibecoded. dont trust 😉 */
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

      eventually(10000) {
        val directory = server.node.connectionDirectory
        directory.get(client.node.localUid.uid).exists(_.elements.exists(peer => peer.uid == server.node.localUid.uid))
      }

      assert(!server.node.connectionDirectory.keySet.contains(server.node.localUid.uid))

      server.node.publishAdd("server-value")
      client.node.publishAdd("client-value")

      eventually() {
        server.node.state.values.elements == Set("server-value", "client-value") &&
        client.node.state.values.elements == Set("server-value", "client-value")
      }

      client.node.publishRemove("server-value")

      eventually() {
        server.node.state.values.elements == Set("client-value") &&
        client.node.state.values.elements == Set("client-value")
      }
    } finally {
      client.stop()
      server.stop()
    }
  }

  test("overlay demo supports multiple clients, stdin-style commands, and replicated connection info") {
    val server  = new OverlayDemo.ServerApp("demo-topic-many")
    val client1 = new OverlayDemo.ClientApp(server.coordinationDetails, "demo-topic-many")
    val client2 = new OverlayDemo.ClientApp(server.coordinationDetails, "demo-topic-many")

    try {
      client1.start()
      client2.start()

      eventually() {
        server.node.activeView.nonEmpty && client1.node.activeView.nonEmpty && client2.node.activeView.nonEmpty
      }

      eventually(10000) {
        val expectedClients = Set(client1.node.localUid.uid, client2.node.localUid.uid)
        val directory       = client1.node.connectionDirectory
        expectedClients.subsetOf(directory.keySet) &&
        !directory.keySet.contains(server.node.localUid.uid)
      }

      server.node.publishAdd("warmup")
      eventually() {
        val expected = Set("warmup")
        server.node.state.values.elements == expected &&
        client1.node.state.values.elements == expected &&
        client2.node.state.values.elements == expected
      }
      server.node.publishRemove("warmup")
      eventually() {
        server.node.state.values.elements.isEmpty && client1.node.state.values.elements.isEmpty && client2.node.state.values.elements.isEmpty
      }

      assert(client1.handleInputLine("alpha"))
      eventually() {
        val expected = Set("alpha")
        server.node.state.values.elements == expected && client1.node.state.values.elements == expected && client2.node.state.values.elements == expected
      }

      assert(client2.handleInputLine("beta"))
      eventually() {
        val expected = Set("alpha", "beta")
        server.node.state.values.elements == expected && client1.node.state.values.elements == expected && client2.node.state.values.elements == expected
      }

      assert(client1.handleInputLine("-alpha"))
      eventually() {
        val expected = Set("beta")
        server.node.state.values.elements == expected && client1.node.state.values.elements == expected && client2.node.state.values.elements == expected
      }

      assert(client2.handleInputLine("gamma"))
      assert(!client2.handleInputLine("q"))

      eventually(10000) {
        val expected = Set("beta", "gamma")
        server.node.state.values.elements == expected &&
        client1.node.state.values.elements == expected &&
        client2.node.state.values.elements == expected
      }
    } finally {
      client2.stop()
      client1.stop()
      server.stop()
    }
  }
}
