package ex2025blockchain


import munit.FunSuite
import rdts.base.{Bottom, Lattice}

class BlockchainTest extends FunSuite {
  test("simple chain") {
    given Lattice[String] = (a, b) => {
      if a.compareTo(b) >= 0 then a else b
    }

    given Bottom[String] = Bottom.provide("")

    val a, b, c = Replica(Blockchain.empty[String, String])

    a.mod(_.addBlock("a", "Welcome To DARE"))
    b.mod(_.addBlock("b", "Hello World"))
    c.mod(_.addBlock("c", "Goodbye World"))
    Replica.quiescence(a, b, c)

    assertEquals(a.buffer.result.state.latest, b.buffer.result.state.latest)
    assertEquals(b.buffer.result.state.latest, c.buffer.result.state.latest)
  }

  test("with orphan branches") {
    given Lattice[String] = (a, b) => { if a.compareTo(b) >= 0 then a else b }
    given Bottom[String] = Bottom.provide("")

    val a, b, c = Replica(Blockchain.empty[String, String])

    a.mod(_.addBlock("a", "Welcome To DARE"))
    b.mod(_.addBlock("b", "Hello World"))
    c.mod(_.addBlock("c", "Goodbye World"))
    Replica.quiescence(a, b, c)

    assertEquals(a.buffer.result.state.latest, b.buffer.result.state.latest)
    assertEquals(b.buffer.result.state.latest, c.buffer.result.state.latest)

    b.mod(_.addBlock("e", "Hello again"))
    b.mod(_.addBlock("f", "Hello again again"))
    c.mod(_.addBlock("d", "Goodbye again"))
    Replica.quiescence(b, c)

    assertNotEquals(a.buffer.result.state.latest, b.buffer.result.state.latest)
    assertEquals(b.buffer.result.state.latest, c.buffer.result.state.latest)

    val latestBlock = b.buffer.result.state.latest

    a.mod(_.addBlock("g", "Welcome again"))
    Replica.quiescence(a, b, c)

    // TODO change to assertEquals once a correct Lattice merge function is implemented
    assertNotEquals(a.buffer.result.state.latest, latestBlock)
    assertNotEquals(b.buffer.result.state.latest, latestBlock)
    assertNotEquals(c.buffer.result.state.latest, latestBlock)
  }
}
