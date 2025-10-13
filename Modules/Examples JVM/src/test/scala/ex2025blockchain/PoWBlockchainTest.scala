package ex2025blockchain

import munit.FunSuite
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.time.{Dot, Dots}

class PoWBlockchainTest extends FunSuite {

  test("simple chain") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val genesisBlock = PoWBlockchain(Block("", None, "", Dot.zero))
    val a, b, c      = Replica(genesisBlock)

    a.mod(_.addBlock("a", "Welcome To DARE", a.nextDot))
    b.mod(_.addBlock("b", "Hello World", b.nextDot))
    c.mod(_.addBlock("c", "Goodbye World", c.nextDot))
    Replica.quiescence(a, b, c)

    println("--- replica a")
    println(a.buffer.result.state.toTreeString)
    println("--- replica b")
    println(b.buffer.result.state.toTreeString)
    println("--- replica c")
    println(c.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, b.buffer.result.state.validHead)
    assertEquals(b.buffer.result.state.validHead, c.buffer.result.state.validHead)
  }

  test("with orphan branches") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val a, b = Replica(PoWBlockchain(Block("", None, "", Dot.zero)))

    val block1 = Block("a", Option(""), "Welcome to DARE", a.nextDot)
    a.mod(_.addBlock(block1))
    val block2 = Block("b", Option(""), "Hello World", b.nextDot)
    b.mod(_.addBlock(block2))
    Replica.quiescence(a, b)

    println("--- blockchain a")
    println(a.buffer.result.state.toTreeString)
    println("--- blockchain b")
    println(b.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, b.buffer.result.state.validHead)
    assert(a.buffer.result.state.contains(block1))
    assert(a.buffer.result.state.verify(block2))
    assert(b.buffer.result.state.contains(block1))
    assert(b.buffer.result.state.verify(block2))

    val block3 = Block("e", b.buffer.result.state.validHead, "Hello again", b.nextDot)
    b.mod(_.addBlock(block3))
    val block4 = Block("f", b.buffer.result.state.validHead, "Hello again again", b.nextDot)
    b.mod(_.addBlock(block4))

    val latestB = b.buffer.result.state.validHead
    assertNotEquals(a.buffer.result.state.validHead, latestB)

    val block5 = Block("g", a.buffer.result.state.validHead, "Welcome again", a.nextDot)
    a.mod(_.addBlock(block5))
    Replica.quiescence(a, b)

    println("--- blockchain a")
    println(a.buffer.result.state.toTreeString)
    println("--- blockchain b")
    println(b.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, latestB)
    assertEquals(b.buffer.result.state.validHead, latestB)
  }

  test("orphan outgrows valid branch") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val a, b = Replica(PoWBlockchain(Block("", None, "", Dot.zero)))

    val block1 = Block("a", a.buffer.result.state.validHead, "Welcome To DARE", a.nextDot)
    a.mod(_.addBlock(block1))
    Replica.quiescence(a, b)

    val block3 = Block("c", a.buffer.result.state.validHead, "Welcome to Lisbon", a.nextDot)
    a.mod(_.addBlock(block3))

    val block4 = Block("d", b.buffer.result.state.validHead, "Welcome to NOVA", b.nextDot)
    b.mod(_.addBlock(block4))
    val block5 = Block("e", b.buffer.result.state.validHead, "Welcome to Costa da Caparica", b.nextDot)
    b.mod(_.addBlock(block5))

    val block6 = Block("f", a.buffer.result.state.validHead, "I hope you enjoyed the summer school", a.nextDot)
    a.mod(_.addBlock(block6))

    assertEquals(a.buffer.result.state.validHead, block6.hash)
    assertEquals(b.buffer.result.state.validHead, block5.hash)

    val block7 = Block("g", a.buffer.result.state.validHead, "We love CRDTs here at DARE", a.nextDot)
    a.mod(_.addBlock(block7))
    Replica.quiescence(a, b)

    assert(b.buffer.result.state `subsumes` a.buffer.result.state)
    assertEquals(a.buffer.result.state.validHead, block7.hash)
    assertEquals(b.buffer.result.state.validHead, block7.hash)

    println("--- blockchain a")
    println(a.buffer.result.state.toTreeString)
    println("--- blockchain b")
    println(b.buffer.result.state.toTreeString)
  }
}
