package ex2025blockchain

import munit.FunSuite
import rdts.base.{Bottom, Lattice}
import rdts.time.Dot

class PoSBlockchainTest extends FunSuite {

  test("single replica chain") {
    given Lattice[String] = Lattice.fromOrdering

    given Bottom[String] = Bottom.provide("")

    val genesisBlock = PoSBlockchain(Block("", None, "", Dot.zero))
    val a            = Replica(genesisBlock)

    a.mod(_.addBlock("a", "Welcome To DARE", a.nextDot))
    a.mod(_.commit)
    a.mod(_.addBlock("b", "Hello World", a.nextDot))
    a.mod(_.commit)
    a.mod(_.addBlock("c", "Goodbye World", a.nextDot))
    a.mod(_.commit)

    println("--- replica a")
    println(a.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, "c")
  }

  test("two replicas simple chain") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val genesisBlock = PoSBlockchain(Block("", None, "", Dot.zero))
    val a, b         = Replica(genesisBlock)

    a.mod(_.addBlock("a", "Welcome To DARE", a.nextDot))
    a.mod(_.commit)
    Replica.quiescence(a, b)
    b.mod(_.addBlock("b", "Hello World", a.nextDot))
    b.mod(_.commit)
    Replica.quiescence(a, b)
    a.mod(_.addBlock("c", "Goodbye World", a.nextDot))
    a.mod(_.commit)
    Replica.quiescence(a, b)

    println("--- replica a")
    println(a.buffer.result.state.toTreeString)
    println("--- replica b")
    println(b.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, "c")
    assertEquals(b.buffer.result.state.validHead, "c")
  }

  test("two replicas concurrent add to genesis chain") {
    given Lattice[String] = Lattice.fromOrdering

    given Bottom[String] = Bottom.provide("")

    val genesisBlock = PoSBlockchain(Block("", None, "", Dot.zero))
    val a, b         = Replica(genesisBlock)

    a.mod(_.addBlock("a", "Welcome To DARE", a.nextDot))
    Replica.quiescence(a, b)
    b.mod(_.addBlock("b", "Hello World", a.nextDot))
    Replica.quiescence(a, b)
    a.mod(_.commit)
    Replica.quiescence(a, b)

    println("--- replica a")
    println(a.buffer.result.state.toTreeString)
    println("--- replica b")
    println(b.buffer.result.state.toTreeString)

    assertEquals(a.buffer.result.state.validHead, "a")
    assertEquals(b.buffer.result.state.validHead, "a")
  }

  test("three replicas with orphan branch") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val a, b = Replica(PoSBlockchain(Block("", None, "", Dot.zero)))

    val block1 = Block("a", a.buffer.result.state.validHead, "Welcome To DARE", a.nextDot)
    a.mod(_.addBlock(block1))
    a.mod(_.commit)
    Replica.quiescence(a, b)

    val block3 = Block("c", a.buffer.result.state.validHead, "Welcome to Lisbon", a.nextDot)
    a.mod(_.addBlock(block3))
    a.mod(_.commit)

    val block4 = Block("d", b.buffer.result.state.validHead, "Welcome to NOVA", b.nextDot)
    b.mod(_.addBlock(block4))
    b.mod(_.commit)
    val block5 = Block("e", b.buffer.result.state.validHead, "Welcome to Costa da Caparica", b.nextDot)
    b.mod(_.addBlock(block5))
    b.mod(_.commit)

    val block6 = Block("f", a.buffer.result.state.validHead, "I hope you enjoyed the summer school", a.nextDot)
    a.mod(_.addBlock(block6))
    a.mod(_.commit)

    assertEquals(a.buffer.result.state.validHead, block6.hash)
    assertEquals(b.buffer.result.state.validHead, block5.hash)

    val block7 = Block("g", a.buffer.result.state.validHead, "We love CRDTs here at DARE", a.nextDot)
    a.mod(_.addBlock(block7))
    a.mod(_.commit)
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
