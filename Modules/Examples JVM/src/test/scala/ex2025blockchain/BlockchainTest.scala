package ex2025blockchain


import munit.FunSuite
import rdts.base.{Bottom, Lattice}

class BlockchainTest extends FunSuite {
  test("simple chain") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String] = Bottom.provide("")

    val a, b, c = Replica(Blockchain(Block("", None, "")))

    a.mod(_.addBlock("a", "Welcome To DARE"))
    b.mod(_.addBlock("b", "Hello World"))
    c.mod(_.addBlock("c", "Goodbye World"))
    Replica.quiescence(a, b, c)

    assertEquals(a.buffer.result.state.validHead, b.buffer.result.state.validHead)
    assertEquals(b.buffer.result.state.validHead, c.buffer.result.state.validHead)
  }

  test("with orphan branches") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String] = Bottom.provide("")

    val genesisBlock: Block[String, String] = Block("", None, "")
    var a, b = Blockchain(genesisBlock)

    val block1 = Block("a", a.validHead, "Welcome To DARE")
    val amod1 = a.addBlock(block1)
    val block2 = Block("b", b.validHead, "Hello World")
    val bmod1 = b.addBlock(block2)
    a = a `merge` amod1
    a = a `merge` bmod1
    b = b `merge` bmod1
    b = b `merge` amod1

    assertEquals(a.validHead, b.validHead)
    assert(a.contains(block1))      // block2 is more recent than block1, hence the chain of b is chosen as the valid branch
    assert(a.verify(block2))
    assert(b.contains(block1))
    assert(b.verify(block2))

    val block3 = Block("e", b.validHead, "Hello again")
    val bmod2 = b.addBlock(block3)
    b = b `merge` bmod2
    val block4 = Block("f", b.validHead, "Hello again again")
    val bmod3 = b.addBlock(block4)
    b = b `merge` bmod3

    assertNotEquals(a.validHead, b.validHead)

    val latestB = b.validHead

    val block5 = Block("g", a.validHead, "Welcome again")
    val amod2 = a.addBlock(block5)
    a = a `merge` amod2
    a = a `merge` bmod2
    a = a `merge` bmod3
    b = b `merge` amod2

    assertEquals(a.validHead, latestB)
    assertEquals(b.validHead, latestB)
  }

  test("orphan outgrows valid branch") {

  }
}
