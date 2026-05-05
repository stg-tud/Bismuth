package ex2025blockchain

import munit.FunSuite
import rdts.base.{Bottom, Lattice}
import rdts.time.Dot

class PoSBlockchainTest extends FunSuite {

  test("single replica chain") {
    given Lattice[String] = Lattice.fromOrdering

    given Bottom[String] = Bottom.provide("")

    val difficulty   = 0
    val genesisBlock = PoSBlockchain(Block("", None, "", Dot.zero))
    val replicaA     = Replica(genesisBlock)

    val block0 = Block(replicaA.buffer.result.state.validHead, "Welcome to DARE", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block0))
    val block1 = Block(replicaA.buffer.result.state.validHead, "Hello World", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block1))
    val block2 = Block(replicaA.buffer.result.state.validHead, "Goodbye World", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block2))

    // println("--- replica a")
    // println(replicaA.buffer.result.state.toTreeString)

    assertEquals(replicaA.buffer.result.state.validHead, block2.hash)
    assert(replicaA.buffer.result.state.validate())
  }

  test("two replicas simple chain") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val difficulty         = 0
    val genesisBlock       = PoSBlockchain(Block("", None, "", Dot.zero))
    val replicaA, replicaB = Replica(genesisBlock)

    val block0 = Block(replicaA.buffer.result.state.validHead, "Welcome to DARE", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block0))
    Replica.quiescence(replicaA, replicaB)
    val block1 = Block(replicaB.buffer.result.state.validHead, "Hello World", replicaB.nextDot, difficulty)
    replicaB.mod(_.addBlock(block1))
    Replica.quiescence(replicaA, replicaB)
    val block2 = Block(replicaA.buffer.result.state.validHead, "Goodbye World", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block2))
    Replica.quiescence(replicaA, replicaB)

    // println("--- replica a")
    // println(replicaA.buffer.result.state.toTreeString)
    // println("--- replica b")
    // println(replicaB.buffer.result.state.toTreeString)

    assertEquals(replicaA.buffer.result.state.validHead, replicaB.buffer.result.state.validHead)
    assert(replicaA.buffer.result.state.validate())
    assert(replicaB.buffer.result.state.validate())
  }

  test("two replicas concurrent add to genesis chain") {
    given Lattice[String] = Lattice.fromOrdering

    given Bottom[String] = Bottom.provide("")

    val difficulty         = 0
    val genesisBlock       = PoSBlockchain(Block("", None, "", Dot.zero))
    val replicaA, replicaB = Replica(genesisBlock)

    val block0 = Block(replicaA.buffer.result.state.validHead, "Welcome to DARE", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block0))

    val block1 = Block(replicaB.buffer.result.state.validHead, "Hello World", replicaB.nextDot, difficulty)
    replicaB.mod(_.addBlock(block1))
    Replica.quiescence(replicaA, replicaB)

    // println("--- replica a")
    // println(replicaA.buffer.result.state.toTreeString)
    // println("--- replica b")
    // println(replicaB.buffer.result.state.toTreeString)

    assertEquals(replicaA.buffer.result.state.validHead, replicaB.buffer.result.state.validHead)
    assert(replicaA.buffer.result.state.validate())
    assert(replicaB.buffer.result.state.validate())
  }

  test("three replicas with orphan branch") {
    given Lattice[String] = Lattice.fromOrdering
    given Bottom[String]  = Bottom.provide("")

    val difficulty         = 0
    val replicaA, replicaB = Replica(PoSBlockchain(Block("", None, "", Dot.zero)))

    val block1 = Block(replicaA.buffer.result.state.validHead, "Welcome To DARE", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block1))
//    Replica.quiescence(replicaA, replicaB)

    val block3 = Block(replicaA.buffer.result.state.validHead, "Welcome to Lisbon", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block3))

    val block4 = Block(replicaB.buffer.result.state.validHead, "Welcome to NOVA", replicaB.nextDot, difficulty)
    replicaB.mod(_.addBlock(block4))
    val block5 =
      Block(replicaB.buffer.result.state.validHead, "Welcome to Costa da Caparica", replicaB.nextDot, difficulty)
    replicaB.mod(_.addBlock(block5))

    val block6 = Block(
      replicaA.buffer.result.state.validHead,
      "I hope you enjoyed the summer school",
      replicaA.nextDot,
      difficulty
    )
    replicaA.mod(_.addBlock(block6))

    assertEquals(replicaA.buffer.result.state.validHead, block6.hash)
    assertEquals(replicaB.buffer.result.state.validHead, block5.hash)

    val block7 =
      Block(replicaA.buffer.result.state.validHead, "We love CRDTs here at DARE", replicaA.nextDot, difficulty)
    replicaA.mod(_.addBlock(block7))
    Replica.quiescence(replicaA, replicaB)

    assert(replicaB.buffer.result.state `subsumes` replicaA.buffer.result.state)
    assertEquals(replicaA.buffer.result.state.validHead, block7.hash)
    assertEquals(replicaB.buffer.result.state.validHead, block7.hash)

    // println("--- blockchain a")
    // println(replicaA.buffer.result.state.toTreeString)
    // println("--- blockchain b")
    // println(replicaB.buffer.result.state.toTreeString)

    assert(replicaA.buffer.result.state.validate())
    assert(replicaB.buffer.result.state.validate())
  }

}
