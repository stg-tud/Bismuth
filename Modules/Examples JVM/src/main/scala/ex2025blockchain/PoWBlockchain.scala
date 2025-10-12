package ex2025blockchain

import ex2025blockchain.Block
import rdts.base.{Bottom, Lattice, Uid}

import scala.annotation.tailrec

/** The implementation ignores the cryptographic concepts used in blockchains and focuses instead on the replication and consensus
  * proof of work implementation
  *
  * @param inner  a map where the key is the hash value of the block
  * @tparam H The type of the hash
  * @tparam T The type of the data in the block
  */
case class PoWBlockchain[H, T](inner: Map[H, Block[H, T]]) extends Blockchain[H, T, PoWBlockchain[H, T]](inner) {

  /** the head of the longest chain should be the validHead
    * in case two chains have the same length, the head with the latest timestamp should be the validHead (untrue)
    */
  given headOrdering: Ordering[H] = Ordering.by[H, (Int, Uid)] { hash =>
    (chainLength(hash), inner(hash).dot.place)
  }

  def validHead: H = heads.max

  /** adds a new block to the blockchain by appending it to the end of the chain
    *
    * @param newBlock the block to add
    * @return the resulting blockchain
    */
  def addBlock(newBlock: Block[H, T]): PoWBlockchain[H, T] = {
    var newHeads = heads - newBlock.previousHash.get
    newHeads += newBlock.hash
    PoWBlockchain(Map(newBlock.hash -> newBlock))
  }

  @tailrec
  private def chainLength(start: H, length: Int = 0): Int = inner(start).previousHash match {
    case Some(next) => chainLength(next, length + 1)
    case None       => length + 1
  }

  def validChainLength: Int = chainLength(validHead)

}

object PoWBlockchain {

  def apply[H, T](genesisBlock: Block[H, T]): PoWBlockchain[H, T] =
    PoWBlockchain(Map(genesisBlock.hash -> genesisBlock))

  given [H: Bottom, T]: Bottom[PoWBlockchain[H, T]] = Bottom.derived

  given [H, T]: Lattice[PoWBlockchain[H, T]] = {
    given Lattice[Block[H, T]] = Lattice.assertEquals

    Lattice.derived
  }

}
