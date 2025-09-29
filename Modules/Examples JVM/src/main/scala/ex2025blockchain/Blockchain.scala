package ex2025blockchain

import ex2025blockchain.Block
import rdts.base.{Bottom, Lattice}
import rdts.time.CausalTime

import scala.annotation.tailrec

/** The implementation ignores the cryptographic concepts used in blockchains and focuses instead on the replication of the blocks
  *
  * @param inner  a map where the key is the hash value of the block
  * @tparam H The type of the hash
  * @tparam T The type of the data in the block
  */
case class Blockchain[H, T](inner: Map[H, Block[H, T]]) {

  type delta = Blockchain[H, T]

  /** heads are all hashes that do not have a succeeding block
    * all hashes - hashes that are referenced as previous hash in a block - genesis block
    * the genesis is only subtracted if there is more than one block in the blockchain, otherwise it is the head
    */
  def heads: Set[H] =
    inner.keySet
      -- inner.values.filter(_.previousHash.isDefined).map(_.previousHash.get)
      -- (if inner.size > 1 then inner.values.filter(_.previousHash.isEmpty).map(_.hash) else Set.empty)

  /** the head of the longest chain should be the validHead
    * in case two chains have the same length, the head with the latest timestamp should be the validHead
    */
  given headOrdering: Ordering[H] = Ordering.by[H, (Int, CausalTime)] { hash =>
    (chainLength(hash), inner(hash).timestamp)
  }

  def validHead: H = heads.max

  /** adds a new block to the blockchain by appending it to the end of the chain
    *
    * @param newBlock the block to add
    * @return the resulting blockchain
    */
  def addBlock(newBlock: Block[H, T]): Blockchain[H, T] = {
    var newHeads = heads - newBlock.previousHash.get
    newHeads += newBlock.hash
    Blockchain(Map(newBlock.hash -> newBlock))
  }

  /** adds a new block to the blockchain by appending it to the end of the chain
    *
    * @param hash the hash value of the block
    * @param data the data in the block
    * @return the resulting blockchain
    */
  def addBlock(hash: H, data: T): Blockchain[H, T] = addBlock(Block(hash, validHead, data))

  @tailrec
  private def chainLength(start: H, length: Int = 0): Int = inner(start).previousHash match {
    case Some(next) => chainLength(next, length + 1)
    case None => length + 1
  }

  def validChainLength: Int = chainLength(validHead)

  /** verify that a given block is part of the current valid blockchain
    *
    * @param block the block to validate
    * @return true if the block is part of the valid blockchain
    */
  def verify(block: Block[H, T]): Boolean = {
    var currentBlock = inner.get(validHead)
    while currentBlock.isDefined do {
      if currentBlock.get == block then return true
      currentBlock = currentBlock.get.previousHash match {
        case Some(value) => inner.get(value)
        case None        => None
      }
    }
    false
  }

  def contains(block: Block[H, T]): Boolean = inner.contains(block.hash)

  def toTreeString: String = {
    @tailrec
    def getBranchString(h: H, list: List[String] = List.empty): List[String] = {
      val block = inner(h)
      block.previousHash match {
        case Some(value) => getBranchString(value, list :+ block.data.toString)
        case None        => list :+ block.data.toString
      }
    }

    val branches = heads.map { head => getBranchString(head) }.toList
    f"---\n${branches.sortBy(_.length).map(_.reverse.map(a => f"($a)").mkString(" <- ")).mkString(" \n")}"
  }

}

object Blockchain {

  def apply[H, T](genesisBlock: Block[H, T]): Blockchain[H, T] =
    Blockchain(Map(genesisBlock.hash -> genesisBlock))

  given [H: Bottom, T]: Bottom[Blockchain[H, T]] = Bottom.derived

  given [H, T]: Lattice[Blockchain[H, T]] = {
    given Lattice[Block[H, T]] = Lattice.assertEquals
    Lattice.derived
  }

}
