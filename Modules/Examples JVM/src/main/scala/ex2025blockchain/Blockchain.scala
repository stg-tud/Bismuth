package ex2025blockchain

import ex2025blockchain.Block
import rdts.base.{Bottom, Lattice}
import rdts.time.CausalTime

import scala.annotation.tailrec

/** The implementation ignores the cryptographic concepts used in blockchains and focuses instead on the replication of the blocks
  *
  * @param inner  a map where the key is the hash value of the block
  * @param heads  a set of hashes that do not have a successor, one of the hashes is the head of the valid chain
  * @tparam H The type of the hash
  * @tparam T The type of the data in the block
  */
case class Blockchain[H, T](inner: Map[H, Block[H, T]], heads: Set[H]) {

  type delta = Blockchain[H, T]

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
    Blockchain(Map(newBlock.hash -> newBlock), newHeads)
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
    branches.sortBy(_.length).map(_.reverse.map(a => f"($a)").mkString(" <- ")).mkString("\n")
  }

}

object Blockchain {

  def apply[H, T](genesisBlock: Block[H, T]): Blockchain[H, T] =
    Blockchain(Map(genesisBlock.hash -> genesisBlock), Set(genesisBlock.hash))

  given [H: Bottom, T]: Bottom[Blockchain[H, T]] = Bottom.derived

  given [H: Lattice, T]: Lattice[Blockchain[H, T]] = (a: Blockchain[H, T], b: Blockchain[H, T]) => {

    /** the merge function keeps orphan branches since one orphan branch can still grow and outgrow the current valid chain
      *
      * if both chains have the same length, there is no deterministic behavior for selecting the valid chain
      * this implementation relies on the latest block to select a winner
      */

    val newInner = a.inner ++ b.inner

    def getNewChainLength(start: H): Int = newInner.get(start) match {
      case Some(startBlock) => startBlock.previousHash match {
          case Some(value) => getNewChainLength(value) + 1
          case None        => 1
        }
      case None => 0
    }

    @tailrec
    def blockIsInChain(blockHash: H, chainHead: H): Boolean = {
      val headBlock = newInner(chainHead)
      if headBlock.hash == blockHash then return true
      headBlock.previousHash match {
        case Some(value) => blockIsInChain(blockHash, value)
        case None        => false
      }
    }

    def subtractHeads(a: Set[H], b: Set[H]): Set[H] = {
      a.filterNot(ahead => b.filter(bhead => ahead != bhead).exists(bhead => blockIsInChain(ahead, bhead)))
    }

    val newHeads = subtractHeads(a.heads, b.heads) ++ subtractHeads(b.heads, a.heads)

    Blockchain(newInner, newHeads)
  }
}
