package ex2025blockchain

import scala.annotation.tailrec

abstract class Blockchain[T, Self <: Blockchain[T, Self]](inner: Map[String, Block[T]], difficulty: Int = 0) {

  /** heads are all hashes that do not have a succeeding block
    * all hashes - hashes that are referenced as previous hash in a block - genesis block
    * the genesis block is only subtracted if there is more than one block in the blockchain, otherwise it is the head
    */
  def heads: Set[String] =
    inner.keySet
    -- inner.values.filter(_.previousHash.isDefined).map(_.previousHash.get)
    -- (if inner.size > 1 then inner.values.filter(_.previousHash.isEmpty).map(_.hash) else Set.empty)

  def validHead: String

  def addBlock(newBlock: Block[T]): Self

  /** adds a new block to the blockchain by appending it to the end of the chain
    *
    * @param hash the hash value of the block
    * @param data the data in the block
    * @return the resulting blockchain
    */
//  def addBlock(hash: H, data: T, dot: Dot): Self = addBlock(Block(hash, validHead, data, dot))

  /** verify that a given block is part of the current valid blockchain
    *
    * @param block the block to validate
    * @return true if the block is part of the valid blockchain
    */
  def verify(block: Block[T]): Boolean = {
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

  def contains(hash: String): Boolean = inner.contains(hash)

  def contains(block: Block[T]): Boolean = contains(block.hash)

  def toTreeString: String = {
    @tailrec
    def getBranchString(h: String, list: List[String] = List.empty): List[String] = {
      val block = inner(h)
      block.previousHash match {
        case Some(value) => getBranchString(value, list :+ block.data.toString)
        case None        => list :+ block.data.toString
      }
    }

    val branchesString = heads.toList
      .map { head => getBranchString(head) }
      .sortBy(_.length).reverse
      .map(_.reverse.map(a => f"($a)").mkString(" <- "))
    f"---\n${branchesString.mkString(" \n")}\nvalid head: (${inner(validHead).data.toString})"
  }

  def validate(): Boolean

}
