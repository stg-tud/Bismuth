package ex2025blockchain

import ex2025blockchain.Block
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.Dot

import java.security.MessageDigest
import scala.annotation.tailrec

/** The implementation ignores the cryptographic concepts used in blockchains and focuses instead on the replication and consensus
  * proof of work implementation
  *
  * @param inner  a map where the key is the hash value of the block
  * @tparam T The type of the data in the block
  */
case class PoWBlockchain[T](inner: Map[String, Block[T]], difficulty: Int)
    extends Blockchain[T, PoWBlockchain[T]](inner, difficulty) {

  /** the head of the longest chain should be the validHead */
  given headOrdering: Ordering[String] = Ordering.by[String, (Int, String)] { hash =>
    (chainLength(hash), hash)
  }

  def validHead: String = heads.max

  override def addBlock(newBlock: Block[T]): PoWBlockchain[T] =
    PoWBlockchain(Map(newBlock.hash -> newBlock), difficulty)

  @tailrec
  private def chainLength(start: String, length: Int = 0): Int = inner(start).previousHash match {
    case Some(next) => chainLength(next, length + 1)
    case None       => length + 1
  }

  def validChainLength: Int = chainLength(validHead)

  override def validate(): Boolean = {
    inner.values.forall { block =>
      block.previousHash match {
        case Some(prevHash) =>
          inner.contains(prevHash) && block.hash == Block.mineHash(
            prevHash,
            block.data,
            block.dot,
            block.timestamp,
            difficulty
          )._1
        case None => true
      }
    }
  }

}

object PoWBlockchain {

  def apply[T](genesisBlock: Block[T], difficulty: Int = 1): PoWBlockchain[T] =
    PoWBlockchain(Map(genesisBlock.hash -> genesisBlock), difficulty)

  given [T]: Bottom[PoWBlockchain[T]] = {
    given Bottom[Int] = Bottom.provide(0)
    Bottom.derived
  }

  given [T]: Lattice[PoWBlockchain[T]] = {
    given Lattice[Block[T]] = Lattice.assertEquals
    given Lattice[Int]      = Lattice.fromOrdering

    Lattice.derived
  }

}
