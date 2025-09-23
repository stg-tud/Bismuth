package ex2025blockchain

import ex2025blockchain.Block
import rdts.base.{Bottom, Lattice}


/**
 * @param inner  a map where the key is the hash value of the block
 * @param latest the hash of the latest block
 * @tparam H The type of the hash
 * @tparam T The type of the data in the block
 */
case class Blockchain[H, T](inner: Map[H, Block[H, T]], latest: H) {

  type delta = Blockchain[H, T]

  def addBlock(hash: H, data: T): Blockchain[H, T] = {
    val newBlock = Block(hash, latest, data)
    Blockchain(Map(hash -> newBlock), hash)
  }

}

object Blockchain {

  def empty[H: Bottom, T: Bottom]: Blockchain[H, T] =
    Blockchain(Map(Block.genesis[H, T].hash -> Block.genesis), Block.genesis[H, T].hash)

  given [H: Bottom, T: Bottom]: Bottom[Blockchain[H, T]] = Bottom.derived

  given [H: Lattice, T: Lattice]: Lattice[Blockchain[H, T]] = Lattice.derived
}
