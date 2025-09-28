package ex2025blockchain

import rdts.time.CausalTime

case class Block[H, T](hash: H, previousHash: Option[H], data: T, timestamp: CausalTime)

object Block {

  def apply[H, T](hash: H, previousHash: H, data: T): Block[H, T] =
    Block(hash, Option(previousHash), data, CausalTime.now())
  def apply[H, T](hash: H, previousHash: Option[H], data: T): Block[H, T] =
    Block(hash, previousHash, data, CausalTime.now())

}
