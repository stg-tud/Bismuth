package ex2025blockchain

import rdts.base.{Lattice, LocalUid}
import rdts.time.{CausalTime, Dot}

case class Block[H, T](hash: H, previousHash: Option[H], data: T, dot: Dot)

object Block {

  def apply[H, T](hash: H, previousHash: H, data: T, dot: Dot): Block[H, T] =
    Block(hash, Option(previousHash), data, dot)
//  def apply[H, T](hash: H, previousHash: Option[H], data: T, dot: Dot): Block[H, T] =
//    Block(hash, previousHash, data, dot)

}
