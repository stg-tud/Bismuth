package ex2025blockchain


import rdts.base.{Bottom, Lattice}
import rdts.time.CausalTime


case class Block[H, T](hash: H, previousHash: H, data: T, timestamp: CausalTime)

object Block {
  def apply[H, T](hash: H, previousHash: H, data: T): Block[H, T] =
    Block(hash, previousHash, data, CausalTime.now())

  def genesis[H: Bottom, T: Bottom]: Block[H, T] = {
    val hash = summon[Bottom[H]].empty
    val data = summon[Bottom[T]].empty
    Block(hash, hash, data, CausalTime.empty)
  }

  given bottom[H: Bottom, T: Bottom]: Bottom[Block[H, T]] = Bottom.derived

  given lattice[H: Lattice, T: Lattice]: Lattice[Block[H, T]] = Lattice.derived
}
