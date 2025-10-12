package ex2025blockchain

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet

import scala.annotation.tailrec

/** proof of stake consensus
  * the stake of a replica is its contributed data size to the valid chain
  * @param inner
  * @param proposedBlocks
  * @tparam H
  * @tparam T
  */
case class PoSBlockchain[H, T](
    inner: Map[H, Block[H, T]],
    proposedBlocks: ReplicatedSet[Block[H, T]], // should we use a remove wins set instead?
) extends Blockchain[H, T, PoSBlockchain[H, T]](inner) {

  given headOrdering: Ordering[H] = Ordering.by[H, (Long, Uid)] { hash =>
    (stakeWeightOfChain(hash), inner(hash).dot.place)
  }

  override def validHead: H = heads.max

  override def addBlock(newBlock: Block[H, T]): PoSBlockchain[H, T] = {
    given LocalUid(newBlock.dot.place)
    PoSBlockchain(inner, proposedBlocks.add(newBlock))
  }

  def commit: PoSBlockchain[H, T] = {
    val currentValidHead    = validHead
    val currentStakeWeights = stakeWeightsOfChain(currentValidHead)
    // choose the block from the replica with the highest stake weight
    val selectedBlock: Block[H, T] = proposedBlocks.elements
      .filter(_.previousHash.exists(_ == currentValidHead))
      .maxBy(block => currentStakeWeights.getOrElse(block.dot.place, 0L))

    PoSBlockchain(inner + (selectedBlock.hash -> selectedBlock), ReplicatedSet.empty)
  }

  @tailrec
  private def stakeWeightsOfChain(head: H, accumulatedStakeWeights: Map[Uid, Long] = Map.empty): Map[Uid, Long] =
    inner.get(head) match {
      case Some(block) => {
        val newStakeWeight = accumulatedStakeWeights.get(block.dot.place) match {
          case Some(value) => value + block.data.toString.length
          case None        => 0L
        }
        block.previousHash match {
          case Some(predecessor) =>
            stakeWeightsOfChain(predecessor, accumulatedStakeWeights + (block.dot.place -> newStakeWeight))
          case None => accumulatedStakeWeights + (block.dot.place -> newStakeWeight)
        }
      }
      case None => accumulatedStakeWeights
    }

  def stakeWeightOfChain(head: H): Long = stakeWeightsOfChain(head).values.sum

}

object PoSBlockchain {

  def apply[H, T](genesisBlock: Block[H, T]): PoSBlockchain[H, T] =
    PoSBlockchain(Map(genesisBlock.hash -> genesisBlock), ReplicatedSet.empty)

  given [H: Bottom, T]: Bottom[PoSBlockchain[H, T]] = Bottom.derived

  given [H, T]: Lattice[PoSBlockchain[H, T]] = {
    given Lattice[Block[H, T]] = Lattice.assertEquals
    Lattice.derived
  }

}
