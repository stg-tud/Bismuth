package ex2025blockchain

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import rdts.time.Dot

import java.security.MessageDigest
import scala.annotation.tailrec

/** proof of stake consensus
  * the stake of a replica is its contributed data size to the valid chain
  * @param inner
  * @param proposedBlocks
  * @tparam T
  */
case class PoSBlockchain[T](
    inner: Map[String, Block[T]],
    proposedBlocks: ReplicatedSet[Block[T]], // should we use a remove wins set instead?
) extends Blockchain[T, PoSBlockchain[T]](inner) {

  final private val randomPrime: Long = 2147483647

  given headOrdering: Ordering[String] = Ordering.by[String, (Long, String)] { hash =>
    (stakeWeightOfChain(hash), hash)
  }

  override def validHead: String = heads.max

  override def addBlock(newBlock: Block[T]): PoSBlockchain[T] = {
    val proposerUid = getProposer(newBlock.dot.place)
    if proposerUid == Uid.zero || proposerUid == newBlock.dot.place then {
      PoSBlockchain(inner + (newBlock.hash -> newBlock), ReplicatedSet.empty)
    } else {
      println(
        f"tried to add block $newBlock with proposer $proposerUid but origin is not proposer ${newBlock.dot.place}"
      )
      PoSBlockchain(Map.empty, ReplicatedSet.empty)
    }
  }

  /** get the proposer for a given block
    * @param blockHash the hash of the block
    * @return the uid of the proposer
    */
  private def getProposer(origin: Uid, blockHash: String = validHead): Uid = {
    val stakeList: List[Uid] = origin :: stakeListOfBranch(blockHash)
    val stakeHolders         = stakeList.toSet.size
    val roundNumber          = stakeList.length

    if roundNumber > 0 then
      val index = (stakeHolders * randomPrime) % roundNumber
      stakeList(index.toInt)
    else Uid.zero // roundNumber == 1 => blockchain only contains genesis block
  }

  @tailrec
  private def stakeWeightsOfChain(head: String, accumulatedStakeWeights: Map[Uid, Long] = Map.empty): Map[Uid, Long] =
    inner.get(head) match {
      case Some(block) =>
        val newStakeWeight = accumulatedStakeWeights.get(block.dot.place) match {
          case Some(value) => value + block.data.toString.length
          case None        => block.data.toString.length.toLong
        }
        block.previousHash match {
          case Some(predecessor) =>
            stakeWeightsOfChain(predecessor, accumulatedStakeWeights + (block.dot.place -> newStakeWeight))
          case None => accumulatedStakeWeights + (block.dot.place -> newStakeWeight)
        }
      case None => accumulatedStakeWeights
    }

  def stakeWeightOfChain(head: String): Long = stakeWeightsOfChain(head).values.sum

  @tailrec
  private def stakeListOfBranch(currentHash: String, stakeList: List[Uid] = List.empty): List[Uid] =
    inner.get(currentHash) match {
      case Some(currentBlock) => currentBlock.previousHash match {
          case Some(previousH) => stakeListOfBranch(previousH, currentBlock.dot.place :: stakeList)
          case None            => stakeList // current block = genesis block
        }
      case None => ???
    }

  override def validate(): Boolean = inner.values.forall { block =>
    val proposer = getProposer(block.dot.place, block.hash)
    if proposer == Uid.zero then true else block.dot.place == proposer
  }

}

object PoSBlockchain {

  def apply[T](genesisBlock: Block[T]): PoSBlockchain[T] =
    PoSBlockchain(Map(genesisBlock.hash -> genesisBlock), ReplicatedSet.empty)

  given [T]: Bottom[PoSBlockchain[T]] = Bottom.derived

  given [T]: Lattice[PoSBlockchain[T]] = {
    given Lattice[Block[T]] = Lattice.assertEquals
    Lattice.derived
  }

}
