package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.*
import lofi_acl.bft.HashDag.{Delta, Encoder, hash}
import rdts.base.{Bottom, Lattice}

import java.security.MessageDigest
import scala.collection.mutable

case class HashDag[D <: Delta[RDT], RDT](root: Hash, ops: Map[Hash, D], heads: Set[Hash])(using Encoder[D]) {
  def add(deltaHash: Hash, delta: D): Either[Set[Hash], HashDag[D, RDT]] = {
    if ops.contains(deltaHash) then return Right(this)
    val deltaParents = delta.parents
    val missing      = deltaParents.filterNot(ops.contains)
    if missing.nonEmpty then return Left(missing)
    Right(copy(
      ops = ops + (deltaHash -> delta),
      heads = (heads -- deltaParents) + deltaHash,
    ))
  }

  // Returns either missing deltas, or the (potentially) updated op graph
  def add(delta: D): Either[Set[Hash], HashDag[D, RDT]] = {
    val deltaParents = delta.parents
    val missing      = deltaParents.filterNot(ops.contains)
    if missing.nonEmpty then return Left(missing)
    val deltaHash = hash(delta)
    if ops.contains(deltaHash) then return Right(this)
    Right(copy(
      ops = ops + (deltaHash -> delta),
      heads = (heads -- deltaParents) + deltaHash,
    ))
  }
}

object HashDag {
  type Encoder[V] = V => Array[Byte]

  object Encoder {
    inline def fromJsoniter[RDT](using codec: JsonValueCodec[RDT]): Encoder[RDT] = rdt => writeToArray(rdt)(using codec)
  }

  def hash[V: Encoder](rdt: V): Hash =
    Hash.unsafeFromArray(MessageDigest.getInstance("SHA3-256", "SUN").digest(summon[Encoder[V]].apply(rdt)))

  trait Delta[RDT]:
      def parents: Set[Hash]
      def rdt: RDT

  object Delta:
      inline def apply[D](using delta: Delta[D]): Delta[D] = delta

  def reduce[D <: Delta[RDT], RDT: {Lattice, Bottom}](hashDag: HashDag[D, RDT], heads: Set[Hash]): RDT = {
    var result  = Bottom[RDT].empty
    val visited = mutable.Set.from(heads)
    val toVisit = mutable.Queue.from(heads)
    while toVisit.nonEmpty do
        val nextHash = toVisit.dequeue()
        val next     = hashDag.ops(nextHash)
        visited += nextHash
        toVisit ++= next.parents.diff(visited)
        result = result.merge(next.rdt)
    result
  }

}
