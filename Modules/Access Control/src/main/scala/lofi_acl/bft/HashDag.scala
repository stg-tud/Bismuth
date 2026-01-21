package lofi_acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.*
import lofi_acl.bft.HashDag.{Delta, Encoder, Hashable}
import rdts.base.{Bottom, Lattice}

import scala.collection.mutable

case class HashDag[D <: Delta[State]: Hashable, State](root: Hash, deltas: Map[Hash, D], heads: Set[Hash])(using
    Encoder[Delta[State]]
) {
  def add(deltaHash: Hash, delta: D): Either[Set[Hash], HashDag[D, State]] = {
    if deltas.contains(deltaHash) then return Right(this)
    val deltaParents = delta.parents
    val missing      = deltaParents.filterNot(deltas.contains)
    if missing.nonEmpty then return Left(missing)
    Right(copy(
      deltas = deltas + (deltaHash -> delta),
      heads = (heads -- deltaParents) + deltaHash,
    ))
  }

  // Returns either missing deltas, or the (potentially) updated op graph
  def add(delta: D): Either[Set[Hash], HashDag[D, State]] = {
    val deltaParents = delta.parents
    val missing      = deltaParents.filterNot(deltas.contains)
    if missing.nonEmpty then return Left(missing)
    val deltaHash = Hashable[D].hash(delta)
    if deltas.contains(deltaHash) then return Right(this)
    Right(copy(
      deltas = deltas + (deltaHash -> delta),
      heads = (heads -- deltaParents) + deltaHash,
    ))
  }
}

object HashDag {
  type Encoder[V] = V => Array[Byte]

  object Encoder {
    inline def fromJsoniter[RDT](using codec: JsonValueCodec[RDT]): Encoder[RDT] = rdt => writeToArray(rdt)(using codec)
  }

  trait Hashable[D]:
      def hash(value: D): Hash

  object Hashable:
      inline def apply[D](using instance: Hashable[D]): Hashable[D] = instance

  trait Delta[State]:
      def parents: Set[Hash]
      def state: State

  object Delta:
      inline def apply[State](using delta: Delta[State]): Delta[State] = delta

  def reduce[D <: Delta[RDT], RDT: {Lattice, Bottom}](hashDag: HashDag[D, RDT], heads: Set[Hash]): RDT = {
    var result  = Bottom[RDT].empty
    val visited = mutable.Set.from(heads)
    val toVisit = mutable.Queue.from(heads)
    while toVisit.nonEmpty do
        val nextHash = toVisit.dequeue()
        val next     = hashDag.deltas(nextHash)
        visited += nextHash
        toVisit ++= next.parents.diff(visited)
        result = result.merge(next.state)
    result
  }
}
