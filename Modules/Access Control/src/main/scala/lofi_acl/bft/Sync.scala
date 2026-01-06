package lofi_acl.bft

import lofi_acl.bft.HashDag.Delta

import scala.collection.mutable

object Sync {
  // Optimization for case when remotes HashDag is a strict subset of local HashDag
  def missingInSubsetHashDag[D <: Delta[RDT], RDT](local: HashDag[D, RDT], remoteHeads: Set[Hash]): Option[Seq[D]] = {
    if !remoteHeads.forall(local.ops.contains) then return None

    val result: mutable.ArrayBuffer[D] = mutable.ArrayBuffer.empty
    val contained: mutable.Set[Hash]   = mutable.Set.from(remoteHeads)
    val queue: mutable.Queue[Hash]     = mutable.Queue.from(local.heads -- remoteHeads)
    while queue.nonEmpty do
        val nextHash = queue.dequeue()
        val delta    = local.ops(nextHash)
        queue ++= delta.parents.diff(contained)
        contained += nextHash
        result.append(delta)

    Some(result.toSeq)
  }
}
