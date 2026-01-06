package lofi_acl.bft

import lofi_acl.bft.HashDag.Hash

import scala.collection.mutable

object Sync {
  // Optimization for case when remotes HashDag is a strict subset of local HashDag
  def missingInSubsetHashDag[RDT](local: HashDag[RDT], remoteHeads: Set[Hash]): Option[Seq[RDT]] = {
    if !remoteHeads.forall(local.ops.contains) then return None

    val result: mutable.ArrayBuffer[RDT] = mutable.ArrayBuffer.empty
    val contained: mutable.Set[Hash]     = mutable.Set.from(remoteHeads)
    val queue: mutable.Queue[Hash]       = mutable.Queue.from(local.heads -- remoteHeads)
    while queue.nonEmpty do
        val nextHash = queue.dequeue()
        val delta    = local.ops(nextHash)
        result.append(delta)

    Some(result.toSeq)
  }
}
