package lofi_acl.bft

import lofi_acl.bft.HashDag.Delta

import scala.collection.mutable

// One approach for sync is described in https://arxiv.org/pdf/2012.00472
object HashDagSync {
  // Optimization for case when remotes HashDag is a strict subset of local HashDag
  def missingInSubsetHashDag[D <: Delta[State], State](
      local: HashDag[D, State],
      remoteHeads: Set[Hash]
  ): Option[Seq[D]] = {
    if !remoteHeads.forall(local.deltas.contains) then return None

    var result                       = List.empty[D]
    val contained: mutable.Set[Hash] = mutable.Set.from(remoteHeads)
    val queue: mutable.Queue[Hash]   = mutable.Queue.from(local.heads -- remoteHeads)
    while queue.nonEmpty do
        val nextHash = queue.dequeue()
        val delta    = local.deltas(nextHash)
        queue ++= delta.parents.diff(contained)
        contained += nextHash
        result = delta :: result

    Some(result)
  }
}
