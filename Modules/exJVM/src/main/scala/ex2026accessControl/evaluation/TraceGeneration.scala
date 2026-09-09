package ex2026accessControl.evaluation

import crypto.PublicIdentity
import ex2026accessControl.travelplanner.TravelPlan
import rdts.filters.PermissionTree
import replication.authz.{ArdtEventGraph, Authorization}

import scala.util.Random

object TraceGeneration {

  def countDecomposed(trace: Array[TravelPlan]): Int = trace.map(delta => delta.decomposed.size).sum

  def countDecomposed(trace: Array[Array[TravelPlan]]): Int = trace.map(countDecomposed).sum

  def pickRandomPermissions(
      replicaIds: Array[PublicIdentity]
  )(using random: Random): Map[PublicIdentity, PermissionTree] = {
    // Pick one to three random permissions
    def pickRandomPermissions: PermissionTree = {
      var resultingPerm = PermissionTree.empty
      // Pick one to three distinct permissions
      var numPerms         = random.between(1, 4)
      var remainingChoices = Seq("title", "bucketList", "expenses")
      while numPerms > 0 do
          numPerms = numPerms - 1
          val choice = random.between(0, remainingChoices.size)
          resultingPerm = resultingPerm.merge(PermissionTree.fromPath(remainingChoices(choice)))
          remainingChoices = remainingChoices.patch(choice, Nil, 1) // Remove chosen permission

      resultingPerm
    }

    replicaIds.map(id => id -> pickRandomPermissions).toMap
  }

  def generateEventGraph(
      numReplicas: Int,
      numDeltasPerReplica: Int,
      minEntriesPerMapPerReplica: Int,
      maxEntriesPerMapPerReplica: Int,
  )(using random: Random): ArdtEventGraph[TravelPlan] = {
    val replicaIds       = BenchmarkHelper.generateReplicaIds(numReplicas)
    val writePermissions =
      pickRandomPermissions(replicaIds.drop(1).map(_.getPublic)) + (replicaIds(0).getPublic -> PermissionTree.allow)

    val deltas = BenchmarkHelper.generateDeltas(
      writePermissions,
      replicaIds.map(_.getPublic),
      numDeltasPerReplica,
      minEntriesPerMapPerReplica,
      maxEntriesPerMapPerReplica
    )

    val genesis    = Authorization.createGenesis(replicaIds(0))
    var eventGraph = ArdtEventGraph(genesis)

    ???
  }
}
