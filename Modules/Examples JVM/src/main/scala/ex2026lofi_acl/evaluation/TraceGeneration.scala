package ex2026lofi_acl.evaluation

import crypto.PublicIdentity
import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026lofi_acl.travelplanner.TravelPlan
import BenchmarkHelper.*
import TravelPlanMutatorChoice.*
import ex2026lofi_acl.bft.{Acl, AclRdt, BftDelta}
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import rdts.time.{ArrayRanges, Dots}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object TraceGeneration {

  def generateDeltas(
      acl: Acl,
      identities: Array[PublicIdentity],
      numDeltasPerReplica: Int,
      minMapEntriesPerReplica: Int,
      maxMapEntriesPerReplica: Int,
  )(using random: Random): Array[Array[TravelPlan]] = {
    identities.map { id =>
      val permittedMutators = TraceGeneration.permittedMutators(acl.write(id))
      given LocalUid        = LocalUid(Uid(id.id))

      @tailrec
      def genRec(deltas: List[TravelPlan], accState: TravelPlan, remaining: Int): Array[TravelPlan] = {
        if remaining > 0 then
            val delta = TraceGeneration.performRandomRdtAction(
              permittedMutators,
              minMapEntriesPerReplica,
              maxMapEntriesPerReplica,
              accState
            )
            genRec(delta :: deltas, accState.merge(delta), remaining - 1)
        else deltas.reverse.toArray
      }

      genRec(Nil, TravelPlan.empty, numDeltasPerReplica)
    }
  }

  def countDecomposed(trace: Array[TravelPlan]): Int = trace.map(delta => delta.decomposed.size).sum

  def countDecomposed(trace: Array[Array[TravelPlan]]): Int = trace.map(countDecomposed).sum

  def performRandomRdtAction(
      permittedMutators: Array[TravelPlanMutatorChoice],
      minEntriesPerMap: Int,
      maxEntriesPerMap: Int,
      state: TravelPlan,
  )(using random: Random, author: LocalUid): TravelPlan = {
    val delta = retryUntilSuccess { // Need to retry, because removal/update doesn't work on empty collection
      permittedMutators(random.nextInt(permittedMutators.length)) match {
        case SET_TITLE                                                            => state.setTitle(dummy)
        case ADD_BUCKET_LIST_ENTRY if state.bucketList.size < maxEntriesPerMap    => state.addBucketListEntry(dummy)
        case REMOVE_BUCKET_LIST_ENTRY if state.bucketList.size > minEntriesPerMap =>
          state.removeBucketListEntry(pickOne(state.bucketList.keySet))
        case SET_BUCKET_LIST_ENTRY_TEXT => state.setBucketListEntryText(pickOne(state.bucketList.keySet), dummy)
        case ADD_EXPENSE if state.expenses.size < maxEntriesPerMap    => state.addExpense(dummy, dummy)
        case REMOVE_EXPENSE if state.expenses.size > minEntriesPerMap =>
          state.removeExpense(pickOne(state.expenses.keySet))
        case SET_EXPENSE_AMOUNT      => state.setExpenseAmount(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_DESCRIPTION => state.setExpenseDescription(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_COMMENT     => state.setExpenseComment(pickOne(state.expenses.keySet), dummy)
        case _                       => ???
      }
    }
    delta
  }

  def permittedMutators(writePerm: PermissionTree): Array[TravelPlanMutatorChoice] =
      val mutators = mutable.ListBuffer.empty[TravelPlanMutatorChoice]

      if PermissionTree.fromPath("title") <= writePerm then mutators.addOne(SET_TITLE): Unit
      if PermissionTree.fromPath("bucketList") <= writePerm then
          mutators.addOne(ADD_BUCKET_LIST_ENTRY).addOne(REMOVE_BUCKET_LIST_ENTRY)
            .addOne(SET_BUCKET_LIST_ENTRY_TEXT): Unit
      if PermissionTree.fromPath("expenses") <= writePerm then
          mutators
            .addOne(ADD_EXPENSE).addOne(REMOVE_EXPENSE)
            .addOne(SET_EXPENSE_AMOUNT).addOne(SET_EXPENSE_DESCRIPTION).addOne(SET_EXPENSE_COMMENT): Unit

      mutators.toArray

  def generateReplicaIds(numReplicas: Int): Array[PrivateIdentity] = {
    require(numReplicas >= 1)
    (0 until numReplicas).map(_ => IdentityFactory.createNewIdentity).toArray
  }

  def getAclWithRandomWritePermissions(replicaIds: Array[PublicIdentity])(using random: Random): Acl = {
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

    Acl(
      // For benchmarks, we require that all replicas are allowed to read everything in order to measure overhead
      // compared to non-enforcing variant.
      read = replicaIds.map(id => id -> PermissionTree.allow).toMap,
      write = replicaIds.map(id => id -> pickRandomPermissions).toMap
    )
  }

  def generateTrace(
      numReplicas: Int,
      numDeltasPerReplica: Int,
      minEntriesPerMapPerReplica: Int,
      maxEntriesPerMapPerReplica: Int,
  )(using random: Random): Trace = {
    val replicaIds = TraceGeneration.generateReplicaIds(numReplicas)
    val genesis    = AclRdt.createSelfSignedRoot(replicaIds(0))

    // Generate permissions for non-root replicas:
    val permissionsToAssign = TraceGeneration.getAclWithRandomWritePermissions(replicaIds.drop(1).map(_.getPublic))
    val trace               = TraceGeneration.generateDeltas(
      genesis.state.merge(permissionsToAssign),
      replicaIds.map(_.getPublic),
      numDeltasPerReplica,
      minEntriesPerMapPerReplica,
      maxEntriesPerMapPerReplica
    )

    Trace(replicaIds, genesis, permissionsToAssign, trace)
  }
}

case class Trace(
    ids: Array[PrivateIdentity],
    genesis: BftDelta[Acl],
    additionalPermissions: Acl,
    deltas: Array[Array[TravelPlan]]
):
    def computeEndStateVersion: Dots = {
      // Currently, we decompose deltas in both variants (enforcing and non-enforcing)
      Dots(
        ids.map(id => Uid(id.getPublic.id))
          .zip {
            // (if withDecomposition then deltas.map(TraceGeneration.countDecomposed) else deltas.map(_.length))
            deltas.map(TraceGeneration.countDecomposed).map(end => ArrayRanges(Seq(0L -> end)))
          }.toMap
      )
    }
