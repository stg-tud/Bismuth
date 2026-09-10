package ex2026accessControl.evaluation

import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026accessControl.evaluation.TravelPlanMutatorChoice.*
import ex2026accessControl.travelplanner.TravelPlan
import rdts.base.LocalUid
import rdts.filters.PermissionTree

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object BenchmarkHelper {

  def dummy(using random: Random): String = random.alphanumeric.take(20).mkString("")

  def pickOne[V](set: Set[V])(using random: Random): V = set.drop(random.nextInt(set.size)).head

  @tailrec
  def retryUntilSuccess[T](action: => T): T =
    try
      action
    catch {
      case _: Throwable => retryUntilSuccess(action)
    }

  def randomTravelPlanDelta(
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
}
