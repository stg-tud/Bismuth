package ex2026accessControl.evaluation

import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026accessControl.evaluation.BenchmarkRdt.given
import ex2026accessControl.evaluation.TravelPlanMutatorChoice.*
import ex2026accessControl.travelplanner.TravelPlan
import rdts.base.LocalUid
import rdts.datatypes.PosNegCounter
import rdts.filters.PermissionTree
import rdts.syntax.deltalens.*

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

  /** Applies a random mutation to the leaf field of [[BenchmarkRdt]] identified by one of the paths in
    * `permittedMutators` (a subset of [[BenchmarkRdt.benchmarkRdtPerms]], see [[permittedBenchmarkRdtMutators]]).
    */
  def randomBenchmarkRdtDelta(
      permittedMutators: Array[String],
      state: BenchmarkRdt,
  )(using random: Random, author: LocalUid): BenchmarkRdt = {
    def randomIntValue: Int = random.nextInt(1000)
    def randomCounterUpdate(counter: PosNegCounter): PosNegCounter =
      if random.nextBoolean() then counter.inc() else counter.dec()

    permittedMutators(random.nextInt(permittedMutators.length)) match {
      case "a"             => state.deltaModify(_.a).using(_.write(randomIntValue))
      case "b.x"           => state.deltaModify(_.b.x).using(randomCounterUpdate)
      case "b.y"           => state.deltaModify(_.b.y).using(randomCounterUpdate)
      case "b.z"           => state.deltaModify(_.b.z).using(_.write(randomIntValue))
      case "c.alpha.x"     => state.deltaModify(_.c.alpha.x).using(randomCounterUpdate)
      case "c.alpha.y"     => state.deltaModify(_.c.alpha.y).using(randomCounterUpdate)
      case "c.alpha.z"     => state.deltaModify(_.c.alpha.z).using(_.write(randomIntValue))
      case "c.beta"        => state.deltaModify(_.c.beta).using(randomCounterUpdate)
      case "d.one.alpha.x" => state.deltaModify(_.d.one.alpha.x).using(randomCounterUpdate)
      case "d.one.alpha.y" => state.deltaModify(_.d.one.alpha.y).using(randomCounterUpdate)
      case "d.one.alpha.z" => state.deltaModify(_.d.one.alpha.z).using(_.write(randomIntValue))
      case "d.one.beta"    => state.deltaModify(_.d.one.beta).using(randomCounterUpdate)
      case "d.two"         => state.deltaModify(_.d.two).using(_.write(randomIntValue))
    }
  }

  /** The paths from [[BenchmarkRdt.benchmarkRdtPerms]] that are permitted by `writePerm` and name a leaf field
    * (i.e. have no other permission path nested underneath them), since only those can be mutated directly.
    */
  def permittedBenchmarkRdtMutators(writePerm: PermissionTree): Array[String] =
      def isLeafPermission(path: String): Boolean =
        !BenchmarkRdt.benchmarkRdtPerms.exists(other => other != path && other.startsWith(s"$path."))

      BenchmarkRdt.benchmarkRdtPerms.iterator
        .filter(path => isLeafPermission(path) && PermissionTree.fromPath(path) <= writePerm)
        .toArray
}
