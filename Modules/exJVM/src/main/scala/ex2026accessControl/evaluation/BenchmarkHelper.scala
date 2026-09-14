package ex2026accessControl.evaluation

import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2026accessControl.evaluation.BenchmarkHelper.BenchmarkRdtMutatorChoice.*
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

  /** The leaf permission path (a subset of [[BenchmarkRdt.benchmarkRdtPerms]]) that each [[BenchmarkRdtMutatorChoice]]
    * mutates.
    */
  private val benchmarkRdtMutatorPath: Map[BenchmarkRdtMutatorChoice, String] = Map(
    WRITE_A             -> "a",
    ADD_B_X             -> "b.x",
    ADD_B_Y             -> "b.y",
    WRITE_B_Z           -> "b.z",
    ADD_C_ALPHA_X       -> "c.alpha.x",
    ADD_C_ALPHA_Y       -> "c.alpha.y",
    WRITE_C_ALPHA_Z     -> "c.alpha.z",
    ADD_C_BETA          -> "c.beta",
    ADD_D_ONE_ALPHA_X   -> "d.one.alpha.x",
    ADD_D_ONE_ALPHA_Y   -> "d.one.alpha.y",
    WRITE_D_ONE_ALPHA_Z -> "d.one.alpha.z",
    ADD_D_ONE_BETA      -> "d.one.beta",
    WRITE_D_TWO         -> "d.two",
  )

  /** The [[BenchmarkRdtMutatorChoice]]s permitted by `writePerm`. */
  def permittedBenchmarkRdtMutators(writePerm: PermissionTree): Array[BenchmarkRdtMutatorChoice] =
    BenchmarkRdtMutatorChoice.values.filter(choice =>
      PermissionTree.fromPath(benchmarkRdtMutatorPath(choice)) <= writePerm
    )

  def randomMutatorChoice(permittedMutators: Array[BenchmarkRdtMutatorChoice])(using
      random: Random
  ): BenchmarkRdtMutatorChoice =
    permittedMutators(random.nextInt(permittedMutators.length))

  /** Applies the mutation identified by `choice` to the corresponding leaf field of [[BenchmarkRdt]]. */
  def applyBenchmarkRdtMutator(
      choice: BenchmarkRdtMutatorChoice,
      state: BenchmarkRdt,
  )(using random: Random, author: LocalUid): BenchmarkRdt = {
    def randomIntValue: Int                                        = random.nextInt(1000)
    def randomCounterUpdate(counter: PosNegCounter): PosNegCounter =
      if random.nextBoolean() then counter.inc() else counter.dec()

    choice match {
      case WRITE_A             => state.deltaModify(_.a).using(_.write(randomIntValue))
      case ADD_B_X             => state.deltaModify(_.b.x).using(randomCounterUpdate)
      case ADD_B_Y             => state.deltaModify(_.b.y).using(randomCounterUpdate)
      case WRITE_B_Z           => state.deltaModify(_.b.z).using(_.write(randomIntValue))
      case ADD_C_ALPHA_X       => state.deltaModify(_.c.alpha.x).using(randomCounterUpdate)
      case ADD_C_ALPHA_Y       => state.deltaModify(_.c.alpha.y).using(randomCounterUpdate)
      case WRITE_C_ALPHA_Z     => state.deltaModify(_.c.alpha.z).using(_.write(randomIntValue))
      case ADD_C_BETA          => state.deltaModify(_.c.beta).using(randomCounterUpdate)
      case ADD_D_ONE_ALPHA_X   => state.deltaModify(_.d.one.alpha.x).using(randomCounterUpdate)
      case ADD_D_ONE_ALPHA_Y   => state.deltaModify(_.d.one.alpha.y).using(randomCounterUpdate)
      case WRITE_D_ONE_ALPHA_Z => state.deltaModify(_.d.one.alpha.z).using(_.write(randomIntValue))
      case ADD_D_ONE_BETA      => state.deltaModify(_.d.one.beta).using(randomCounterUpdate)
      case WRITE_D_TWO         => state.deltaModify(_.d.two).using(_.write(randomIntValue))
    }
  }

  enum BenchmarkRdtMutatorChoice:
      case WRITE_A
      case ADD_B_X
      case ADD_B_Y
      case WRITE_B_Z
      case ADD_C_ALPHA_X
      case ADD_C_ALPHA_Y
      case WRITE_C_ALPHA_Z
      case ADD_C_BETA
      case ADD_D_ONE_ALPHA_X
      case ADD_D_ONE_ALPHA_Y
      case WRITE_D_ONE_ALPHA_Z
      case ADD_D_ONE_BETA
      case WRITE_D_TWO
}
