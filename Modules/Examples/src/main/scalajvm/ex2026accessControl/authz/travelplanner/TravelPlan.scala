package ex2026accessControl.authz.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import ex2026accessControl.authz.permission_pane.SelectorFactory
import ex2026accessControl.authz.travelplanner.TravelPlan.{*, given}
import rdts.base.{Bottom, Decompose, Lattice, ReplicaId}
import rdts.datatypes.{DotRemoveMap, LastWriterWins}
import rdts.filters.Filter
import rdts.syntax.deltalens.*
import rdts.time.Dot

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: DotRemoveMap[LastWriterWins[String]],
    expenses: DotRemoveMap[Expense]
) derives Lattice, Bottom, Filter, Decompose {
  def setTitle(newTitle: String): Delta =
    this.deltaModify(_.title).using(_.write(newTitle))

  def addBucketListEntry(text: String)(using localUid: ReplicaId): Delta =
    this.deltaModify(_.bucketList).using(_.insert(LastWriterWins.now(text)))

  /** does nothing if the entry has been removed */
  def setBucketListEntryText(bucketListId: UniqueId, text: String): Delta =
    this.deltaModify(_.bucketList).using(_.transform(bucketListId)(_.write(text)))

  def removeBucketListEntry(bucketListId: UniqueId): Delta =
    this.deltaModify(_.bucketList).using(_.remove(bucketListId))

  def addExpense(description: String, amount: String)(using localUid: ReplicaId): Delta =
    this.deltaModify(_.expenses).using(_.insert(
      Expense(LastWriterWins.now(Some(description)), LastWriterWins.now(Some(amount)), LastWriterWins.empty)
    ))

  def removeExpense(expenseId: UniqueId): Delta =
    this.deltaModify(_.expenses).using(_.remove(expenseId))

  def setExpenseAmount(expenseId: UniqueId, amount: String): Delta =
    this.deltaModify(_.expenses).using(_.transform(expenseId)(_.deltaModify(_.amount).using(_.write(Some(amount)))))

  def setExpenseDescription(expenseId: UniqueId, description: String): Delta =
    this.deltaModify(_.expenses).using(_.transform(expenseId)(
      _.deltaModify(_.description).using(_.write(Some(description)))
    ))

  def setExpenseComment(expenseId: UniqueId, comment: String): Delta = {
    val commentValue = if comment.isEmpty then None else Some(comment)
    this.deltaModify(_.expenses).using(_.transform(expenseId)(_.deltaModify(_.comment).using(_.write(commentValue))))
  }
}

case class Expense(
    description: LastWriterWins[Option[String]],
    amount: LastWriterWins[Option[String]],
    comment: LastWriterWins[Option[String]],
) derives Lattice, Bottom, Filter, Decompose

object TravelPlan {
  type Title = String
  given Bottom[Title]                                                 = Bottom.provide("")
  given titleFilter: Filter[LastWriterWins[Title]]                    = Filter.terminalLwwFilter
  given lwwOptionStringFilter: Filter[LastWriterWins[Option[String]]] = Filter.terminalLwwFilter
  type UniqueId = Dot
  val empty: TravelPlan = Bottom[TravelPlan].empty

  given TravelPlanSelectorFactory: SelectorFactory[TravelPlan] = {
    import SelectorFactory.given
    given SelectorFactory[Option[String]]                 = SelectorFactory.OptionSelectorFactory
    given SelectorFactory[LastWriterWins[Option[String]]] = SelectorFactory.LwwSelectorFactory
    given SelectorFactory[Expense]                        = SelectorFactory.derived
    SelectorFactory.derived
  }

  type Delta = TravelPlan
  given jsonCodec: JsonValueCodec[TravelPlan] =
      given JsonValueCodec[String]         = JsonCodecMaker.make
      given Bottom[String]                 = Bottom.provide("")
      given JsonValueCodec[Option[String]] = JsonCodecMaker.make
      given Bottom[Option[String]]         = Bottom.provide(None)
      import channels.JsoniterCodecs.given
      JsonCodecMaker.make[TravelPlan](CodecMakerConfig.withMapAsArray(true))
}

object Expense {
  val empty: Expense = Bottom[Expense].empty
}
