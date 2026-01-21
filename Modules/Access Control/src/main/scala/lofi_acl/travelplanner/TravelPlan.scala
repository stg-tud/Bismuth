package lofi_acl.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.softwaremill.quicklens.*
import lofi_acl.permission_pane.SelectorFactory
import lofi_acl.travelplanner.TravelPlan.{*, given}
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.filters.Filter
import rdts.time.Dots

import java.util.Base64
import scala.util.Random

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: ObserveRemoveMap[UniqueId, LastWriterWins[String]],
    expenses: ObserveRemoveMap[UniqueId, Expense]
) derives Lattice, Bottom, Filter, Decompose {
  def setTitle(newTitle: String): Delta =
    this.deltaModify(_.title).using(_.write(newTitle))

  def addBucketListEntry(text: String)(using localUid: LocalUid): Delta = {
    val key = randomIdentifier
    this.deltaModify(_.bucketList).using { ormap =>
      ormap.transform(key) {
        case None => Some(LastWriterWins.now(text))
        case _    => ???
      }
    }
  }

  def setBucketListEntryText(bucketListId: UniqueId, text: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.bucketList).using { ormap =>
      ormap.transform(bucketListId) {
        case Some(prior) => Some(prior.write(text))
        case None        => Some(LastWriterWins.now(text))
      }
    }
  }

  def removeBucketListEntry(bucketListId: UniqueId): Delta = {
    this.deltaModify(_.bucketList).using { ormap =>
      ormap.remove(bucketListId)
    }
  }

  def addExpense(description: String, amount: String)(using localUid: LocalUid): Delta = {
    val key = randomIdentifier
    this.deltaModify(_.expenses).using { ormap =>
      val expense =
        Expense(LastWriterWins.now(Some(description)), LastWriterWins.now(Some(amount)), LastWriterWins.empty)
      ormap.transform(key) {
        case None => Some(expense)
        case _    => ???
      }
    }
  }

  def removeExpense(key: UniqueId): Delta = {
    this.deltaModify(_.expenses).using { ormap =>
      ormap.remove(key)
    }
  }

  def setExpenseAmount(expenseId: UniqueId, amount: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.expenses).using { ormap =>
      ormap.transform(expenseId) {
        case Some(prior: Expense) =>
          Some(prior.deltaModify(_.amount).using(_.write(Some(amount))))
        case None => ???
      }
    }
  }

  def setExpenseDescription(expenseId: UniqueId, description: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.expenses).using { ormap =>
      ormap.transform(expenseId) {
        case Some(prior) =>
          Some(prior.deltaModify(_.description).using(_.write(Some(description))))
        case None => ???
      }
    }
  }

  def setExpenseComment(expenseId: UniqueId, comment: String)(using localUid: LocalUid): Delta = {
    val commentValue = if comment.isEmpty then None else Some(comment)
    this.deltaModify(_.expenses).using { ormap =>
      ormap.transform(expenseId) {
        case Some(prior) =>
          Some(prior.deltaModify(_.comment).using(_.write(commentValue)))
        case None => ???
      }
    }
  }
}

case class Expense(
    description: LastWriterWins[Option[String]],
    amount: LastWriterWins[Option[String]],
    comment: LastWriterWins[Option[String]],
) derives Lattice, Bottom, Filter, Decompose

object TravelPlan {
  private val base64Encoder            = Base64.getEncoder
  private val random                   = Random
  private def randomIdentifier: String =
    base64Encoder.encodeToString(random.nextBytes(6))

  type Title = String
  given Bottom[Title]                                                 = Bottom.provide("")
  given titleFilter: Filter[LastWriterWins[Title]]                    = Filter.terminalLwwFilter
  given lwwOptionStringFilter: Filter[LastWriterWins[Option[String]]] = Filter.terminalLwwFilter
  type UniqueId = String
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
      import replication.JsoniterCodecs.given
      JsonCodecMaker.make[TravelPlan](CodecMakerConfig.withMapAsArray(true))
}

object Expense {
  val empty: Expense = Bottom[Expense].empty
}
