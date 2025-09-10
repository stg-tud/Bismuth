package ex2024travel.lofi_acl.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.softwaremill.quicklens.*
import ex2024travel.lofi_acl.travelplanner.TravelPlan.{*, given}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.filters.Filter
import rdts.time.Dots

import java.util.Base64
import scala.util.Random

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: ObserveRemoveMap[UniqueId, LastWriterWins[String]],
    expenses: ObserveRemoveMap[UniqueId, Expense]
) derives Lattice, Bottom, Filter {
  def changeTitle(newTitle: String): Delta = {
    this.deltaModify(_.title).using(_.write(newTitle))
  }

  def addBucketListEntry(text: String)(using localUid: LocalUid): Delta = {
    val key = randomKey
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

  def addExpense(description: String, amount: String)(using localUid: LocalUid): Delta = {
    val key = randomKey
    this.deltaModify(_.expenses).using { ormap =>
      val expense =
        Expense(LastWriterWins.now(Some(description)), LastWriterWins.now(Some(amount)), LastWriterWins.now(None))
      ormap.transform(key) {
        case None => Some(expense)
        case _    => ???
      }
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
) derives Lattice, Bottom, Filter

object TravelPlan {
  private val base64Encoder     = Base64.getEncoder
  private val random            = Random
  private def randomKey: String =
    base64Encoder.encodeToString(random.nextBytes(6))

  type Title = String
  given Bottom[Title]                                                 = Bottom.provide("")
  given titleFilter: Filter[LastWriterWins[Title]]                    = Filter.terminalLwwFilter
  given lwwOptionStringFilter: Filter[LastWriterWins[Option[String]]] = Filter.terminalLwwFilter
  type UniqueId = String
  val empty: TravelPlan = Bottom[TravelPlan].empty

  type Delta = TravelPlan

  import ex2024travel.lofi_acl.sync.JsoniterCodecs.uidKeyCodec
  given jsonCodec: JsonValueCodec[TravelPlan] = JsonCodecMaker.make[TravelPlan]
}

object Expense {
  val empty: Expense = Bottom[Expense].empty
}
