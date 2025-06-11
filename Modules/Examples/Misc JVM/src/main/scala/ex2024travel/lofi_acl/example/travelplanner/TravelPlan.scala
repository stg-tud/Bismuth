package ex2024travel.lofi_acl.example.travelplanner

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.softwaremill.quicklens.*
import ex2024travel.lofi_acl.example.travelplanner.TravelPlan.{*, given}
import lofi_acl.access.Filter
import lofi_acl.ardt.datatypes.LWW
import lofi_acl.collections.DeltaAWLWWMContainer.Entry
import lofi_acl.collections.ORMap.{observeRemoveMapEntryFilter, stringKeyORMapFilter}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.dotted.{HasDots, Obrem}
import rdts.time.Dots

import java.util.Base64
import scala.util.Random

case class TravelPlan(
    title: LastWriterWins[Title],
    bucketList: ObserveRemoveMap[UniqueId, Entry[LastWriterWins[String]]],
    expenses: ObserveRemoveMap[UniqueId, Entry[Expense]]
) derives Lattice, Bottom, Filter {
  def changeTitle(newTitle: String): Delta = {
    this.deltaModify(_.title).using(_.write(newTitle))
  }

  def addBucketListEntry(text: String)(using localUid: LocalUid): Delta = {
    val key = randomKey
    this.deltaModify(_.bucketList).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      ormap.transformPlain(key) {
        case None => Some(Entry(nextDot, LastWriterWins.now(text)))
        case _    => ???
      }
    }
  }

  def setBucketListEntryText(bucketListId: UniqueId, text: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.bucketList).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      ormap.transformPlain(bucketListId) {
        case Some(prior) => Some(Entry(nextDot, prior.value.write(text)))
        case None        => Some(Entry(nextDot, LastWriterWins.now(text)))
      }
    }
  }

  def addExpense(description: String, amount: String)(using localUid: LocalUid): Delta = {
    val key = randomKey
    this.deltaModify(_.expenses).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      val expense =
        Expense(LastWriterWins.now(Some(description)), LastWriterWins.now(Some(amount)), LastWriterWins.now(None))
      ormap.transformPlain(key) {
        case None => Some(Entry(nextDot, expense))
        case _    => ???
      }
    }
  }

  def setExpenseAmount(expenseId: UniqueId, amount: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.expenses).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      ormap.transformPlain(expenseId) {
        case Some(prior: Entry[Expense]) =>
          Some(Entry(nextDot, prior.value.deltaModify(_.amount).using(_.write(Some(amount)))))
        case None => ???
      }
    }
  }

  def setExpenseDescription(expenseId: UniqueId, description: String)(using localUid: LocalUid): Delta = {
    this.deltaModify(_.expenses).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      ormap.transformPlain(expenseId) {
        case Some(prior: Entry[Expense]) =>
          Some(Entry(nextDot, prior.value.deltaModify(_.description).using(_.write(Some(description)))))
        case None => ???
      }
    }
  }

  def setExpenseComment(expenseId: UniqueId, comment: String)(using localUid: LocalUid): Delta = {
    val commentValue = if comment.isEmpty then None else Some(comment)
    this.deltaModify(_.expenses).using { ormap =>
      val nextDot = Dots.single(ormap.repr.context.nextDot(localUid.uid))
      ormap.transformPlain(expenseId) {
        case Some(prior: Entry[Expense]) =>
          Some(Entry(nextDot, prior.value.deltaModify(_.comment).using(_.write(commentValue))))
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
  given titleFilter: Filter[LastWriterWins[Title]]                    = LWW.terminalFilter
  given lwwOptionStringFilter: Filter[LastWriterWins[Option[String]]] = LWW.terminalFilter
  type UniqueId = String
  val empty: TravelPlan = Bottom[TravelPlan].empty

  type Delta = TravelPlan

  import lofi_acl.sync.JsoniterCodecs.uidKeyCodec
  given jsonCodec: JsonValueCodec[TravelPlan] = JsonCodecMaker.make[TravelPlan]
}

object Expense {
  val empty: Expense = Bottom[Expense].empty
}
