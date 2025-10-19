package ex2024travel.lofi_acl.travelplanner.model

import crypto.channels.{IdentityFactory, PrivateIdentity}
import crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import ex2024travel.lofi_acl.sync.monotonic.MonotonicAclSyncMessage.AclDelta
import ex2024travel.lofi_acl.sync.monotonic.{MonotonicAcl, MonotonicInvitation, SyncWithMonotonicAcl}
import ex2024travel.lofi_acl.sync.{Acl, Invitation, RDTSync}
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import ex2024travel.lofi_acl.travelplanner.TravelPlan.given
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.LastWriterWins
import rdts.filters.Operation.{READ, WRITE}
import rdts.filters.PermissionTree
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.global

class TravelPlanModel(
    private val localIdentity: PrivateIdentity,
    syncProvider: (TravelPlan => Unit) => RDTSync[TravelPlan]
) {
  val publicId: PublicIdentity = localIdentity.getPublic

  private given localUid: LocalUid = LocalUid(Uid(publicId.id))

  def state: TravelPlan = sync.currentState

  def currentAcl: Acl = sync.currentAcl

  private val sync: RDTSync[TravelPlan] = syncProvider(stateChanged)
  sync.start()
  Runtime.getRuntime.addShutdownHook(new Thread(() => sync.stop()))

  def grantPermission(
      affectedUser: PublicIdentity,
      readPermissions: PermissionTree,
      writePermissions: PermissionTree
  ): Unit = {
    sync.grantPermissions(affectedUser, readPermissions, READ)
    if !writePermissions.isEmpty
    then sync.grantPermissions(affectedUser, writePermissions, WRITE)
  }

  def createInvitation: Invitation =
    sync.createInvitation

  def addConnection(remoteUser: PublicIdentity, address: String): Unit =
    sync.connect(remoteUser, address)

  def changeTitle(newTitle: String): Unit =
    mutateRdt(_.changeTitle(newTitle))

  def addBucketListEntry(text: String): Unit =
    mutateRdt(_.addBucketListEntry(text))

  def setBucketListEntryText(bucketListId: String, text: String): Unit =
    mutateRdt(_.setBucketListEntryText(bucketListId, text))

  def addExpense(description: String, amount: String): Unit =
    mutateRdt(_.addExpense(description, amount))

  def setExpenseAmount(expenseId: String, amount: String): Unit =
    mutateRdt(_.setExpenseAmount(expenseId, amount))

  def setExpenseDescription(expenseId: String, description: String): Unit =
    mutateRdt(_.setExpenseDescription(expenseId, description))

  def setExpenseComment(expenseId: String, comment: String): Unit =
    mutateRdt(_.setExpenseComment(expenseId, comment))

  private def mutateRdt(mutator: TravelPlan => TravelPlan): Unit = {
    global.execute { () =>
      sync.mutateState(mutator)
    }
  }

  val title: StringProperty                      = StringProperty(state.title.read)
  val bucketListIdList: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.inner.keySet)
  private var bucketListIdSet: Set[String]       = bucketListIdList.toSet
  val bucketListProperties: AtomicReference[Map[String, StringProperty]] =
    AtomicReference(state.bucketList.inner.map((id, lww) => id -> StringProperty(lww.value.read)))
  val expenseIdList: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.inner.keySet)
  private var expenseIdSet: Set[String]       = expenseIdList.toSet
  val expenseListProperties: AtomicReference[Map[String, (StringProperty, StringProperty, StringProperty)]] =
    AtomicReference(state.expenses.inner.map { (id, orMapEntry) =>
      val expense = orMapEntry.value
      id -> (
        StringProperty(expense.description.read.getOrElse("")),
        StringProperty(expense.amount.read.getOrElse("0.00 €")),
        StringProperty(expense.comment.read.getOrElse(""))
      )
    })

  private def stateChanged(delta: TravelPlan): Unit = Platform.runLater {
    val newTravelPlan = state
    // Title
    if !delta.title.isEmpty then
      title.value = newTravelPlan.title.read

    // Bucket List Entries
    val bucketListEntriesInDelta = delta.bucketList.inner
    if bucketListEntriesInDelta.nonEmpty then
      val newIds = bucketListEntriesInDelta.keySet.diff(bucketListIdSet)
      val props  = bucketListProperties.updateAndGet(oldProps =>
        oldProps ++ newIds.map(id => id -> StringProperty(""))
      )
      bucketListEntriesInDelta.foreach { (id, entry) =>
        props(id).value = entry.value.read
      }
      bucketListIdList.addAll(newIds)
      bucketListIdSet = bucketListIdSet ++ newIds

    // Expenses
    val expenseEntriesInDelta = delta.expenses.inner
    if expenseEntriesInDelta.nonEmpty then
      val newIds = expenseEntriesInDelta.keySet.diff(expenseIdSet)
      val props  = expenseListProperties.updateAndGet(oldProps =>
        oldProps ++ newIds.map(id =>
          id -> (StringProperty(""), StringProperty("0.00 €"), StringProperty(""))
        )
      )
      expenseEntriesInDelta.foreach { (id, _) =>
        val (description, amount, comment) = props(id)
        // TODO: Would fail on removal of entries
        val curValue = newTravelPlan.expenses.get(id).get
        description.value = curValue.description.read.getOrElse("")
        amount.value = curValue.amount.read.getOrElse("0.00 €")
        comment.value = curValue.comment.read.getOrElse("")

      }
      expenseIdList.addAll(newIds)
      expenseIdSet = expenseIdSet ++ newIds
  }
}
