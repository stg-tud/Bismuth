package ex2026accessControl.authz.travelplanner.model

import channels.crypto.{PrivateIdentity, PublicIdentity}
import ex2026accessControl.authz.travelplanner.TravelPlan.given
import ex2026accessControl.authz.travelplanner.{Invitation, Sync, TravelPlan}
import rdts.base.{ReplicaId, Uid}
import rdts.datatypes.LastWriterWins
import rdts.filters.{KeyAsString, PermissionTree}
import rdts.time.Dot
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.global

// The init checker (-Wsafe-init) cannot verify this constructor:
// handlers that capture the model are registered during construction.
@scala.annotation.nowarn("msg=initialized|initialization")
class TravelPlanModel(
    private val localIdentity: PrivateIdentity,
    syncProvider: (stateChanged: TravelPlan => Unit) => Sync[TravelPlan]
) {
  val publicId: PublicIdentity = localIdentity.getPublic

  private given localUid: ReplicaId = ReplicaId(Uid(publicId.id))

  def state: TravelPlan = sync.currentState

  def availablePermissions: Map[PublicIdentity, (read: PermissionTree, write: PermissionTree)] =
    sync.availablePermissions

  val sync: Sync[TravelPlan] = syncProvider(stateChanged)
  sync.start()
  Runtime.getRuntime.addShutdownHook(new Thread(() => sync.stop()))

  def grantPermission(
      affectedUser: PublicIdentity,
      readPermissions: PermissionTree,
      writePermissions: PermissionTree
  ): Unit = sync.grantPermissions(affectedUser, readPermissions, writePermissions)

  def createInvitation: Invitation =
    sync.createInvitation

  def addConnection(remoteUser: PublicIdentity, address: String): Unit =
    sync.connect(remoteUser, address)

  def changeTitle(newTitle: String): Unit =
    mutateRdt(_.setTitle(newTitle))

  def addBucketListEntry(text: String): Unit =
    mutateRdt(_.addBucketListEntry(text))

  def setBucketListEntryText(bucketListId: String, text: String): Unit =
    mutateRdt(_.setBucketListEntryText(idFromString(bucketListId), text))

  def addExpense(description: String, amount: String): Unit =
    mutateRdt(_.addExpense(description, amount))

  def setExpenseAmount(expenseId: String, amount: String): Unit =
    mutateRdt(_.setExpenseAmount(idFromString(expenseId), amount))

  def setExpenseDescription(expenseId: String, description: String): Unit =
    mutateRdt(_.setExpenseDescription(idFromString(expenseId), description))

  def setExpenseComment(expenseId: String, comment: String): Unit =
    mutateRdt(_.setExpenseComment(idFromString(expenseId), comment))

  /** the UI identifies entries by the string encoding of their dot, the same encoding is used in permission trees */
  private def idToString(id: Dot): String   = KeyAsString[Dot].encode(id)
  private def idFromString(id: String): Dot = KeyAsString[Dot].decode(id)

  private def mutateRdt(mutator: TravelPlan => TravelPlan): Unit =
    global.execute { () =>
      sync.mutateState(mutator)
    }

  val title: StringProperty                      = StringProperty(state.title.read)
  val bucketListIdList: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.ids.map(idToString))
  private var bucketListIdSet: Set[String]       = bucketListIdList.toSet
  val bucketListProperties: AtomicReference[Map[String, StringProperty]] =
    AtomicReference(state.bucketList.elements.map((id, lww) => idToString(id) -> StringProperty(lww.read)))
  val expenseIdList: ObservableBuffer[String] = ObservableBuffer.from(state.expenses.ids.map(idToString))
  private var expenseIdSet: Set[String]       = expenseIdList.toSet
  val expenseListProperties: AtomicReference[Map[String, (StringProperty, StringProperty, StringProperty)]] =
    AtomicReference(state.expenses.elements.map { (id, expense) =>
      idToString(id) -> (
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
    val bucketListEntriesInDelta = delta.bucketList.elements.map((id, entry) => idToString(id) -> entry)
    if bucketListEntriesInDelta.nonEmpty then
        val newIds = bucketListEntriesInDelta.keySet.diff(bucketListIdSet)
        val props  = bucketListProperties.updateAndGet(oldProps =>
          oldProps ++ newIds.map(id => id -> StringProperty(""))
        )
        bucketListEntriesInDelta.foreach { (id, entry) =>
          props(id).value = entry.read
        }
        bucketListIdList.addAll(newIds)
        bucketListIdSet = bucketListIdSet ++ newIds

    // Expenses
    val expenseEntriesInDelta = delta.expenses.elements.map((id, entry) => idToString(id) -> entry)
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
          val curValue = newTravelPlan.expenses.get(idFromString(id)).get
          description.value = curValue.description.read.getOrElse("")
          amount.value = curValue.amount.read.getOrElse("0.00 €")
          comment.value = curValue.comment.read.getOrElse("")

        }
        expenseIdList.addAll(newIds)
        expenseIdSet = expenseIdSet ++ newIds
  }
}
