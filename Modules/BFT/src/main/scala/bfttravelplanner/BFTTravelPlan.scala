package bfttravelplanner
import crypto.Ed25519Util
import dag.{Event, HashDAG, SyncRequest}
import datatypes.Replica
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import ex2024travel.lofi_acl.travelplanner.TravelPlan.UniqueId
import rdts.base.LocalUid
import riblt.{CodedSymbol, RIBLT}
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}

type Delta = BFTTravelPlan

case class BFTTravelPlan(state: TravelPlan, hashDAG: HashDAG[TravelPlan], riblt: RIBLT[String])
    extends Replica[TravelPlan, BFTTravelPlan]:

  def changeTitle(newTitle: String): Delta = {
    val delta = state.changeTitle(newTitle)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)
  }

  def addBucketListEntry(text: String)(using localUid: LocalUid): Delta =
    val delta = state.addBucketListEntry(text)
    BFTTravelPlan(delta, hashDAG.generateDelta(delta), RIBLT.empty)

  def setBucketListEntryText(bucketListId: UniqueId, text: String)(using localUid: LocalUid): Delta =
    val delta = state.setBucketListEntryText(bucketListId, text)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)

  def addExpense(description: String, amount: String)(using localUid: LocalUid): Delta =
    val delta = state.addExpense(description, amount)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)

  def setExpenseAmount(expenseId: UniqueId, amount: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseAmount(expenseId, amount)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)

  def setExpenseDescription(expenseId: UniqueId, description: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseDescription(expenseId, description)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)

  def setExpenseComment(expenseId: UniqueId, comment: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseComment(expenseId, comment)
    BFTTravelPlan(delta, hashDAG.empty.generateDelta(delta), RIBLT.empty)

  def merge(other: BFTTravelPlan): BFTTravelPlan =
    // BFTTravelPlan(this.state.merge(other.state), this.causalContext.merge(other.causalContext))
    val newHashDAG = this.hashDAG.merge(other.hashDAG)
    var state      = this.state
    for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      val delta = event.content
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) && delta.nonEmpty then {
        state = state.merge(delta.get)
        this.riblt.addSymbol(event.id)
      }

    BFTTravelPlan(state, newHashDAG, riblt)

  def mergeEevents(events: Set[Event[TravelPlan]]): BFTTravelPlan =
    var state      = this.state
    var newHashDAG = this.hashDAG
    for event <- events do
      val delta = event.content
      newHashDAG = newHashDAG.effector(event)
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) && delta.nonEmpty then {
        state = state.merge(delta.get)
        this.riblt.addSymbol(event.id)
      }

    BFTTravelPlan(state, newHashDAG, this.riblt)

  def processQueue: BFTTravelPlan =
    var newHashDAG = this.hashDAG
    var newState   = this.state
    var i          = newHashDAG.queue.size
    while i != 0 do {
      i = 0
      val q = newHashDAG.queue
      newHashDAG = newHashDAG.processQueue()
      for event <- q -- newHashDAG.queue do {
        if event.content.nonEmpty then
          i = i + 0
          newState = newState.merge(event.content.get)
          this.riblt.addSymbol(event.id)
      }
    }

    BFTTravelPlan(newState, newHashDAG, this.riblt)

  override def empty: Delta = BFTTravelPlan()

  override def withHashDAG(hashDAG: HashDAG[TravelPlan]): Delta =
    this.copy(hashDAG = hashDAG)

object BFTTravelPlan:
  def apply(): BFTTravelPlan =
    BFTTravelPlan(TravelPlan.empty, HashDAG(Ed25519Util.generateNewKeyPair), RIBLT())
