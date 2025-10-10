package bfttravelplanner
import crypto.Ed25519Util
import dag.{HashDAG, SyncRequest, Event}
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import ex2024travel.lofi_acl.travelplanner.TravelPlan.UniqueId
import rdts.base.LocalUid
import riblt.CodedSymbol

type Delta = BFTTravelPlan

case class BFTTravelPlan(state: TravelPlan, causalContext: HashDAG[TravelPlan]):
  def changeTitle(newTitle: String): Delta = {
    val delta = state.changeTitle(newTitle)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))
  }

  def addBucketListEntry(text: String)(using localUid: LocalUid): Delta =
    val delta = state.addBucketListEntry(text)
    BFTTravelPlan(delta, causalContext.generateDelta(delta))

  def setBucketListEntryText(bucketListId: UniqueId, text: String)(using localUid: LocalUid): Delta =
    val delta = state.setBucketListEntryText(bucketListId, text)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))

  def addExpense(description: String, amount: String)(using localUid: LocalUid): Delta =
    val delta = state.addExpense(description, amount)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))

  def setExpenseAmount(expenseId: UniqueId, amount: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseAmount(expenseId, amount)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))

  def setExpenseDescription(expenseId: UniqueId, description: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseDescription(expenseId, description)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))

  def setExpenseComment(expenseId: UniqueId, comment: String)(using localUid: LocalUid): Delta =
    val delta = state.setExpenseComment(expenseId, comment)
    BFTTravelPlan(delta, causalContext.empty.generateDelta(delta))

  def merge(other: BFTTravelPlan): BFTTravelPlan =
    // BFTTravelPlan(this.state.merge(other.state), this.causalContext.merge(other.causalContext))
    val newCausalContext = this.causalContext.merge(other.causalContext)
    var state = this.state
    for event <- other.causalContext.events.values ++ other.causalContext.queue do
      val delta = event.content
      if newCausalContext.contains(event) && delta.nonEmpty then
        state = state.merge(delta.get)

    BFTTravelPlan(state, newCausalContext)

  def mergeEevents(events: Set[Event[TravelPlan]]): BFTTravelPlan =
    var state = this.state
    var cc = this.causalContext
    for event <- events do
      val delta = event.content
      cc = cc.effector(event)
      if cc.contains(event) && delta.nonEmpty then
        state = state.merge(delta.get)

    BFTTravelPlan(state, cc)
    
  def processQueue: BFTTravelPlan =
    var newCausalContext = this.causalContext
    var newState = this.state
    var i = newCausalContext.queue.size
    while i != 0 do {
      i = 0
      val q = newCausalContext.queue
      newCausalContext = newCausalContext.processQueue()
      for event <- q -- newCausalContext.queue do {
        if event.content.nonEmpty then
          i = i + 0
          newState = newState.merge(event.content.get)
      }
    }
    
    BFTTravelPlan(newState, newCausalContext)

  def syncDone: Boolean =
    this.causalContext.riblt.isDecoded

  def sendCodedSymbols(count: Int = 1): List[CodedSymbol[String]] =
    this.causalContext.sendCodedSymbols(count)

  def receiveCodedSymbols(codedSymbols: List[CodedSymbol[String]]): BFTTravelPlan =
    this.copy(causalContext = causalContext.receiveCodedSymbols(codedSymbols))

  def sendSyncRequest: SyncRequest[TravelPlan] =
    causalContext.sendSyncRequest

  def receiveSyncRequest(syncRequest: SyncRequest[TravelPlan]): (BFTTravelPlan, Set[Event[TravelPlan]]) = {
    val (newCausalContext, missingEvents) = causalContext.receiveSyncRequest(syncRequest)
    var state = this.state
    for event <- syncRequest.events do
      val delta = event.content
      if newCausalContext.contains(event) && delta.nonEmpty then
        state = state.merge(delta.get)

    (
      BFTTravelPlan(state, newCausalContext),
      missingEvents
    )
  }

object BFTTravelPlan:
  def apply(): BFTTravelPlan =
    BFTTravelPlan(TravelPlan.empty, HashDAG(Ed25519Util.generateNewKeyPair))
