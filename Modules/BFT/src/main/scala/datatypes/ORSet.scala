package datatypes

import datatypes.OpType.{Add, Remove}
import crypto.Ed25519Util
import dag.{Event, HashDAG, SyncRequest}
import riblt.CodedSymbol

import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}

case class ORSet[T] private (
                              elements: Map[T, Set[String]],
                              causalContext: HashDAG[Op[T]],
                            ):

  def add(element: T): ORSet[T] =
    val op = Op(element, OpType.Add)

    ORSet(Map.empty, causalContext.generateDelta(op))

  def remove(element: T): ORSet[T] =
    val op = Op(element, OpType.Remove)

    ORSet(Map.empty, causalContext.generateDelta(op))
    
  def merge(other: ORSet[T]): ORSet[T] =
    var newElements = this.elements
    val newCausalContext = this.causalContext.merge(other.causalContext)

    for event <- this.causalContext.queue ++ other.causalContext.events.values ++ other.causalContext.queue do
      if newCausalContext.contains(event) && !this.causalContext.contains(event) then 
        val op = event.content.get
        op.opType match 
          case Add =>
            newElements = newElements + (op.element -> (elements.getOrElse(op.element, Set.empty) + event.id))
          case Remove =>
            var ids = elements.getOrElse(op.element, Set.empty)
            for id <- ids do
              if newCausalContext.pathExists(id, event.id) then
                ids = ids - id
            newElements = newElements + (op.element -> ids)

    ORSet(newElements, newCausalContext)

  def getElements: Set[T] =
    elements.filter((k, v) => v.nonEmpty).keySet

  def produceNextCodedSymbols(count: Int = 1): List[CodedSymbol[String]] =
    this.causalContext.sendCodedSymbols(count)

  def addCodedSymbols(codedSymbols: List[CodedSymbol[String]]): ORSet[T] =
    this.copy(causalContext = this.causalContext.receiveCodedSymbols(codedSymbols))

  def sendSyncRequest: SyncRequest[Op[T]] =
    this.causalContext.sendSyncRequest

  def receiveSyncRequest(syncRequest: SyncRequest[Op[T]]): (ORSet[T], ORSet[T]) = {
    (
      this.merge(ORSet(new HashMap(), syncRequest.causalContext)),
      ORSet(new HashMap(),
        causalContext.empty.withQueue(syncRequest.requestedEvents.map(id => causalContext.events(id))))
    )
  }

object ORSet:
  def apply[T](): ORSet[T] =
    new ORSet[T](new HashMap(), HashDAG[Op[T]](Ed25519Util.generateNewKeyPair))

case class Op[T](
    element: T,
    opType: OpType
)

enum OpType:
  case Add, Remove
