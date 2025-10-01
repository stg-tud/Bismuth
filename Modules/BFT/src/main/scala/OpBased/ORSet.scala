package OpBased

import OpBased.OpType.{Add, Remove}
import crypto.Ed25519Util
import dag.{Event, HashDAG}
import riblt.CodedSymbol

import scala.collection.immutable.HashMap

case class ORSet[T] private (
    hashDAG: HashDAG[Op[T]],
    elements: Map[T, Set[String]]
):

  def add(element: T): (ORSet[T], Event[Op[T]]) =
    val op = Op(element, OpType.Add)

    generate(op)

  def remove(element: T): (ORSet[T], Event[Op[T]]) =
    val op = Op(element, OpType.Remove)

    generate(op)

  private def generate(op: Op[T]): (ORSet[T], Event[Op[T]]) =
    val event = hashDAG.generator(op)

    val newElements = op.opType match
      case Add =>
        elements + (op.element -> (elements.getOrElse(op.element, Set.empty) + event.id))
      case Remove =>
        elements - op.element

    (
      ORSet(hashDAG.effector(event), newElements),
      event
    )

  def receiveEvent(event: Event[Op[T]]): ORSet[T] =
    val graph = hashDAG.effector(event)

    if graph.contains(event) then
      val op = event.content.get
      op.opType match
        case Add =>
          ORSet(graph, elements + (op.element -> (elements.getOrElse(op.element, Set.empty) + event.id)))
        case Remove =>
          var ids = elements.getOrElse(op.element, Set.empty)
          for id <- ids do
            if graph.pathExists(id, event.id) then
              ids = ids - id

          ORSet(graph, elements + (op.element -> ids))
    else
      this.copy(hashDAG = graph)

  def receiveEvents(events: Iterable[Event[Op[T]]]): ORSet[T] =
    var tmp = this
    for (event <- events)
      tmp = tmp.receiveEvent(event)

    tmp

  def getElements: Set[T] =
    Set.from(elements.filter((k, v) => v.nonEmpty).map((k, v) => k))

  def produceNextCodedSymbols(count: Int = 1): List[CodedSymbol[String]] =
    this.hashDAG.produceNextCodedSymbols(count)

  def addCodedSymbols(codedSymbols: List[CodedSymbol[String]]): (ORSet[T], Boolean) =
    val res = this.hashDAG.addCodedSymbols(codedSymbols)
    (
      this.copy(hashDAG = res._1),
      res._2
    )


  def sendDiff: (Set[Event[Op[T]]], Set[String]) =
    this.hashDAG.sendDiff

  def receiveDiff(response: Set[Event[Op[T]]], request: Set[String]): (ORSet[T], Set[Event[Op[T]]]) = {
    val res = hashDAG.receiveDiff(response, request)
    (
      receiveEvents(response),
      res._2
    )
  }

  def processQueue: ORSet[T] =
    var i = hashDAG.queue.size
    var j = 0
    var tmp = hashDAG
    var b = true
    while b do {
      tmp = hashDAG.processQueue()
      j = tmp.queue.size
      if i == j then
        b = false
      else
        i = j
    }

    var tmp2 = this.copy(hashDAG = tmp)
    val processedEvents  = this.hashDAG.queue -- tmp.queue
    for e <- processedEvents do
      tmp2 = tmp2.receiveEvent(e)

    tmp2



object ORSet:
  def apply[T](): ORSet[T] =
    new ORSet[T](HashDAG[Op[T]](Ed25519Util.generateNewKeyPair), new HashMap())

case class Op[T](
    element: T,
    opType: OpType
)

enum OpType:
  case Add, Remove
