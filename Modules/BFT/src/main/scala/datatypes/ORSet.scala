package datatypes

import scala.collection.immutable.HashMap
import dag.{Event, HashDAG}
import crypto.Ed25519Util
import datatypes.OpType.{Add, Remove}

case class ORSet[T] private(
                             hashDAG: HashDAG[Op[T]],
                             elements: Map[T, String]
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
        elements + (op.element -> event.id)
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
          ORSet(graph, elements + (op.element -> event.id))
        case Remove =>
          if !elements.contains(op.element)then
            ORSet(graph, elements)
          else {
            val lastAddEvent = hashDAG.getEventByID(elements(op.element))
            if hashDAG.pathExists(lastAddEvent, event) then
              ORSet(graph, elements - op.element)
            else
              ORSet(graph, elements)

          }
          else
      this


  private def getElements(event: Event[Op[T]], acc: Set[T]): Set[T] =
    var result = acc

    if event.id != "0" then
      event.content.get.opType match
        case OpType.Add =>
          for child <- hashDAG.graph(event) do
            result = result ++ getElements(child, result)
        case OpType.Remove =>
          for child <- hashDAG.graph(event) do
            result = result ++ getElements(child, result)

    result

  private def removeOpHappenedAfterAddOp(event: Event[Op[T]], element: T): Boolean =
    val parents = getParents(event)

    var res = false
    for parent <- parents do
      if parent.id != "0" then
        if parent.content.get.opType == OpType.Add && parent.content.get.element == element then
          res = true
        else
          var result = Set.empty[Boolean]
          for parent <- parents do
            result = result + removeOpHappenedAfterAddOp(parent, element)

          res = result.contains(true)

    res

  private def getParents(event: Event[Op[T]]): Set[Event[Op[T]]] =
    var result = Set.empty[Event[Op[T]]]

    hashDAG.graph.foreach((key, value) =>
      if (value.contains(event)) {
        result = result + key
      }
    )

    result
    
  def getElements: Set[T] =
    Set.from(elements.keys)


object ORSet:
  def apply[T](): ORSet[T] =
    new ORSet[T](HashDAG[Op[T]](Ed25519Util.generateNewKeyPair), new HashMap())



case class Op[T](
                  element: T,
                  opType: OpType
                )

enum OpType:
  case Add, Remove

