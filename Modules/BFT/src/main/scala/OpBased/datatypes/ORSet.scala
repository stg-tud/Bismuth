package OpBased.datatypes

import OpBased.dag.{Event, HashDAG}
import scala.collection.immutable.HashMap
import crypto.Ed25519Util
import OpType.{Add, Remove}

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
      this

  def getElements: Set[T] =
    Set.from(elements.filter((k, v) => v.nonEmpty).map((k, v) => k))

object ORSet:
  def apply[T](): ORSet[T] =
    new ORSet[T](HashDAG[Op[T]](Ed25519Util.generateNewKeyPair), new HashMap())

case class Op[T](
    element: T,
    opType: OpType
)

enum OpType:
  case Add, Remove
