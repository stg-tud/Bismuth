package datatypes

import crypto.Ed25519Util
import dag.HashDAG
import datatypes.OpType.{Add, Remove}
import riblt.RIBLT
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}

import scala.collection.immutable.HashMap

case class ORSet[T] private (
    hashDAG: HashDAG[Op[T]],
    riblt: RIBLT[String],
    elements: Map[T, Set[String]],
) extends Replica[Op[T], ORSet[T]]:

  def add(element: T): ORSet[T] =
    val op = Op(element, OpType.Add)

    ORSet(hashDAG.generateDelta(op), RIBLT.empty, Map.empty)

  def remove(element: T): ORSet[T] =
    val op = Op(element, OpType.Remove)

    ORSet(hashDAG.generateDelta(op), RIBLT.empty, Map.empty)

  override def merge(other: ORSet[T]): ORSet[T] =
    var newElements = this.elements
    val newHashDAG  = this.hashDAG.merge(other.hashDAG)

    for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
        this.riblt.addSymbol(event.id)
        val op = event.content.get
        op.opType match
          case Add =>
            newElements = newElements + (op.element -> (elements.getOrElse(op.element, Set.empty) + event.id))
          case Remove =>
            var ids = elements.getOrElse(op.element, Set.empty)
            for id <- ids do
              if newHashDAG.pathExists(id, event.id) then
                ids = ids - id
            newElements = newElements + (op.element -> ids)

    ORSet(newHashDAG, this.riblt, newElements)

  def getElements: Set[T] =
    elements.filter((k, v) => v.nonEmpty).keySet

  override def empty: ORSet[T] = ORSet()

  override def withHashDAG(hashDAG: HashDAG[Op[T]]): ORSet[T] = this.copy(hashDAG = hashDAG)

object ORSet:
  def apply[T](): ORSet[T] =
    new ORSet[T](HashDAG[Op[T]](Ed25519Util.generateNewKeyPair), RIBLT.empty, new HashMap())

case class Op[T](
    element: T,
    opType: OpType
)

enum OpType:
  case Add, Remove
