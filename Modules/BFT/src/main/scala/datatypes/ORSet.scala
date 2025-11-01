package datatypes

import datatypes.OpType.{Add, Remove}
import crypto.Ed25519Util
import dag.{Event, HashDAG}
import riblt.{CodedSymbol, RIBLT}
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}
import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}

case class ORSet[T](
    elements: Map[T, Set[String]],
    hashDAG: HashDAG[Op[T]]
) extends Replica[Op[T], ORSet[T]]:

  def add(element: T): ORSet[T] =
    val op = Op(element, OpType.Add)

    ORSet(Map.empty, hashDAG.generateDelta(op))

  def remove(element: T): ORSet[T] =
    val op = Op(element, OpType.Remove)

    ORSet(Map.empty, hashDAG.generateDelta(op))

  override def merge(other: ORSet[T]): ORSet[T] =
    var newElements = this.elements
    val newHashDAG  = this.hashDAG.merge(other.hashDAG)

    for event <- this.hashDAG.queue ++ other.hashDAG.events.values ++ other.hashDAG.queue do
      if newHashDAG.contains(event) && !this.hashDAG.contains(event) then
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

    ORSet(newElements, newHashDAG)

  def getElements: Set[T] =
    elements.filter((k, v) => v.nonEmpty).keySet

  def empty: ORSet[T] = ORSet()

  def withHashDAG(hashDAG: HashDAG[Op[T]]): ORSet[T] = this.copy(hashDAG = hashDAG)

  override def generateDelta(ids: List[String]): ORSet[T] =
    ORSet(Map.empty, hashDAG.getDelta(ids))

object ORSet:
  def apply[T](): ORSet[T] = {
    val keyPair = Ed25519Util.generateNewKeyPair
    new ORSet[T](new HashMap(), HashDAG[Op[T]](keyPair.getPublic, Some(keyPair.getPrivate)))
  }

case class Op[T](
    element: T,
    opType: OpType
)

enum OpType:
  case Add, Remove
