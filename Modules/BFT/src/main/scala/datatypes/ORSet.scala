package datatypes

import dag.{Event, HashDAG, KeyPair}

case class ORSet[T] private(
                             hashDAG: HashDAG[Op[T]]
                           ):

  lazy val elements: Set[T] = getElements(hashDAG.graph.keys.find(event => event.id == "0").get)

  def add(element: T): (ORSet[T], Event[Op[T]]) =
    val op = Op(element, OpType.Add)

    generate(op)

  def remove(element: T): (ORSet[T], Event[Op[T]]) =
    val op = Op(element, OpType.Remove)

    generate(op)

  private def generate(op: Op[T]): (ORSet[T], Event[Op[T]]) =
    val event = hashDAG.generator(op)

    (ORSet(hashDAG.effector(event)), event)

  def receiveEvent(event: Event[Op[T]]): ORSet[T] =
    ORSet(hashDAG.effector(event))

  private def getElements(event: Event[Op[T]]): Set[T] =
    var result = Set.empty[T]

    if event.id != "0" then
      event.content.get.opType match
        case OpType.Add => result = result + event.content.get.element
        case OpType.Remove =>
          if (removeOpHappenedAfterAddOp(event, event.content.get.element)) {
            result = result - event.content.get.element
          }

    for child <- hashDAG.graph(event) do
      result = result ++ getElements(child)

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


object ORSet:
  def apply[T](): ORSet[T] =
    val authorKeyPair = KeyPair(Array.empty, Array.empty)
    new ORSet[T](HashDAG[Op[T]](authorKeyPair))



case class Op[T](
    element: T,
    opType: OpType
             )

enum OpType:
  case Add, Remove

