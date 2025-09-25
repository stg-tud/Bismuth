package rdts.experiments

import rdts.time.{Dot, Dots}
import rdts.base.{Uid, Lattice}
import rdts.base.LocalUid
import rdts.base.Bottom
import rdts.datatypes.ReplicatedSet
import rdts.datatypes.ReplicatedList

object UndoRedoDeltaBased {
  type Id = Dot

  enum OperationType[+T]:
    case set(value: T)
    case restore(anchor: Id)
    case delete

    def isTerminal: Boolean = this match
      case set(_)     => true
      case restore(_) => false
      case delete     => true

    def getAnchor: Option[Id] = this match
      case restore(anchor) => Some(anchor)
      case set(_)          => None
      case delete          => None

    def getValue: Option[T] = this match
      case set(value) => Some(value)
      case restore(_) => None
      case delete     => None

  case class Operation[T](id: Id, predecessors: Set[Dot], ty: OperationType[T])

  case class MVR[T](
      dot: Dot,
      operations: Map[Id, Operation[T]],
      headIds: ReplicatedSet[Id],
      undoStack: ReplicatedList[Operation[T]],
      redoStack: ReplicatedList[Operation[T]],
  ) {
    type Delta = MVR[T]

    def heads(): List[Id] = headIds.elements.toList.sorted

    def operation(id: Id): Option[Operation[T]] = operations.get(id)

    def values(): List[T] = {
      terminalHeads().sortBy(_._1).flatMap { case (_, operation) =>
        operation.ty.getValue
      }
    }

    private def terminalHeads(): List[(Id, Operation[T])] = {
      var todo: List[(Operation[T], List[Dot])] = headIds.elements.toList.map { headId =>
        val operation = operations(headId)
        (operation, List.empty[Dot])
      }

      var termHeads = List.empty[(Id, Operation[T])]

      while todo.nonEmpty do {
        val (nextOp, opIdTrace) = todo.head
        todo = todo.tail
        val newOpIdTrace = opIdTrace :+ nextOp.id

        if nextOp.ty.isTerminal then {
          termHeads = termHeads :+ (nextOp.id, nextOp)
        } else {
          val anchor          = nextOp.ty.getAnchor.get
          val anchorOperation = operations(anchor)
          for predecessor <- anchorOperation.predecessors do {
            val predecessorOperation = operations(predecessor)
            todo = (predecessorOperation, newOpIdTrace) :: todo
          }
        }
      }

      termHeads
    }

    given dotOrdering: Ordering[Dot] {
      def compare(x: Dot, y: Dot): Int = {
        val timeComparison = x.time.compare(y.time)
        if timeComparison != 0 then timeComparison
        else x.place.delegate.compare(y.place.delegate)
      }
    }

    def set(value: T): Delta = {
      applyLocalOperation(OperationType.set(value))
    }

    def delete(): Delta = {
      applyLocalOperation(OperationType.delete)
    }

    def undo(): Delta = {
      given LocalUid = LocalUid(dot.place)

      if undoStack.isEmpty then return MVR.forReplica(dot.place)

      val lastOp = undoStack.read(0).get
      val opId   = dot
      val delta  = applyLocalOperation(OperationType.restore(lastOp.id))
      delta.copy(
        undoStack = undoStack.delete(0),
        redoStack = redoStack.prepend(delta.operations(opId)),
      )
    }

    def redo(): Delta = {
      given LocalUid = LocalUid(dot.place)

      if redoStack.isEmpty then return return MVR.forReplica(dot.place)

      val lastOp = redoStack.read(0).get

      val lastAnchor = lastOp.ty match
        case OperationType.restore(anchor) => anchor
        case _ => throw new Exception(s"Redo stack contains non restore operation ${lastOp.id}")

      applyLocalOperation(OperationType.restore(lastOp.id))
        .copy(
          undoStack = undoStack.prepend(operations(lastAnchor)),
          redoStack = redoStack.delete(0),
        )
    }

    private def applyLocalOperation(operationType: OperationType[T]): Delta =
      given LocalUid = LocalUid(dot.place)

      if operations.contains(dot) then return this

      val operation = Operation(dot, headIds.elements, operationType)

      val newOperations = operations + (operation.id -> operation)

      val newRemovedHeadIds = headIds `merge` headIds.removeAll(operation.predecessors)
      val newHeadIds        = newRemovedHeadIds `merge` newRemovedHeadIds.add(operation.id)

      val delta = this.copy(
        dot = dot.advance,
        operations = newOperations,
        headIds = newHeadIds,
      )
      if operationType.isTerminal then {
        delta.copy(
          undoStack = undoStack.prepend(operation),
          redoStack = redoStack.clear(),
        )
      } else {
        delta
      }
  }

  object MVR {
    def forReplica[T](id: Uid): MVR[T] = MVR(
      dot = Dot(id, 0),
      operations = Map.empty,
      headIds = ReplicatedSet.empty,
      undoStack = ReplicatedList.empty,
      redoStack = ReplicatedList.empty,
    )

    given lattice[T]: Lattice[MVR[T]] = new Lattice[MVR[T]] {
      def merge(left: MVR[T], right: MVR[T]): MVR[T] = {
        val dot = if left.dot.place == right.dot.place then
          Dot(left.dot.place, scala.math.max(left.dot.time, right.dot.time))
        else left.dot
        val headIds    = left.headIds.merge(right.headIds)
        val operations = left.operations ++ right.operations

        val (undoStack, redoStack) = if left.dot.place == right.dot.place then
          (left.undoStack `merge` right.undoStack, left.redoStack `merge` right.redoStack)
        else
          (left.undoStack, left.redoStack)

        MVR(
          dot,
          operations,
          headIds,
          undoStack,
          redoStack
        )
      }
    }
  }
}
