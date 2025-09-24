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
      dots: Dots,
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
        val counterComparison = x.time.compare(y.time)
        if counterComparison != 0 then counterComparison
        else x.place.delegate.compare(y.place.delegate)
      }
    }

    def set(value: T)(using LocalUid): Delta = {
      applyLocalOperation(OperationType.set(value))
    }

    def delete()(using LocalUid): Delta = {
      applyLocalOperation(OperationType.delete)
    }

    def undo()(using LocalUid): Delta = {
      if undoStack.isEmpty then return MVR.empty

      val lastOp = undoStack.read(0).get
      val opId   = dots.nextDot
      val delta  = applyLocalOperation(OperationType.restore(lastOp.id))
      delta.copy(
        undoStack = undoStack.delete(0),
        redoStack = redoStack.prepend(delta.operations(opId)),
      )
    }

    def redo()(using LocalUid): Delta = {
      if redoStack.isEmpty then return MVR.empty

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

    private def applyLocalOperation(operationType: OperationType[T])(using LocalUid): Delta =
      val operation = Operation(dots.nextDot, headIds.elements, operationType)

      if operations.contains(operation.id) then return this

      val newOperations  = operations + (operation.id -> operation)
      val removedHeadIds =
        headIds `merge` headIds.removeAll(operation.predecessors)
      val newHeadIds = removedHeadIds `merge` removedHeadIds.add(operation.id)

      val delta = this.copy(
        dots = dots.advanced(LocalUid.replicaId),
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
    given bottom[T]: Bottom[MVR[T]] = Bottom.derived
    def empty[T]: MVR[T]            = bottom.empty

    given lattice[T]: Lattice[MVR[T]] =
      given Lattice[Operation[T]] = Lattice.assertEquals
      Lattice.derived[MVR[T]]
  }
}
