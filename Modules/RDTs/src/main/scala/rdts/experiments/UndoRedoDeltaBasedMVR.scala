package rdts.experiments

import rdts.time.{Dot, Dots}
import rdts.base.{Uid, Lattice}
import rdts.base.LocalUid
import rdts.base.Bottom
import rdts.datatypes.ReplicatedSet
import rdts.datatypes.ReplicatedList

object UndoRedoDeltaBased {
  case class Replica[T](
      val id: Uid,
      var state: MVR[T],
      private var undoStack: List[Operation[T]],
      private var redoStack: List[Operation[T]],
  ) {
    def set(value: T): MVR[T] = {
      val delta   = state.set(value)(using LocalUid(id))
      val (_, op) = delta.operations.head
      undoStack = op :: undoStack
      redoStack = List.empty
      apply(delta)
    }

    def delete(): MVR[T] = {
      val delta   = state.delete()(using LocalUid(id))
      val (_, op) = delta.operations.head
      undoStack = op :: undoStack
      redoStack = List.empty
      apply(delta)
    }

    def undo(): MVR[T] = {
      if undoStack.isEmpty then return MVR.empty[T]

      val lastOp  = undoStack.head
      val delta   = state.restore(lastOp.id)(using LocalUid(id))
      val (_, op) = delta.operations.head
      undoStack = undoStack.tail
      redoStack = op :: redoStack
      apply(delta)
    }

    def redo(): MVR[T] = {
      if redoStack.isEmpty then return MVR.empty[T]

      val lastOp = redoStack.head

      val lastAnchor = lastOp.ty match
        case OperationType.restore(anchor) => anchor
        case _ => throw new Exception(s"Redo stack contains non restore operation ${lastOp.id}")

      val delta = state.restore(lastOp.id)(using LocalUid(id))
      undoStack = state.operations(lastAnchor) :: undoStack
      redoStack = redoStack.tail
      apply(delta)
    }

    def receive(other: MVR[T]): Replica[T] = {
      apply(other)
      this
    }

    def undoValues: List[T] = {
      undoStack
        .flatMap(_.ty.getValue)
        .reverse
    }

    def redoAnchors: List[Id] = {
      redoStack
        .flatMap(_.ty.getAnchor)
        .reverse
    }

    private def apply(delta: MVR[T]): MVR[T] = {
      state = state `merge` delta
      this.state
    }
  }

  object Replica {
    def empty[T](using LocalUid): Replica[T] = Replica(LocalUid.replicaId, MVR.empty, List.empty, List.empty)
  }

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
      operations: Map[Id, Operation[T]],
      headIds: ReplicatedSet[Id],
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

    def set(value: T)(using LocalUid): Delta = {
      applyLocalOperation(OperationType.set(value))
    }

    def delete()(using LocalUid): Delta = {
      applyLocalOperation(OperationType.delete)
    }

    def restore(node: Id)(using LocalUid): Delta = {
      if !operations.contains(node) then
        throw new Exception(s"Cannot restore non existing operation $node")

      applyLocalOperation(OperationType.restore(node))
    }

    private def applyLocalOperation(operationType: OperationType[T])(using LocalUid): Delta =
      val dots = Dots.from(operations.keys)
      val dot  = dots.nextDot

      if operations.contains(dot) then return this

      val operation = Operation(dot, headIds.elements, operationType)

      val newOperations = Map((operation.id -> operation))

      val newRemovedHeadIds = headIds `merge` headIds.removeAll(operation.predecessors)
      val newHeadIds        = newRemovedHeadIds `merge` newRemovedHeadIds.add(operation.id)

      this.copy(
        operations = newOperations,
        headIds = newHeadIds,
      )
  }

  object MVR {
    def empty[T]: MVR[T] = MVR(
      operations = Map.empty,
      headIds = ReplicatedSet.empty,
    )

    def of[T](value: T)(using LocalUid): MVR[T] = empty[T].set(value)

    given lattice[T]: Lattice[MVR[T]] = new Lattice[MVR[T]] {
      def merge(left: MVR[T], right: MVR[T]): MVR[T] = {
        val headIds    = left.headIds.merge(right.headIds)
        val operations = left.operations ++ right.operations
        MVR(
          operations,
          headIds,
        )
      }
    }
  }
}
