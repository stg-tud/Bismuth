package rdts.experiments

import rdts.time.Dot
import rdts.base.{Uid, Lattice}

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

case class UndoRedoOpBasedMVR[T](
    opId: Id,
    operations: Map[Id, Operation[T]],
    headIds: Set[Id],
    undoStack: List[Operation[T]],
    redoStack: List[Operation[T]]
) {
  def heads(): List[Id] = headIds.toList.sorted

  def values(): List[T] = {
    terminalHeads().sortBy(_._1).flatMap { case (_, operation) =>
      operation.ty.getValue
    }
  }

  private def terminalHeads(): List[(Id, Operation[T])] = {
    var todo: List[(Operation[T], List[Dot])] = headIds.toList.map { headId =>
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

  def set(value: T): (UndoRedoOpBasedMVR[T], Operation[T]) = {
    applyLocalOperation(OperationType.set(value))
  }

  def delete(): (UndoRedoOpBasedMVR[T], Operation[T]) = {
    applyLocalOperation(OperationType.delete)
  }

  def undo(): (UndoRedoOpBasedMVR[T], Option[Operation[T]]) = {
    if undoStack.isEmpty then return (this, None)

    val lastOp                = undoStack.head
    val (register, operation) = applyLocalOperation(OperationType.restore(lastOp.id))
    (
      register.copy(
        undoStack = undoStack.tail,
        redoStack = operation :: redoStack
      ),
      Some(operation)
    )
  }

  def redo(): (UndoRedoOpBasedMVR[T], Option[Operation[T]]) = {
    if redoStack.isEmpty then return (this, None)

    val lastOp = redoStack.head

    val lastAnchor = lastOp.ty match
      case OperationType.restore(anchor) => anchor
      case _ => throw new Exception(s"Redo stack contains non restore operation ${lastOp.id}")

    val (register, operation) = applyLocalOperation(OperationType.restore(lastOp.id))
    (
      register.copy(
        undoStack = operations(lastAnchor) :: undoStack,
        redoStack = redoStack.tail
      ),
      Some(operation)
    )
  }

  private def applyLocalOperation(operationType: OperationType[T]): (UndoRedoOpBasedMVR[T], Operation[T]) =
    val operation = Operation(opId, headIds, operationType)
    val register  = apply(operation).copy(opId = opId.advance)
    if operationType.isTerminal then {
      (
        register.copy(
          undoStack = operation :: undoStack,
          redoStack = List.empty
        ),
        operation
      )
    } else {
      (register, operation)
    }

  def applyRemoteOperation(operation: Operation[T]): UndoRedoOpBasedMVR[T] = {
    for pred <- operation.predecessors do
      if !operations.contains(pred) then
        throw new Exception(s"Missing predecessor $pred for operation ${operation.id}")

    apply(operation)
  }

  private def apply(operation: Operation[T]): UndoRedoOpBasedMVR[T] = {
    if operations.contains(operation.id) then return this

    val newOperations = operations + (operation.id -> operation)
    val newHeadIds    = (headIds -- operation.predecessors) + operation.id

    UndoRedoOpBasedMVR(
      opId = opId,
      operations = newOperations,
      headIds = newHeadIds,
      undoStack,
      redoStack
    )
  }
}

object UndoRedoOpBasedMVR {
  def forReplica[T](replicaId: Uid): UndoRedoOpBasedMVR[T] = UndoRedoOpBasedMVR(
    opId = Dot(replicaId, 0),
    operations = Map.empty,
    headIds = Set.empty,
    undoStack = List.empty,
    redoStack = List.empty
  )

  def createReplicas[T](n: Int): Array[UndoRedoOpBasedMVR[T]] = {
    Array.tabulate(n) { i =>
      UndoRedoOpBasedMVR.forReplica[T](Uid(s"R${i + 1}"))
    }
  }
}
