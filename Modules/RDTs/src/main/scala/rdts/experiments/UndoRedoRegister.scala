package rdts.experiments

import rdts.time.Dot
import rdts.base.{Uid, Lattice}

type Id = Dot

enum OperationType[+T]:
  case set(value: T)
  case restore(anchor: Id)
  case delete

  def is_terminal(): Boolean = this match
    case set(_)     => true
    case restore(_) => false
    case delete     => true

  def get_anchor(): Option[Id] = this match
    case restore(anchor) => Some(anchor)
    case set(_)          => None
    case delete          => None

  def get_value(): Option[T] = this match
    case set(value) => Some(value)
    case restore(_) => None
    case delete     => None

case class Operation[T](id: Id, predecessors: Set[Dot], ty: OperationType[T])

case class UndoRedoRegister[T](
    op_id: Id,
    operations: Map[Id, Operation[T]],
    head_ids: Set[Id],
    undo_stack: List[Operation[T]],
    redo_stack: List[Operation[T]]
) {
  def heads(): List[Id] = head_ids.toList.sorted

  def values(): List[T] = {
    terminal_heads().sortBy(_._1).flatMap { case (_, operation) =>
      operation.ty.get_value()
    }
  }

  private def terminal_heads(): List[(Id, Operation[T])] = {
    var todo: List[(Operation[T], List[Dot])] = head_ids.toList.map { head_id =>
      val operation = operations(head_id)
      (operation, List.empty[Dot])
    }

    var term_heads = List.empty[(Id, Operation[T])]

    while todo.nonEmpty do {
      val (next_op, op_id_trace) = todo.head
      todo = todo.tail
      val new_op_id_trace = op_id_trace :+ next_op.id

      if next_op.ty.is_terminal() then {
        term_heads = term_heads :+ (next_op.id, next_op)
      } else {
        val anchor           = next_op.ty.get_anchor().get
        val anchor_operation = operations(anchor)
        for predecessor <- anchor_operation.predecessors do {
          val predecessor_operation = operations(predecessor)
          todo = (predecessor_operation, new_op_id_trace) :: todo
        }
      }
    }

    term_heads
  }

  given dotOrdering: Ordering[Dot] {
    def compare(x: Dot, y: Dot): Int = {
      val counterComparison = x.time.compare(y.time)
      if counterComparison != 0 then counterComparison
      else x.place.delegate.compare(y.place.delegate)
    }
  }

  def set(value: T): (UndoRedoRegister[T], Operation[T]) = {
    apply_local_operation(OperationType.set(value))
  }

  def delete(): (UndoRedoRegister[T], Operation[T]) = {
    apply_local_operation(OperationType.delete)
  }

  def undo(): (UndoRedoRegister[T], Option[Operation[T]]) = {
    if undo_stack.isEmpty then return (this, None)

    val last_op               = undo_stack.head
    val (register, operation) = apply_local_operation(OperationType.restore(last_op.id))
    (
      register.copy(
        undo_stack = undo_stack.tail,
        redo_stack = operation :: redo_stack
      ),
      Some(operation)
    )
  }

  def redo(): (UndoRedoRegister[T], Option[Operation[T]]) = {
    if redo_stack.isEmpty then return (this, None)

    val last_op = redo_stack.head

    val last_anchor = last_op.ty match
      case OperationType.restore(anchor) => anchor
      case _ => throw new Exception(s"Redo stack contains non restore operation ${last_op.id}")

    val (register, operation) = apply_local_operation(OperationType.restore(last_op.id))
    (
      register.copy(
        undo_stack = operations(last_anchor) :: undo_stack,
        redo_stack = redo_stack.tail
      ),
      Some(operation)
    )
  }

  private def apply_local_operation(operation_ty: OperationType[T]): (UndoRedoRegister[T], Operation[T]) =
    val operation = Operation(op_id, head_ids, operation_ty)
    val register  = apply(operation).copy(op_id = op_id.advance)
    if operation_ty.is_terminal() then {
      (
        register.copy(
          undo_stack = operation :: undo_stack,
          redo_stack = List.empty
        ),
        operation
      )
    } else {
      (register, operation)
    }

  def apply_remote_operation(operation: Operation[T]): UndoRedoRegister[T] = {
    for pred <- operation.predecessors do
      if !operations.contains(pred) then
        throw new Exception(s"Missing predecessor $pred for operation ${operation.id}")

    apply(operation)
  }

  private def apply(operation: Operation[T]): UndoRedoRegister[T] = {
    if operations.contains(operation.id) then return this

    val new_operations = operations + (operation.id -> operation)
    val new_head_ids   = (head_ids -- operation.predecessors) + operation.id

    UndoRedoRegister(
      op_id = op_id,
      operations = new_operations,
      head_ids = new_head_ids,
      undo_stack,
      redo_stack
    )
  }
}

object UndoRedoRegister {
  def for_replica[T](replica_id: Uid): UndoRedoRegister[T] = UndoRedoRegister(
    op_id = Dot(replica_id, 0),
    operations = Map.empty,
    head_ids = Set.empty,
    undo_stack = List.empty,
    redo_stack = List.empty
  )

  def test[T](n: Int): Array[UndoRedoRegister[T]] = {
    Array.tabulate(n) { i =>
      UndoRedoRegister.for_replica[T](Uid(s"R${i + 1}"))
    }
  }
}
