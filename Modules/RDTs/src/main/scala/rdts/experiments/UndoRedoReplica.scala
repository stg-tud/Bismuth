package rdts.experiments

import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import rdts.base.Lattice
import rdts.base.Bottom

case class UndoRedoReplica[A](
    val deltas: Set[UndoRedoReplica.Delta[A]],
    val removed: Dots = Dots.empty,
    val nextLocalDot: Dot = Dot.zero,
    val undoStack: List[Dot] = List.empty,
    val redoStack: List[Dot] = List.empty,
    val buffer: UndoRedoReplica.Buffer[A] = UndoRedoReplica.Buffer[A](Set.empty, Dots.empty),
    val base: Option[A] = None,
) {
  def id: LocalUid = LocalUid(nextLocalDot.place)

  def clearBuffer(): UndoRedoReplica[A] = {
    this.copy(
      buffer = UndoRedoReplica.Buffer[A](Set.empty, Dots.empty)
    )
  }

  def receive(other: UndoRedoReplica.Buffer[A]): UndoRedoReplica[A] = {
    this.copy(
      deltas = deltas.union(other.deltas),
      removed = removed.union(other.removed)
    )
  }

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    val delta = f(using id)(state)
    val dot   = nextLocalDot

    val operationDelta = UndoRedoReplica.Delta(dot, delta)

    this.copy(
      buffer = buffer.add(operationDelta),
      nextLocalDot = nextLocalDot.advance,
      deltas = deltas + operationDelta,
      undoStack = dot :: undoStack,
      redoStack = List.empty
    )
  }

  def modUntracked(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    val delta = f(using id)(state)
    val dot   = nextLocalDot

    val operationDelta = UndoRedoReplica.Delta(dot, delta)
    this.copy(
      base = Some(base.map(b => Lattice.merge(b, delta)).getOrElse(delta)),
      nextLocalDot = nextLocalDot.advance,
      buffer = buffer.add(operationDelta)
    )
  }

  def canUndo = undoStack.nonEmpty

  def undo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    if undoStack.isEmpty then return this

    val lastDot = undoStack.head
    this.copy(
      undoStack = undoStack.tail,
      redoStack = lastDot :: redoStack,
      removed = removed.add(lastDot),
      buffer = buffer.remove(lastDot)
    )
  }

  def canRedo = redoStack.nonEmpty

  def redo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    if redoStack.isEmpty then return this

    val dot = redoStack.head

    deltas.find(_.dot == dot) match
      case Some(value) => {
        val dot   = nextLocalDot
        val delta = UndoRedoReplica.Delta(dot, value.delta)
        this.copy(
          deltas = deltas + delta,
          undoStack = dot :: undoStack,
          redoStack = redoStack.tail,
          nextLocalDot = nextLocalDot.advance,
          buffer = buffer.add(delta)
        )
      }
      case None =>
        throw new Exception(s"Redo stack contains unknown operation ${dot}")
  }

  def state(using Lattice[A])(using Bottom[A]) = {
    deltas
      .filterNot(d => removed.contains(d.dot))
      .map(_.delta)
      .foldLeft(base.getOrElse(Bottom.empty[A]))(Lattice.merge)
  }
}

object UndoRedoReplica {
  case class Buffer[A](var deltas: Set[UndoRedoReplica.Delta[A]], var removed: Dots) {
    def add(delta: Delta[A]): Buffer[A] = {
      this.copy(
        deltas = deltas + delta
      )
    }

    def remove(dot: Dot) = {
      this.copy(
        removed = removed.add(dot)
      )
    }
  }

  case class Delta[A](dot: Dot, delta: A)

  def empty[A](using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica[A](deltas = Set.empty[UndoRedoReplica.Delta[A]], nextLocalDot = Dot(LocalUid.replicaId, 0))

  def of[A](state: A)(using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica(
      deltas = Set(Delta(Dot(LocalUid.replicaId, 0), state)),
      nextLocalDot = Dot(LocalUid.replicaId, 1),
    )
}
