package rdts.experiments

import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import rdts.base.Lattice
import rdts.base.Bottom

case class UndoRedoReplica[A](
    private var deltas: Set[UndoRedoReplica.Delta[A]] = Set.empty,
    private var removed: Dots = Dots.empty,
    private var nextLocalDot: Dot = Dot.zero,
    private var undoStack: List[Dot] = List.empty,
    private var redoStack: List[Dot] = List.empty,
) {
  def id: LocalUid = LocalUid(nextLocalDot.place)

  def receive(other: UndoRedoReplica[A]) = {
    deltas = deltas.union(other.deltas)
    removed = removed `merge` other.removed
  }

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    given LocalUid = id
    applyLocal(f(state))
    UndoRedoReplica(deltas = deltas, removed = removed)
  }

  def undo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    given LocalUid = id

    if undoStack.isEmpty then return UndoRedoReplica.empty[A]

    val lastDot = undoStack.head
    undoStack = undoStack.tail
    redoStack = lastDot :: redoStack
    removed = removed.add(lastDot)

    UndoRedoReplica(deltas = deltas, removed = removed)
  }

  def redo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica[A] = {
    given LocalUid = id

    if redoStack.isEmpty then return UndoRedoReplica.empty[A]

    val dot = redoStack.head

    deltas.find(_.dot == dot) match
      case Some(value) => {
        val dot = nextLocalDot
        deltas = deltas + UndoRedoReplica.Delta(dot, value.delta)
        undoStack = dot :: undoStack
        redoStack = redoStack.tail
        nextLocalDot = nextLocalDot.advance
      }
      case None =>
        throw new Exception(s"Redo stack contains unknown operation ${dot}")

    UndoRedoReplica(deltas = deltas, removed = removed)
  }

  def state(using Lattice[A])(using Bottom[A]) = {
    deltas
      .filterNot(d => removed.contains(d.dot))
      .map(_.delta)
      .foldLeft(Bottom.empty[A])(Lattice.merge)
  }

  private def applyLocal(delta: A) = {
    val dot = nextLocalDot

    deltas = deltas + UndoRedoReplica.Delta(dot, delta)
    undoStack = dot :: undoStack
    redoStack = List.empty

    nextLocalDot = nextLocalDot.advance
  }
}

object UndoRedoReplica {
  case class Delta[A](dot: Dot, delta: A)

  def empty[A](using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica[A](deltas = Set.empty[UndoRedoReplica.Delta[A]], nextLocalDot = Dot(LocalUid.replicaId, 0))

  def of[A](state: A)(using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica(
      deltas = Set(Delta(Dot(LocalUid.replicaId, 0), state)),
      nextLocalDot = Dot(LocalUid.replicaId, 1),
    )
}
