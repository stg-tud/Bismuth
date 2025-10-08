package rdts.experiments

import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import rdts.base.Lattice
import rdts.base.Bottom

case class UndoRedoReplica[A](
    private var deltas: Set[UndoRedoReplica.ElementDelta[A]] = Set.empty,
    private var removed: Dots = Dots.empty,
    private var nextLocalDot: Dot = Dot.zero,
    private var undoStack: List[Dot] = List.empty,
    private var redoStack: List[Dot] = List.empty,
) {
  def id: LocalUid = LocalUid(nextLocalDot.place)

  def receive(other: UndoRedoReplica.Delta[A]) = {
    deltas = deltas.union(other.deltas)
    removed = removed.union(other.removed)
  }

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A]): UndoRedoReplica.Delta[A] = {
    val delta = f(using id)(state)
    val dot   = nextLocalDot

    val operationDelta = UndoRedoReplica.ElementDelta(dot, delta)
    deltas = deltas + operationDelta
    undoStack = dot :: undoStack
    redoStack = List.empty

    nextLocalDot = nextLocalDot.advance

    UndoRedoReplica.Delta(deltas = Set(operationDelta), removed = Dots.empty)
  }

  def undo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica.Delta[A] = {
    if undoStack.isEmpty then return UndoRedoReplica.Delta.empty

    val lastDot = undoStack.head
    undoStack = undoStack.tail
    redoStack = lastDot :: redoStack
    removed = removed.add(lastDot)

    UndoRedoReplica.Delta.removed(lastDot)
  }

  def redo()(using Lattice[A])(using Bottom[A]): UndoRedoReplica.Delta[A] = {
    if redoStack.isEmpty then return UndoRedoReplica.Delta.empty

    val dot = redoStack.head

    deltas.find(_.dot == dot) match
      case Some(value) => {
        val dot   = nextLocalDot
        val delta = UndoRedoReplica.ElementDelta(dot, value.delta)
        deltas = deltas + delta
        undoStack = dot :: undoStack
        redoStack = redoStack.tail
        nextLocalDot = nextLocalDot.advance
        UndoRedoReplica.Delta.added(delta)
      }
      case None =>
        throw new Exception(s"Redo stack contains unknown operation ${dot}")
  }

  def state(using Lattice[A])(using Bottom[A]) = {
    deltas
      .filterNot(d => removed.contains(d.dot))
      .map(_.delta)
      .foldLeft(Bottom.empty[A])(Lattice.merge)
  }
}

object UndoRedoReplica {
  case class ElementDelta[A](dot: Dot, delta: A)

  case class Delta[A](deltas: Set[ElementDelta[A]], removed: Dots)

  object Delta {
    def empty[A]: Delta[A]                         = Delta(deltas = Set.empty, removed = Dots.empty)
    def added[A](delta: ElementDelta[A]): Delta[A] = Delta(deltas = Set(delta), removed = Dots.empty)
    def removed[A](dot: Dot): Delta[A]             = Delta(deltas = Set.empty, removed = Dots.single(dot))
  }

  def empty[A](using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica[A](deltas = Set.empty[UndoRedoReplica.ElementDelta[A]], nextLocalDot = Dot(LocalUid.replicaId, 0))

  def of[A](state: A)(using LocalUid): UndoRedoReplica[A] =
    UndoRedoReplica(
      deltas = Set(ElementDelta(Dot(LocalUid.replicaId, 0), state)),
      nextLocalDot = Dot(LocalUid.replicaId, 1),
    )
}
