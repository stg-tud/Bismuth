package rdts.experiments

import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import rdts.base.Lattice
import rdts.base.Bottom

case class DeltaHistory[A](val deltas: Map[Dot, A], val removed: Dots) {
  lazy val dots: Dots = Dots.from(deltas.map({ case (dot, _) => dot })).union(removed)

  def add(dot: Dot, delta: A): DeltaHistory[A] =
    DeltaHistory(deltas = Map(dot -> delta), removed = Dots.empty)

  def remove(dot: Dot): DeltaHistory[A] =
    DeltaHistory(deltas = Map.empty, removed = Dots.single(dot))

  def state(using Lattice[A])(using Bottom[A]): A =
    deltas
      .filter({ case (dot, _) => !removed.contains(dot) })
      .map({ case (_, delta) => delta })
      .foldLeft(Bottom.empty[A])(Lattice.merge)
}

object DeltaHistory {
  def empty[A]: DeltaHistory[A] = DeltaHistory(Map.empty, Dots.empty)

  given lattice[A]: Lattice[DeltaHistory[A]] = new Lattice[DeltaHistory[A]] {
    override def merge(l: DeltaHistory[A], r: DeltaHistory[A]): DeltaHistory[A] =
      val removed = l.removed.union(r.removed)
      val deltas  = (l.deltas ++ r.deltas)
        .filter({ case (dot, _) => !removed.contains(dot) })
      DeltaHistory(deltas, removed)
  }
}

import scala.collection.mutable.Stack

case class Replica[A](
    var history: DeltaHistory[A],
    var undoStack: Stack[Dot] = Stack.empty,
    var redoStack: Stack[A] = Stack.empty,
) {
  def state(using Lattice[A])(using Bottom[A]): A = history.state

  def receive(delta: DeltaHistory[A])(using Lattice[A]) =
    this.history = Lattice.merge(this.history, delta)

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A])(using LocalUid): DeltaHistory[A] =
    val dot          = history.dots.nextDot
    val delta        = f(state)
    val historyDelta = this.history.add(dot, delta)
    this.history = Lattice.merge(this.history, historyDelta)
    this.undoStack.push(dot)
    this.redoStack.clear()
    historyDelta

  def undo(): DeltaHistory[A] =
    if undoStack.isEmpty then return DeltaHistory.empty[A]
    val dotToUndo    = undoStack.pop()
    val deltaToUndo  = this.history.deltas(dotToUndo)
    val historyDelta = this.history.remove(dotToUndo)
    this.history = Lattice.merge(this.history, historyDelta)
    this.redoStack.push(deltaToUndo)
    historyDelta

  def redo()(using LocalUid): DeltaHistory[A] =
    if redoStack.isEmpty then return DeltaHistory.empty[A]
    val deltaToRedo  = redoStack.pop()
    val dot          = this.history.dots.nextDot
    val historyDelta = this.history.add(dot, deltaToRedo)
    this.history = Lattice.merge(this.history, historyDelta)
    this.undoStack.push(dot)
    historyDelta
}

object Replica {
  def empty[A]: Replica[A] =
    Replica(DeltaHistory.empty[A], Stack.empty, Stack.empty)
}

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
