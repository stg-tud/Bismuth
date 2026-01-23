package rdts.experiments

import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import rdts.base.Lattice
import rdts.base.Bottom

case class DeltaHistory[A](val deltas: Map[Dot, A], val removed: Dots, val dots: Dots) {

  def add(dot: Dot, delta: A): DeltaHistory[A] =
    DeltaHistory(deltas = Map(dot -> delta), removed = Dots.empty, dots = Dots.single(dot))

  def remove(dot: Dot): DeltaHistory[A] =
    DeltaHistory(deltas = Map.empty, removed = Dots.single(dot), dots = Dots.single(dot))

  def state(using Lattice[A])(using Bottom[A]): A =
    deltas
      .filter { case (dot, _) => !removed.contains(dot) }
      .map { case (_, delta) => delta }
      .foldLeft(Bottom.empty[A])(Lattice.merge)
}

object DeltaHistory {
  def empty[A]: DeltaHistory[A] = DeltaHistory(Map.empty, Dots.empty, Dots.empty)

  given lattice[A]: Lattice[DeltaHistory[A]] = new Lattice[DeltaHistory[A]] {
    override def merge(l: DeltaHistory[A], r: DeltaHistory[A]): DeltaHistory[A] =
        val removed = l.removed.union(r.removed)

        val lFiltered = if r.removed.isEmpty then l.deltas
        else l.deltas.filter { case (dot, _) => !r.removed.contains(dot) }
        val rFiltered = if l.removed.isEmpty then r.deltas
        else r.deltas.filter { case (dot, _) => !l.removed.contains(dot) }

        val deltas = lFiltered ++ rFiltered
        val dots   = l.dots.union(r.dots)
        DeltaHistory(deltas, removed, dots)
  }
}

import scala.collection.mutable.Stack

// Timing stats for debugging
object ReplicaTimings {
  var getStateNanos: Long     = 0
  var applyFNanos: Long       = 0
  var getDotsNanos: Long      = 0
  var addDeltaNanos: Long     = 0
  var mergeHistoryNanos: Long = 0
  var updateCacheNanos: Long  = 0
  var callCount: Long         = 0

  def reset(): Unit = {
    getStateNanos = 0
    applyFNanos = 0
    getDotsNanos = 0
    addDeltaNanos = 0
    mergeHistoryNanos = 0
    updateCacheNanos = 0
    callCount = 0
  }

  def report(): String = {
    if callCount == 0 then return "No calls"
    f"""Replica.mod breakdown (${callCount} calls):
       |  getState:     ${getStateNanos / 1_000_000.0 / callCount}%.4fms avg
       |  applyF:       ${applyFNanos / 1_000_000.0 / callCount}%.4fms avg
       |  getDots:      ${getDotsNanos / 1_000_000.0 / callCount}%.4fms avg
       |  addDelta:     ${addDeltaNanos / 1_000_000.0 / callCount}%.4fms avg
       |  mergeHistory: ${mergeHistoryNanos / 1_000_000.0 / callCount}%.4fms avg
       |  updateCache:  ${updateCacheNanos / 1_000_000.0 / callCount}%.4fms avg""".stripMargin
  }
}

case class Replica[A: Lattice](
    var history: DeltaHistory[A],
    var cached: Option[A] = None,
    var undoStack: Stack[Dot] = Stack.empty,
    var redoStack: Stack[A] = Stack.empty,
) {
  def state(using Lattice[A])(using Bottom[A]): A =
    if cached.isDefined then
        cached.get
    else
        cached = Some(history.state)
        cached.get

  def receive(delta: DeltaHistory[A]) =
      if delta.removed.isEmpty && cached.isDefined then {
        for delta <- delta.deltas.values do
            cached = Some(Lattice.merge(cached.get, delta))
      } else {
        cached = None
      }

      this.history = Lattice.merge(this.history, delta)

  def mod(f: LocalUid ?=> A => A)(using Lattice[A])(using Bottom[A])(using LocalUid): DeltaHistory[A] =
      ReplicaTimings.callCount += 1

      var t0           = System.nanoTime()
      val currentState = state
      ReplicaTimings.getStateNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      val delta = f(currentState)
      ReplicaTimings.applyFNanos += System.nanoTime() - t0

      val (dot, historyDelta) = this.pushDelta(delta)
      this.undoStack.push(dot)
      this.redoStack.clear()
      historyDelta

  def undo(): DeltaHistory[A] =
      if undoStack.isEmpty then return DeltaHistory.empty[A]
      val dotToUndo    = undoStack.pop()
      val deltaToUndo  = this.history.deltas(dotToUndo)
      val historyDelta = this.history.remove(dotToUndo)
      this.history = Lattice.merge(this.history, historyDelta)
      this.cached = None
      this.redoStack.push(deltaToUndo)
      historyDelta

  def redo()(using LocalUid)(using Bottom[A]): DeltaHistory[A] =
      if redoStack.isEmpty then return DeltaHistory.empty[A]
      val deltaToRedo         = redoStack.pop()
      val (dot, historyDelta) = this.pushDelta(deltaToRedo)
      this.undoStack.push(dot)
      historyDelta

  private def pushDelta(delta: A)(using LocalUid)(using Bottom[A]): (Dot, DeltaHistory[A]) =
      var t0  = System.nanoTime()
      val dot = this.history.dots.nextDot
      ReplicaTimings.getDotsNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      val historyDelta = this.history.add(dot, delta)
      ReplicaTimings.addDeltaNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      this.history = Lattice.merge(this.history, historyDelta)
      ReplicaTimings.mergeHistoryNanos += System.nanoTime() - t0

      t0 = System.nanoTime()
      cached = if cached.isDefined then
          Some(Lattice.merge(cached.get, delta))
      else
          Some(this.history.state)
      ReplicaTimings.updateCacheNanos += System.nanoTime() - t0

      (dot, historyDelta)
}

object Replica {
  def empty[A: Lattice]: Replica[A] =
    Replica(DeltaHistory.empty[A], None, Stack.empty, Stack.empty)
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

  def undo(): UndoRedoReplica[A] = {
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

  def redo(): UndoRedoReplica[A] = {
    if redoStack.isEmpty then return this

    val dot = redoStack.head

    deltas.find(_.dot == dot) match
        case Some(value) =>
          val dot   = nextLocalDot
          val delta = UndoRedoReplica.Delta(dot, value.delta)
          this.copy(
            deltas = deltas + delta,
            undoStack = dot :: undoStack,
            redoStack = redoStack.tail,
            nextLocalDot = nextLocalDot.advance,
            buffer = buffer.add(delta)
          )
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
