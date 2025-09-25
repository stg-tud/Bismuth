package test.rdts.bespoke

import rdts.experiments.UndoRedoDeltaBased
import rdts.base.Uid
import rdts.time.Dot
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.ReplicatedSet

case class MVRTestReplica[A](id: Uid, var state: A) {
  def apply(delta: A)(using Lattice[A]): MVRTestReplica[A] =
    state = state `merge` delta
    this

  def mod(f: A => A)(using Lattice[A]) = {
    apply(f(state))
  }

  def receive(other: MVRTestReplica[A])(using Lattice[A]) = {
    apply(other.state)
  }
}

object MVRTestReplica {
  def create[A](n: Int): Array[MVRTestReplica[UndoRedoDeltaBased.MVR[A]]] = {
    (1 to n).map(i => Uid.predefined(s"R$i")).map(rid =>
      MVRTestReplica(rid, UndoRedoDeltaBased.MVR.forReplica[A](rid))
    ).toArray
  }
}

class UndoRedoDeltaBasedMVRTest extends munit.FunSuite {
  test("single set operation") {
    val Array(replica) = MVRTestReplica.create[Int](1)
    replica.mod(_.set(42))
    assertEquals(replica.state.values(), List(42))
  }

  test("sequential set operations same replica") {
    val Array(replica) = MVRTestReplica.create[Int](1)

    replica.mod(_.set(1))
    assertEquals(replica.state.heads(), List(Dot(replica.id, 0)))

    replica.mod(_.set(2))
    assertEquals(replica.state.heads(), List(Dot(replica.id, 1)))
    assertEquals(replica.state.values(), List(2))
  }

  test("concurrent set operations") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    // Concurrent sets on different replicas
    val delta1 = replica1.mod(_.set(1))
    val delta2 = replica2.mod(_.set(2))

    replica2.receive(delta1)
    replica1.receive(delta2)

    // Both replicas should see both values as siblings
    assertEquals(replica1.state.values().toSet, Set(1, 2))
    assertEquals(replica2.state.values().toSet, Set(1, 2))
  }

  test("three way concurrent sets") {
    val Array(replica1, replica2, replica3) = MVRTestReplica.create[Int](3)

    val delta1 = replica1.mod(_.set(1))
    val delta2 = replica2.mod(_.set(2))
    val delta3 = replica3.mod(_.set(3))

    replica1.receive(delta2).receive(delta3)
    replica2.receive(delta1).receive(delta3)
    replica3.receive(delta1).receive(delta2)

    assertEquals(replica1.state.values().toSet, Set(1, 2, 3))
    assertEquals(replica2.state.values().toSet, Set(1, 2, 3))
    assertEquals(replica3.state.values().toSet, Set(1, 2, 3))
  }

  test("causal dependency resolution") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    // Sequential: op1 → op2
    val delta1 = replica1.mod(_.set(1))
    replica2.receive(delta1)

    assertEquals(replica1.state.values(), List(1))
    assertEquals(replica2.state.values(), List(1))

    val delta2    = replica2.mod(_.set(2))
    val replica1b = replica1.receive(delta2)

    assertEquals(replica1.state.values(), List(2))
    assertEquals(replica2.state.values(), List(2))
  }

  test("delete operation") {
    val Array(replica) = MVRTestReplica.create[Int](1)

    replica.mod(_.set(42))
    replica.mod(_.delete())

    assertEquals(replica.state.values(), List())
  }

  test("delete with concurrent set") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    val delta1 = replica1.mod(_.set(1))
    replica2.receive(delta1)

    val delta2a = replica1.mod(_.delete())
    val delta2b = replica2.mod(_.set(2))

    replica1.receive(delta2b)
    replica2.receive(delta2a)

    assertEquals(replica1.state.values(), List(2))
    assertEquals(replica2.state.values(), List(2))
  }

  test("concurrent deletes") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    val deltaInit = replica1.mod(_.set(1))
    replica2.receive(deltaInit)

    val deltaDelete1 = replica1.mod(_.delete())
    val deltaDelete2 = replica2.mod(_.delete())
    replica1.receive(deltaDelete2)
    replica2.receive(deltaDelete1)

    assertEquals(replica1.state.values(), List())
    assertEquals(replica2.state.values(), List())
  }

  test("apply same operation twice") {
    val Array(replica) = MVRTestReplica.create[Int](1)

    val delta = replica.mod(_.set(42))
    // Applying the same delta again should not change the state
    replica.receive(delta)

    assertEquals(replica.state.values(), List(42))
  }

  test("complex concurrent scenario (delta-based)") {
    val Array(replica1, replica2, replica3) = MVRTestReplica.create[Int](3)

    val deltaInit = replica1.mod(_.set(0))
    replica2.receive(deltaInit)
    replica3.receive(deltaInit)

    val delta1a = replica1.mod(_.set(1))
    val delta1b = replica2.mod(_.delete())
    val delta1c = replica3.mod(_.set(3))

    replica1.receive(delta1b).receive(delta1c)
    replica2.receive(delta1a).receive(delta1c)
    replica3.receive(delta1a).receive(delta1b)

    assertEquals(replica1.state.values(), List(3, 1))
    assertEquals(replica2.state.values(), List(3, 1))
    assertEquals(replica3.state.values(), List(3, 1))
  }

  test("empty register operations") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    assert(replica1.state.values().isEmpty)

    val delta = replica1.mod(_.delete())
    replica2.receive(delta)

    assert(replica1.state.values().isEmpty)
    assert(replica2.state.values().isEmpty)
  }

  test("heads tracking") {
    val Array(replica1, replica2) = MVRTestReplica.create[Int](2)

    val delta1 = replica1.mod(_.set(1))
    replica2.receive(delta1)

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 0)))
    assertEquals(replica2.state.heads(), List(Dot(replica1.id, 0)))

    val delta2 = replica2.mod(_.set(1))
    replica1.receive(delta2)

    assertEquals(replica1.state.heads(), List(Dot(replica2.id, 0)))
    assertEquals(replica2.state.heads(), List(Dot(replica2.id, 0)))

    val delta3a = replica1.mod(_.set(4))
    val delta3b = replica2.mod(_.set(3))

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 1)))
    assertEquals(replica2.state.heads(), List(Dot(replica2.id, 1)))
    assertEquals(replica1.state.operation(Dot(replica1.id, 1)).get.predecessors, Set(Dot(replica2.id, 0)))
    assertEquals(replica2.state.operation(Dot(replica2.id, 1)).get.predecessors, Set(Dot(replica2.id, 0)))

    replica1.receive(delta3b)
    replica2.receive(delta3a)

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 1), Dot(replica2.id, 1)))
    assertEquals(replica2.state.heads(), List(Dot(replica1.id, 1), Dot(replica2.id, 1)))

    val delta4 = replica2.mod(_.set(5))
    replica1.receive(delta4)

    assertEquals(replica1.state.heads(), List(Dot(replica2.id, 2)))
    assertEquals(replica2.state.heads(), List(Dot(replica2.id, 2)))
    assertEquals(
      replica1.state.operation(Dot(replica2.id, 2)).get.predecessors,
      Set(Dot(replica1.id, 1), Dot(replica2.id, 1))
    )
    assertEquals(
      replica2.state.operation(Dot(replica2.id, 2)).get.predecessors,
      Set(Dot(replica1.id, 1), Dot(replica2.id, 1))
    )
  }

  test("simple undo redo") {
    val Array(replica) = MVRTestReplica.create[Int](1)

    replica.mod(_.set(1))
    replica.mod(_.delete())

    assertEquals(replica.state.values(), List())

    replica.mod(_.undo())
    assertEquals(replica.state.values(), List(1))

    replica.mod(_.redo())
    assertEquals(replica.state.values(), List())
  }

  // This test replicates the example from Figure 2 in the paper
  test("undo/redo paper example") {
    val Array(replicaA, replicaB) = MVRTestReplica.create[Int](2)

    // op_1
    val delta1 = replicaA.mod(_.set(1))
    replicaB.receive(delta1)

    // op_2
    val delta2 = replicaB.mod(_.set(2))
    replicaA.receive(delta2)

    // op_3_a and op_3_b
    val delta3a = replicaA.mod(_.set(4))
    val delta3b = replicaB.mod(_.set(3))
    replicaA.receive(delta3b)
    replicaB.receive(delta3a)

    // op_4
    val delta4 = replicaB.mod(_.set(5))
    replicaA.receive(delta4)

    // (1)
    assertEquals(undoValues(replicaA.state), List(1, 4))
    assertEquals(redoAnchors(replicaA.state), List())
    assertEquals(replicaA.state.values(), List(5))
    assertEquals(undoValues(replicaB.state), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB.state), List())
    assertEquals(replicaB.state.values(), List(5))

    // op_5_a and op_5_b (undo)
    val delta5a = replicaA.mod(_.undo())
    val delta5b = replicaB.mod(_.undo())

    // (2a)
    assertEquals(undoValues(replicaA.state), List(1))
    assertEquals(redoAnchors(replicaA.state), List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(undoValues(replicaB.state), List(2, 3))
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(4, 3))

    // Exchange undo ops
    replicaA.receive(delta5b)
    replicaB.receive(delta5a)

    // (2b)
    assertEquals(undoValues(replicaA.state), List(1))
    assertEquals(redoAnchors(replicaA.state), List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB.state), List(2, 3))
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(2, 4, 3))

    // op_6 (undo)
    val delta6 = replicaB.mod(_.undo())
    replicaA.receive(delta6)

    // (3)
    assertEquals(undoValues(replicaA.state), List(1))
    assertEquals(redoAnchors(replicaA.state), List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(undoValues(replicaB.state), List(2))
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2), Dot(replicaB.id, 1)))
    assertEquals(replicaB.state.values(), List(2))

    // op_7_a (set) and op_7_b (undo)
    val delta7a = replicaA.mod(_.set(6))
    val delta7b = replicaB.mod(_.undo())
    replicaA.receive(delta7b)
    replicaB.receive(delta7a)

    // (4)
    assertEquals(undoValues(replicaA.state), List(1, 6))
    assertEquals(redoAnchors(replicaA.state), List())
    assertEquals(replicaA.state.values(), List(1, 6))
    assertEquals(undoValues(replicaB.state), List())
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2), Dot(replicaB.id, 1), Dot(replicaB.id, 0)))
    assertEquals(replicaB.state.values(), List(1, 6))

    // op_8 (redo)
    val delta8 = replicaB.mod(_.redo())
    replicaA.receive(delta8)

    // (5)
    assertEquals(undoValues(replicaA.state), List(1, 6))
    assertEquals(redoAnchors(replicaA.state), List())
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(undoValues(replicaB.state), List(2))
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2), Dot(replicaB.id, 1)))
    assertEquals(replicaB.state.values(), List(2))

    // op_9 (redo)
    val delta9 = replicaB.mod(_.redo())
    replicaA.receive(delta9)

    // (6)
    assertEquals(undoValues(replicaA.state), List(1, 6))
    assertEquals(redoAnchors(replicaA.state), List())
    assertEquals(replicaA.state.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB.state), List(2, 3))
    assertEquals(redoAnchors(replicaB.state), List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(2, 4, 3))

    // op_10 (redo)
    val delta10 = replicaB.mod(_.redo())
    replicaA.receive(delta10)

    // (7)
    assertEquals(undoValues(replicaA.state), List(1, 6))
    assertEquals(redoAnchors(replicaA.state), List())
    assertEquals(replicaA.state.values(), List(5))
    assertEquals(undoValues(replicaB.state), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB.state), List())
    assertEquals(replicaB.state.values(), List(5))
  }
}

def undoValues[T](register: UndoRedoDeltaBased.MVR[T]): List[T] = {
  register.undoStack.toList
    .flatMap(_.ty.getValue)
    .reverse
}

def redoAnchors[T](register: UndoRedoDeltaBased.MVR[T]): List[Dot] = {
  register.redoStack.toList
    .flatMap(_.ty.getAnchor)
    .reverse
}
