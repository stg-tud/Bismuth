package test.rdts.bespoke

import rdts.experiments.UndoRedoDeltaBased
import rdts.base.Uid
import rdts.time.Dot
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.ReplicatedSet

class UndoRedoDeltaBasedMVRTest extends munit.FunSuite {
  // test("replicated set operation") {
  //   val rid = LocalUid.predefined("R1")
  //   val set = ReplicatedSet.empty[Int].add(using rid)(42)
  //   assertEquals(set.elements, Set(42))

  //   val set1 = set `merge` set.add(using rid)(4)
  //   assertEquals(set1.elements, Set(4, 42))

  //   val set2 = set1 `merge` set1.remove(4)
  //   assertEquals(set2.elements, Set(42))

  //   val set3 = set2 `merge` set2.add(using rid)(1)
  //   assertEquals(set3.elements, Set(1, 42))
  // }

  test("single set operation") {
    val rid      = LocalUid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.empty[Int]
    val merged   = register `merge` register.set(42)(using rid)
    assertEquals(merged.values(), List(42))
  }

  test("sequential set operations same replica") {
    val rid      = LocalUid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.empty[Int]

    val register1 = register `merge` register.set(1)(using rid)
    assertEquals(register1.heads(), List(Dot(rid.uid, 0)))

    val register2 = register1 `merge` register1.set(2)(using rid)
    assertEquals(register2.heads(), List(Dot(rid.uid, 1)))
    assertEquals(register2.values(), List(2))
  }

  test("concurrent set operations") {
    val rid1      = LocalUid.predefined("R1")
    val rid2      = LocalUid.predefined("R2")
    val register1 = UndoRedoDeltaBased.MVR.empty[Int]
    val register2 = UndoRedoDeltaBased.MVR.empty[Int]

    // Concurrent sets on different replicas
    val reg1Set = register1.set(1)(using rid1)
    val reg2Set = register2.set(2)(using rid2)

    // Merge operations to simulate concurrent updates
    val merged1 = reg1Set `merge` reg2Set
    val merged2 = reg2Set `merge` reg1Set

    // Both replicas should see both values as siblings
    assertEquals(merged1.values().toSet, Set(1, 2))
    assertEquals(merged2.values().toSet, Set(1, 2))
  }

  test("three way concurrent sets") {
    val rid1     = LocalUid.predefined("R1")
    val rid2     = LocalUid.predefined("R2")
    val rid3     = LocalUid.predefined("R3")
    val replica1 = UndoRedoDeltaBased.MVR.empty[Int].set(1)(using rid1)
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int].set(2)(using rid2)
    val replica3 = UndoRedoDeltaBased.MVR.empty[Int].set(3)(using rid3)

    val merged1 = replica1 `merge` replica2 `merge` replica3
    val merged2 = replica2 `merge` replica1 `merge` replica3
    val merged3 = replica3 `merge` replica1 `merge` replica2

    assertEquals(merged1.values().toSet, Set(1, 2, 3))
    assertEquals(merged2.values().toSet, Set(1, 2, 3))
    assertEquals(merged3.values().toSet, Set(1, 2, 3))
  }

  test("causal dependency resolution") {
    val rid1     = LocalUid.predefined("R1")
    val rid2     = LocalUid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]

    // Sequential: op1 → op2
    val delta1    = replica1.set(1)(using rid1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    assertEquals(replica1a.values(), List(1))
    assertEquals(replica2a.values(), List(1))

    val delta2    = replica2a.set(2)(using rid2)
    val replica1b = replica1a `merge` delta2
    val replica2b = replica2a `merge` delta2

    assertEquals(replica1b.values(), List(2))
    assertEquals(replica2b.values(), List(2))
  }

  test("delete operation") {
    val rid       = LocalUid.predefined("R1")
    val register  = UndoRedoDeltaBased.MVR.empty[Int]
    val register1 = register `merge` register.set(42)(using rid)
    val register2 = register1 `merge` register1.delete()(using rid)
    assertEquals(register2.values(), List())
  }

  test("delete with concurrent set") {
    val rid1     = LocalUid.predefined("R1")
    val rid2     = LocalUid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]

    val delta1    = replica1.set(1)(using rid1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    val delta2a = replica1a.delete()(using rid1)
    val delta2b = replica2a.set(2)(using rid2)

    val replica1b = replica1a `merge` delta2a `merge` delta2b
    val replica2b = replica1a `merge` delta2b `merge` delta2a

    assertEquals(replica1b.values(), List(2))
    assertEquals(replica2b.values(), List(2))
  }

  test("concurrent deletes") {
    val rid1     = LocalUid.predefined("R1")
    val rid2     = LocalUid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]

    val deltaInit = replica1.set(1)(using rid1)
    val replica1a = replica1 `merge` deltaInit
    val replica2a = replica2 `merge` deltaInit

    val deltaDelete1 = replica1a.delete()(using rid1)
    val deltaDelete2 = replica2a.delete()(using rid2)

    val replica1b = replica1a `merge` deltaDelete2
    val replica2b = replica2a `merge` deltaDelete1

    assertEquals(replica1b.values(), List())
    assertEquals(replica2b.values(), List())
  }

  test("apply same operation twice") {
    val rid       = LocalUid.predefined("R1")
    val register  = UndoRedoDeltaBased.MVR.empty[Int]
    val delta     = register.set(42)(using rid)
    val register1 = register `merge` delta

    // Applying the same delta again should not change the state
    val register2 = register1 `merge` delta
    assertEquals(register2.values(), List(42))
  }

  test("complex concurrent scenario (delta-based)") {
    val rid1 = LocalUid.predefined("R1")
    val rid2 = LocalUid.predefined("R2")
    val rid3 = LocalUid.predefined("R3")

    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica3 = UndoRedoDeltaBased.MVR.empty[Int]

    val deltaInit = replica1.set(0)(using rid1)
    val replica1a = replica1 `merge` deltaInit
    val replica2a = replica2 `merge` deltaInit
    val replica3a = replica3 `merge` deltaInit

    val deltaSet1   = replica1a.set(1)(using rid1)
    val deltaDelete = replica2a.delete()(using rid2)
    val deltaSet3   = replica3a.set(3)(using rid3)

    val replica1c = replica1a `merge` deltaSet1 `merge` deltaDelete `merge` deltaSet3
    val replica2c = replica2a `merge` deltaDelete `merge` deltaSet1 `merge` deltaSet3
    val replica3c = replica3a `merge` deltaSet3 `merge` deltaSet1 `merge` deltaDelete

    assertEquals(replica1c.values(), List(3, 1))
    assertEquals(replica2c.values(), List(3, 1))
    assertEquals(replica3c.values(), List(3, 1))
  }

  test("empty register operations") {
    val rid1 = LocalUid.predefined("R1")

    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]

    assert(replica1.values().isEmpty)

    val delta     = replica1.delete()(using rid1)
    val replica1a = replica1 `merge` delta
    val replica2a = replica2 `merge` delta

    assert(replica1a.values().isEmpty)
    assert(replica2a.values().isEmpty)
  }

  test("heads tracking") {
    val rid1 = LocalUid.predefined("R1")
    val rid2 = LocalUid.predefined("R2")

    val replica1 = UndoRedoDeltaBased.MVR.empty[Int]
    val replica2 = UndoRedoDeltaBased.MVR.empty[Int]

    // op_1
    val delta1    = replica1.set(1)(using rid1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    assertEquals(replica1a.heads(), List(Dot(rid1.uid, 0)))
    assertEquals(replica2a.heads(), List(Dot(rid1.uid, 0)))

    // op_2
    val delta2    = replica2a.set(1)(using rid2)
    val replica1b = replica1a `merge` delta2
    val replica2b = replica2a `merge` delta2

    assertEquals(replica1b.heads(), List(Dot(rid2.uid, 0)))
    assertEquals(replica2b.heads(), List(Dot(rid2.uid, 0)))

    val delta3a   = replica1b.set(4)(using rid1)
    val delta3b   = replica2b.set(3)(using rid2)
    val replica1c = replica1b `merge` delta3a
    val replica2c = replica2b `merge` delta3b

    assertEquals(replica1c.heads(), List(Dot(rid1.uid, 1)))
    assertEquals(replica2c.heads(), List(Dot(rid2.uid, 1)))
    assertEquals(replica1c.operation(Dot(rid1.uid, 1)).get.predecessors, Set(Dot(rid2.uid, 0)))
    assertEquals(replica2c.operation(Dot(rid2.uid, 1)).get.predecessors, Set(Dot(rid2.uid, 0)))

    val replica1d = replica1c `merge` delta3b
    val replica2d = replica2c `merge` delta3a

    assertEquals(replica1d.heads(), List(Dot(rid1.uid, 1), Dot(rid2.uid, 1)))
    assertEquals(replica2d.heads(), List(Dot(rid1.uid, 1), Dot(rid2.uid, 1)))

    val delta4    = replica2d.set(5)(using rid2)
    val replica1e = replica1d `merge` delta4
    val replica2e = replica2d `merge` delta4

    assertEquals(replica1e.heads(), List(Dot(rid2.uid, 2)))
    assertEquals(replica2e.heads(), List(Dot(rid2.uid, 2)))
    assertEquals(replica1e.operation(Dot(rid2.uid, 2)).get.predecessors, Set(Dot(rid1.uid, 1), Dot(rid2.uid, 1)))
    assertEquals(replica2e.operation(Dot(rid2.uid, 2)).get.predecessors, Set(Dot(rid1.uid, 1), Dot(rid2.uid, 1)))
  }

  test("simple undo redo") {
    val rid      = LocalUid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.empty[Int]

    val register1 = register `merge` register.set(1)(using rid)
    val register2 = register1 `merge` register1.delete()(using rid)

    assertEquals(register2.values(), List())

    val register3 = register2 `merge` register2.undo()(using rid)
    assertEquals(register3.values(), List(1))

    val register4 = register3 `merge` register3.redo()(using rid)
    assertEquals(register4.values(), List())
  }

  // This test replicates the example from Figure 2 in the paper
  test("undo/redo paper example") {
    val ridA = LocalUid.predefined("R1")
    val ridB = LocalUid.predefined("R2")

    val replicaA = UndoRedoDeltaBased.MVR.empty[Int]
    val replicaB = UndoRedoDeltaBased.MVR.empty[Int]

    val delta1    = replicaA.set(1)(using ridA)
    val replicaA1 = replicaA `merge` delta1
    val replicaB1 = replicaB `merge` delta1

    val delta2    = replicaB1.set(2)(using ridB)
    val replicaA2 = replicaA1 `merge` delta2
    val replicaB2 = replicaB1 `merge` delta2

    val delta3a   = replicaA2.set(4)(using ridA)
    val delta3b   = replicaB2.set(3)(using ridB)
    val replicaA3 = replicaA2 `merge` delta3a `merge` delta3b
    val replicaB3 = replicaB2 `merge` delta3b `merge` delta3a

    val delta4    = replicaB3.set(5)(using ridB)
    val replicaA4 = replicaA3 `merge` delta4
    val replicaB4 = replicaB3 `merge` delta4

    // (1)
    assertEquals(undoValues(replicaA4), List(1, 4))
    assertEquals(redoAnchors(replicaA4), List())
    assertEquals(replicaA4.values(), List(5))
    assertEquals(undoValues(replicaB4), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB4), List())
    assertEquals(replicaB4.values(), List(5))

    // // op_5_a and op_5_b (undo)
    // val (replicaA6, op5a) = replicaA5.undo()
    // val (replicaB6, op5b) = replicaB5.undo()

    // // (2a)
    // assertEquals(undoValues(replicaA6), List(1))
    // assertEquals(redoAnchors(replicaA6), List(op3a.id))
    // assertEquals(replicaA6.values(), List(2))
    // assertEquals(undoValues(replicaB6), List(2, 3))
    // assertEquals(redoAnchors(replicaB6), List(op4.id))
    // assertEquals(replicaB6.values(), List(4, 3))

    // // Exchange undo ops
    // val replicaA7 = replicaA6.applyRemoteOperation(op5b.get)
    // val replicaB7 = replicaB6.applyRemoteOperation(op5a.get)

    // // (2b)
    // assertEquals(undoValues(replicaA7), List(1))
    // assertEquals(redoAnchors(replicaA7), List(op3a.id))
    // assertEquals(replicaA7.values(), List(2, 4, 3))
    // assertEquals(undoValues(replicaB7), List(2, 3))
    // assertEquals(redoAnchors(replicaB7), List(op4.id))
    // assertEquals(replicaB7.values(), List(2, 4, 3))

    // // op_6 (undo)
    // val (replicaB8, op6) = replicaB7.undo()
    // val replicaA8        = replicaA7.applyRemoteOperation(op6.get)

    // // (3)
    // assertEquals(undoValues(replicaA8), List(1))
    // assertEquals(redoAnchors(replicaA8), List(op3a.id))
    // assertEquals(replicaA8.values(), List(2))
    // assertEquals(undoValues(replicaB8), List(2))
    // assertEquals(redoAnchors(replicaB8), List(op4.id, op3b.id))
    // assertEquals(replicaB8.values(), List(2))

    // // op_7_a (set) and op_7_b (undo)
    // val (replicaA9, op7a) = replicaA8.set(6)
    // val (replicaB9, op7b) = replicaB8.undo()
    // val replicaB10        = replicaB9.applyRemoteOperation(op7a)
    // val replicaA10        = replicaA9.applyRemoteOperation(op7b.get)

    // // (4)
    // assertEquals(undoValues(replicaA10), List(1, 6))
    // assertEquals(redoAnchors(replicaA10), List())
    // assertEquals(replicaA10.values(), List(1, 6))
    // assertEquals(undoValues(replicaB10), List())
    // assertEquals(redoAnchors(replicaB10), List(op4.id, op3b.id, op2.id))
    // assertEquals(replicaB10.values(), List(1, 6))

    // // op_8 (redo)
    // val (replicaB11, op8) = replicaB10.redo()
    // val replicaA11        = replicaA10.applyRemoteOperation(op8.get)

    // // (5)
    // assertEquals(undoValues(replicaA11), List(1, 6))
    // assertEquals(redoAnchors(replicaA11), List())
    // assertEquals(replicaA11.values(), List(2))
    // assertEquals(undoValues(replicaB11), List(2))
    // assertEquals(redoAnchors(replicaB11), List(op4.id, op3b.id))
    // assertEquals(replicaB11.values(), List(2))

    // // op_9 (redo)
    // val (replicaB12, op9) = replicaB11.redo()
    // val replicaA12        = replicaA11.applyRemoteOperation(op9.get)

    // // (6)
    // assertEquals(undoValues(replicaA12), List(1, 6))
    // assertEquals(redoAnchors(replicaA12), List())
    // assertEquals(replicaA12.values(), List(2, 4, 3))
    // assertEquals(undoValues(replicaB12), List(2, 3))
    // assertEquals(redoAnchors(replicaB12), List(op4.id))
    // assertEquals(replicaB12.values(), List(2, 4, 3))

    // // op_10 (redo)
    // val (replicaB13, op10) = replicaB12.redo()
    // val replicaA13         = replicaA12.applyRemoteOperation(op10.get)

    // // (7)
    // assertEquals(undoValues(replicaA13), List(1, 6))
    // assertEquals(redoAnchors(replicaA13), List())
    // assertEquals(replicaA13.values(), List(5))
    // assertEquals(undoValues(replicaB13), List(2, 3, 5))
    // assertEquals(redoAnchors(replicaB13), List())
    // assertEquals(replicaB13.values(), List(5))
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
