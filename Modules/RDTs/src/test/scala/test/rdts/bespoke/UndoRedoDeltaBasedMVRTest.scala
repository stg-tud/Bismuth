package test.rdts.bespoke

import rdts.experiments.UndoRedoDeltaBased
import rdts.base.Uid
import rdts.time.Dot
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.ReplicatedSet

class UndoRedoDeltaBasedMVRTest extends munit.FunSuite {
  test("single set operation") {
    val rid      = Uid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.forReplica[Int](rid)
    val merged   = register `merge` register.set(42)
    assertEquals(merged.values(), List(42))
  }

  test("sequential set operations same replica") {
    val rid      = Uid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.forReplica[Int](rid)

    val register1 = register `merge` register.set(1)
    assertEquals(register1.heads(), List(Dot(rid, 0)))

    val register2 = register1 `merge` register1.set(2)
    assertEquals(register2.heads(), List(Dot(rid, 1)))
    assertEquals(register2.values(), List(2))
  }

  test("concurrent set operations") {
    val rid1      = Uid.predefined("R1")
    val rid2      = Uid.predefined("R2")
    val register1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val register2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    // Concurrent sets on different replicas
    val reg1Set = register1.set(1)
    val reg2Set = register2.set(2)

    // Merge operations to simulate concurrent updates
    val merged1 = reg1Set `merge` reg2Set
    val merged2 = reg2Set `merge` reg1Set

    // Both replicas should see both values as siblings
    assertEquals(merged1.values().toSet, Set(1, 2))
    assertEquals(merged2.values().toSet, Set(1, 2))
  }

  test("three way concurrent sets") {
    val rid1     = Uid.predefined("R1")
    val rid2     = Uid.predefined("R2")
    val rid3     = Uid.predefined("R3")
    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1).set(1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2).set(2)
    val replica3 = UndoRedoDeltaBased.MVR.forReplica[Int](rid3).set(3)

    val merged1 = replica1 `merge` replica2 `merge` replica3
    val merged2 = replica2 `merge` replica1 `merge` replica3
    val merged3 = replica3 `merge` replica1 `merge` replica2

    assertEquals(merged1.values().toSet, Set(1, 2, 3))
    assertEquals(merged2.values().toSet, Set(1, 2, 3))
    assertEquals(merged3.values().toSet, Set(1, 2, 3))
  }

  test("causal dependency resolution") {
    val rid1     = Uid.predefined("R1")
    val rid2     = Uid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    // Sequential: op1 → op2
    val delta1    = replica1.set(1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    assertEquals(replica1a.values(), List(1))
    assertEquals(replica2a.values(), List(1))

    val delta2    = replica2a.set(2)
    val replica1b = replica1a `merge` delta2
    val replica2b = replica2a `merge` delta2

    assertEquals(replica1b.values(), List(2))
    assertEquals(replica2b.values(), List(2))
  }

  test("delete operation") {
    val rid       = Uid.predefined("R1")
    val register  = UndoRedoDeltaBased.MVR.forReplica[Int](rid)
    val register1 = register `merge` register.set(42)
    val register2 = register1 `merge` register1.delete()
    assertEquals(register2.values(), List())
  }

  test("delete with concurrent set") {
    val rid1     = Uid.predefined("R1")
    val rid2     = Uid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    val delta1    = replica1.set(1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    val delta2a = replica1a.delete()
    val delta2b = replica2a.set(2)

    val replica1b = replica1a `merge` delta2a `merge` delta2b
    val replica2b = replica1a `merge` delta2b `merge` delta2a

    assertEquals(replica1b.values(), List(2))
    assertEquals(replica2b.values(), List(2))
  }

  test("concurrent deletes") {
    val rid1     = Uid.predefined("R1")
    val rid2     = Uid.predefined("R2")
    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    val deltaInit = replica1.set(1)
    val replica1a = replica1 `merge` deltaInit
    val replica2a = replica2 `merge` deltaInit

    val deltaDelete1 = replica1a.delete()
    val deltaDelete2 = replica2a.delete()

    val replica1b = replica1a `merge` deltaDelete2
    val replica2b = replica2a `merge` deltaDelete1

    assertEquals(replica1b.values(), List())
    assertEquals(replica2b.values(), List())
  }

  test("apply same operation twice") {
    val rid       = Uid.predefined("R1")
    val register  = UndoRedoDeltaBased.MVR.forReplica[Int](rid)
    val delta     = register.set(42)
    val register1 = register `merge` delta

    // Applying the same delta again should not change the state
    val register2 = register1 `merge` delta
    assertEquals(register2.values(), List(42))
  }

  test("complex concurrent scenario (delta-based)") {
    val rid1 = Uid.predefined("R1")
    val rid2 = Uid.predefined("R2")
    val rid3 = Uid.predefined("R3")

    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)
    val replica3 = UndoRedoDeltaBased.MVR.forReplica[Int](rid3)

    val deltaInit = replica1.set(0)
    val replica1a = replica1 `merge` deltaInit
    val replica2a = replica2 `merge` deltaInit
    val replica3a = replica3 `merge` deltaInit

    val deltaSet1   = replica1a.set(1)
    val deltaDelete = replica2a.delete()
    val deltaSet3   = replica3a.set(3)

    val replica1c = replica1a `merge` deltaSet1 `merge` deltaDelete `merge` deltaSet3
    val replica2c = replica2a `merge` deltaDelete `merge` deltaSet1 `merge` deltaSet3
    val replica3c = replica3a `merge` deltaSet3 `merge` deltaSet1 `merge` deltaDelete

    assertEquals(replica1c.values(), List(3, 1))
    assertEquals(replica2c.values(), List(3, 1))
    assertEquals(replica3c.values(), List(3, 1))
  }

  test("empty register operations") {
    val rid1 = Uid.predefined("R1")
    val rid2 = Uid.predefined("R2")

    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    assert(replica1.values().isEmpty)

    val delta     = replica1.delete()
    val replica1a = replica1 `merge` delta
    val replica2a = replica2 `merge` delta

    assert(replica1a.values().isEmpty)
    assert(replica2a.values().isEmpty)
  }

  test("heads tracking") {
    val rid1 = Uid.predefined("R1")
    val rid2 = Uid.predefined("R2")

    val replica1 = UndoRedoDeltaBased.MVR.forReplica[Int](rid1)
    val replica2 = UndoRedoDeltaBased.MVR.forReplica[Int](rid2)

    // op_1
    val delta1    = replica1.set(1)
    val replica1a = replica1 `merge` delta1
    val replica2a = replica2 `merge` delta1

    assertEquals(replica1a.heads(), List(Dot(rid1, 0)))
    assertEquals(replica2a.heads(), List(Dot(rid1, 0)))

    // op_2
    val delta2    = replica2a.set(1)
    val replica1b = replica1a `merge` delta2
    val replica2b = replica2a `merge` delta2

    assertEquals(replica1b.heads(), List(Dot(rid2, 0)))
    assertEquals(replica2b.heads(), List(Dot(rid2, 0)))

    val delta3a   = replica1b.set(4)
    val delta3b   = replica2b.set(3)
    val replica1c = replica1b `merge` delta3a
    val replica2c = replica2b `merge` delta3b

    assertEquals(replica1c.heads(), List(Dot(rid1, 1)))
    assertEquals(replica2c.heads(), List(Dot(rid2, 1)))
    assertEquals(replica1c.operation(Dot(rid1, 1)).get.predecessors, Set(Dot(rid2, 0)))
    assertEquals(replica2c.operation(Dot(rid2, 1)).get.predecessors, Set(Dot(rid2, 0)))

    val replica1d = replica1c `merge` delta3b
    val replica2d = replica2c `merge` delta3a

    assertEquals(replica1d.heads(), List(Dot(rid1, 1), Dot(rid2, 1)))
    assertEquals(replica2d.heads(), List(Dot(rid1, 1), Dot(rid2, 1)))

    val delta4    = replica2d.set(5)
    val replica1e = replica1d `merge` delta4
    val replica2e = replica2d `merge` delta4

    assertEquals(replica1e.heads(), List(Dot(rid2, 2)))
    assertEquals(replica2e.heads(), List(Dot(rid2, 2)))
    assertEquals(replica1e.operation(Dot(rid2, 2)).get.predecessors, Set(Dot(rid1, 1), Dot(rid2, 1)))
    assertEquals(replica2e.operation(Dot(rid2, 2)).get.predecessors, Set(Dot(rid1, 1), Dot(rid2, 1)))
  }

  test("simple undo redo") {
    val rid      = Uid.predefined("R1")
    val register = UndoRedoDeltaBased.MVR.forReplica[Int](rid)

    val register1 = register `merge` register.set(1)
    val register2 = register1 `merge` register1.delete()

    assertEquals(register2.values(), List())

    val register3 = register2 `merge` register2.undo()
    assertEquals(register3.values(), List(1))

    val register4 = register3 `merge` register3.redo()
    assertEquals(register4.values(), List())
  }

  // This test replicates the example from Figure 2 in the paper
  test("undo/redo paper example") {
    val ridA = Uid.predefined("R1")
    val ridB = Uid.predefined("R2")

    val replicaA = UndoRedoDeltaBased.MVR.forReplica[Int](ridA)
    val replicaB = UndoRedoDeltaBased.MVR.forReplica[Int](ridB)

    // op_1
    val delta1    = replicaA.set(1)
    val replicaA1 = replicaA `merge` delta1
    val replicaB1 = replicaB `merge` delta1

    // op_2
    val delta2    = replicaB1.set(2)
    val replicaA2 = replicaA1 `merge` delta2
    val replicaB2 = replicaB1 `merge` delta2

    // op_3_a and op_3_b
    val delta3a   = replicaA2.set(4)
    val delta3b   = replicaB2.set(3)
    val replicaA3 = replicaA2 `merge` delta3a `merge` delta3b
    val replicaB3 = replicaB2 `merge` delta3b `merge` delta3a

    // op_4
    val delta4    = replicaB3.set(5)
    val replicaA4 = replicaA3 `merge` delta4
    val replicaB4 = replicaB3 `merge` delta4

    // (1)
    assertEquals(undoValues(replicaA4), List(1, 4))
    assertEquals(redoAnchors(replicaA4), List())
    assertEquals(replicaA4.values(), List(5))
    assertEquals(undoValues(replicaB4), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB4), List())
    assertEquals(replicaB4.values(), List(5))

    // op_5_a and op_5_b (undo)
    val delta5a   = replicaA4.undo()
    val delta5b   = replicaB4.undo()
    val replicaA5 = replicaA4 `merge` delta5a
    val replicaB5 = replicaB4 `merge` delta5b

    // (2a)
    assertEquals(undoValues(replicaA5), List(1))
    assertEquals(redoAnchors(replicaA5), List(replicaA2.dot))
    assertEquals(replicaA5.values(), List(2))
    assertEquals(undoValues(replicaB5), List(2, 3))
    assertEquals(redoAnchors(replicaB5), List(replicaB3.dot))
    assertEquals(replicaB5.values(), List(4, 3))

    // Exchange undo ops
    val replicaA6 = replicaA5 `merge` delta5b
    val replicaB6 = replicaB5 `merge` delta5a

    // (2b)
    assertEquals(undoValues(replicaA6), List(1))
    assertEquals(redoAnchors(replicaA6), List(replicaA2.dot))
    assertEquals(replicaA6.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB6), List(2, 3))
    assertEquals(redoAnchors(replicaB6), List(replicaB3.dot))
    assertEquals(replicaB6.values(), List(2, 4, 3))

    // op_6 (undo)
    val delta6    = replicaB6.undo()
    val replicaB7 = replicaB6 `merge` delta6
    val replicaA7 = replicaA6 `merge` delta6

    // (3)
    assertEquals(undoValues(replicaA7), List(1))
    assertEquals(redoAnchors(replicaA7), List(replicaA2.dot))
    assertEquals(replicaA7.values(), List(2))
    assertEquals(undoValues(replicaB7), List(2))
    assertEquals(redoAnchors(replicaB7), List(replicaB3.dot, replicaB2.dot))
    assertEquals(replicaB7.values(), List(2))

    // op_7_a (set) and op_7_b (undo)
    val delta7a   = replicaA7.set(6)
    val delta7b   = replicaB7.undo()
    val replicaA8 = replicaA7 `merge` delta7a `merge` delta7b
    val replicaB8 = replicaB7 `merge` delta7b `merge` delta7a

    // (4)
    assertEquals(undoValues(replicaA8), List(1, 6))
    assertEquals(redoAnchors(replicaA8), List())
    assertEquals(replicaA8.values(), List(1, 6))
    assertEquals(undoValues(replicaB8), List())
    assertEquals(redoAnchors(replicaB8), List(replicaB3.dot, replicaB2.dot, replicaB1.dot))
    assertEquals(replicaB8.values(), List(1, 6))

    // op_8 (redo)
    val delta8    = replicaB8.redo()
    val replicaB9 = replicaB8 `merge` delta8
    val replicaA9 = replicaA8 `merge` delta8

    // (5)
    assertEquals(undoValues(replicaA9), List(1, 6))
    assertEquals(redoAnchors(replicaA9), List())
    assertEquals(replicaA9.values(), List(2))
    assertEquals(undoValues(replicaB9), List(2))
    assertEquals(redoAnchors(replicaB9), List(replicaB3.dot, replicaB2.dot))
    assertEquals(replicaB9.values(), List(2))

    // op_9 (redo)
    val delta9     = replicaB9.redo()
    val replicaA10 = replicaA9 `merge` delta9
    val replicaB10 = replicaB9 `merge` delta9

    // (6)
    assertEquals(undoValues(replicaA10), List(1, 6))
    assertEquals(redoAnchors(replicaA10), List())
    assertEquals(replicaA10.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB10), List(2, 3))
    assertEquals(redoAnchors(replicaB10), List(replicaB3.dot))
    assertEquals(replicaB10.values(), List(2, 4, 3))

    // op_10 (redo)
    val delta10    = replicaB10.redo()
    val replicaA11 = replicaA10 `merge` delta10
    val replicaB11 = replicaB10 `merge` delta10

    // (7)
    assertEquals(undoValues(replicaA11), List(1, 6))
    assertEquals(redoAnchors(replicaA11), List())
    assertEquals(replicaA11.values(), List(5))
    assertEquals(undoValues(replicaB11), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB11), List())
    assertEquals(replicaB11.values(), List(5))
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
