package test.rdts.bespoke

import rdts.experiments.UndoRedoOpBased
import rdts.base.Uid
import rdts.time.Dot

class UndoRedoOpBasedMVRTest extends munit.FunSuite {
  test("single set operation") {
    val Array(register)      = UndoRedoOpBased.MVR.createReplicas[Int](1)
    val (updatedRegister, _) = register.set(42)
    assertEquals(updatedRegister.values(), List(42))
  }

  test("sequential set operations same replica") {
    val Array(register) = UndoRedoOpBased.MVR.createReplicas[Int](1)
    val (register1, _)  = register.set(1)
    val (register2, _)  = register1.set(2)
    assertEquals(register2.values(), List(2))
  }

  test("concurrent set operations") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)
    val (replica1a, op1)          = replica1.set(1)
    val (replica2a, op2)          = replica2.set(2)

    val replica1b = replica1a.applyRemoteOperation(op2)
    val replica2b = replica2a.applyRemoteOperation(op1)

    assertEquals(replica1b.values(), List(1, 2))
    assertEquals(replica2b.values(), List(1, 2))
  }

  test("three way concurrent sets") {
    val Array(replica1, replica2, replica3) = UndoRedoOpBased.MVR.createReplicas[Int](3)
    val (replica1a, op1)                    = replica1.set(1)
    val (replica2a, op2)                    = replica2.set(2)
    val (replica3a, op3)                    = replica3.set(3)

    val replica1b = replica1a.applyRemoteOperation(op2).applyRemoteOperation(op3)
    val replica2b = replica2a.applyRemoteOperation(op1).applyRemoteOperation(op3)
    val replica3b = replica3a.applyRemoteOperation(op1).applyRemoteOperation(op2)

    assertEquals(replica1b.values(), List(1, 2, 3))
    assertEquals(replica2b.values(), List(1, 2, 3))
    assertEquals(replica3b.values(), List(1, 2, 3))
  }

  test("causal dependency resolution") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)

    // Sequential: op1 → op2
    val (replica1a, op1) = replica1.set(1)
    val replica2a        = replica2.applyRemoteOperation(op1)

    assertEquals(replica1a.values(), List(1))
    assertEquals(replica2a.values(), List(1))

    val (replica2b, op2) = replica2a.set(2)
    val replica1b        = replica1a.applyRemoteOperation(op2)

    assertEquals(replica1b.values(), List(2))
    assertEquals(replica2b.values(), List(2))
  }

  test("delete operation") {
    val Array(register) = UndoRedoOpBased.MVR.createReplicas[Int](1)
    val (register1, _)  = register.set(42)
    val (register2, _)  = register1.delete()
    assertEquals(register2.values(), List())
  }

  test("delete with concurrent set") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)
    val (replica1a, opInit)       = replica1.set(1)
    val replica2a                 = replica2.applyRemoteOperation(opInit)

    val (replica1b, opDelete) = replica1a.delete()
    val (replica2b, opSet)    = replica2a.set(2)

    val replica1c = replica1b.applyRemoteOperation(opSet)
    val replica2c = replica2b.applyRemoteOperation(opDelete)

    assertEquals(replica1c.values(), List(2))
    assertEquals(replica2c.values(), List(2))
  }

  test("concurrent deletes") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)
    val (replica1a, opInit)       = replica1.set(1)
    val replica2a                 = replica2.applyRemoteOperation(opInit)

    val (replica1b, opDelete1) = replica1a.delete()
    val (replica2b, opDelete2) = replica2a.delete()

    val replica1c = replica1b.applyRemoteOperation(opDelete2)
    val replica2c = replica2b.applyRemoteOperation(opDelete1)

    assertEquals(replica1c.values(), List())
    assertEquals(replica2c.values(), List())
  }

  test("apply same operation twice") {
    val Array(register) = UndoRedoOpBased.MVR.createReplicas[Int](1)
    val (register1, op) = register.set(42)
    val initialLen      = register1.values().length

    val register2 = register1.applyRemoteOperation(op)
    assertEquals(register2.values().length, initialLen)
    assertEquals(register2.values(), List(42))
  }

  test("complex concurrent scenario") {
    val Array(replica1, replica2, replica3) = UndoRedoOpBased.MVR.createReplicas[Int](3)

    val (replica1a, opInit) = replica1.set(0)
    val replica2a           = replica2.applyRemoteOperation(opInit)
    val replica3a           = replica3.applyRemoteOperation(opInit)

    val (replica1b, opSet1)   = replica1a.set(1)
    val (replica2b, opDelete) = replica2a.delete()
    val (replica3b, opSet3)   = replica3a.set(3)

    val replica1c = replica1b.applyRemoteOperation(opDelete).applyRemoteOperation(opSet3)
    val replica2c = replica2b.applyRemoteOperation(opSet1).applyRemoteOperation(opSet3)
    val replica3c = replica3b.applyRemoteOperation(opSet1).applyRemoteOperation(opDelete)

    val expected = List(3, 1)
    assertEquals(replica1c.values(), expected)
    assertEquals(replica2c.values(), expected)
    assertEquals(replica3c.values(), expected)
  }

  test("empty register operations") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)

    assert(replica1.values().isEmpty)

    val (replica1a, opDelete) = replica1.delete()
    assert(replica1a.values().isEmpty)

    val replica2a = replica2.applyRemoteOperation(opDelete)
    assert(replica2a.values().isEmpty)
  }

  test("heads tracking") {
    val Array(replica1, replica2) = UndoRedoOpBased.MVR.createReplicas[Int](2)

    // op_1
    val (replica1a, op1) = replica1.set(1)
    val replica2a        = replica2.applyRemoteOperation(op1)

    assertEquals(replica1a.heads(), List(op1.id))
    assertEquals(replica2a.heads(), List(op1.id))

    // op_2
    val (replica2b, op2) = replica2a.set(2)
    val replica1b        = replica1a.applyRemoteOperation(op2)

    assertEquals(replica1b.heads(), List(op2.id))
    assertEquals(replica2b.heads(), List(op2.id))

    // op_3_1 and op_3_2
    val (replica1c, op3_1) = replica1b.set(4)
    val (replica2c, op3_2) = replica2b.set(3)

    assertEquals(replica1c.heads(), List(op3_1.id))
    assertEquals(replica2c.heads(), List(op3_2.id))
    assertEquals(op3_1.predecessors.size, 1)
    assert(op3_1.predecessors.contains(op2.id))
    assertEquals(op3_2.predecessors.size, 1)
    assert(op3_2.predecessors.contains(op2.id))

    // Exchange op_3_1 and op_3_2
    val replica2d = replica2c.applyRemoteOperation(op3_1)
    val replica1d = replica1c.applyRemoteOperation(op3_2)

    assertEquals(replica1d.heads().toSet, Set(op3_1.id, op3_2.id))
    assertEquals(replica2d.heads().toSet, Set(op3_1.id, op3_2.id))

    // op_4
    val (replica2e, op4) = replica2d.set(5)
    val replica1e        = replica1d.applyRemoteOperation(op4)

    assertEquals(replica1e.heads(), List(op4.id))
    assertEquals(replica2e.heads(), List(op4.id))
    assertEquals(op4.predecessors.size, 2)
    assert(op4.predecessors.contains(op3_1.id))
    assert(op4.predecessors.contains(op3_2.id))
  }

  // This test replicates the example from Figure 2 in the paper
  test("undo/redo paper example") {
    val Array(replicaA, replicaB) = UndoRedoOpBased.MVR.createReplicas[Int](2)

    // op_1
    val (replicaA1, op1) = replicaA.set(1)
    val replicaB1        = replicaB.applyRemoteOperation(op1)

    // op_2
    val (replicaB2, op2) = replicaB1.set(2)
    val replicaA2        = replicaA1.applyRemoteOperation(op2)

    // op_3_a and op_3_b
    val (replicaA3, op3a) = replicaA2.set(4)
    val (replicaB3, op3b) = replicaB2.set(3)
    val replicaB4         = replicaB3.applyRemoteOperation(op3a)
    val replicaA4         = replicaA3.applyRemoteOperation(op3b)

    // op_4
    val (replicaB5, op4) = replicaB4.set(5)
    val replicaA5        = replicaA4.applyRemoteOperation(op4)

    // (1)
    assertEquals(undoValues(replicaA5), List(1, 4))
    assertEquals(redoAnchors(replicaA5), List())
    assertEquals(replicaA5.values(), List(5))
    assertEquals(undoValues(replicaB5), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB5), List())
    assertEquals(replicaB5.values(), List(5))

    // op_5_a and op_5_b (undo)
    val (replicaA6, op5a) = replicaA5.undo()
    val (replicaB6, op5b) = replicaB5.undo()

    // (2a)
    assertEquals(undoValues(replicaA6), List(1))
    assertEquals(redoAnchors(replicaA6), List(op3a.id))
    assertEquals(replicaA6.values(), List(2))
    assertEquals(undoValues(replicaB6), List(2, 3))
    assertEquals(redoAnchors(replicaB6), List(op4.id))
    assertEquals(replicaB6.values(), List(4, 3))

    // Exchange undo ops
    val replicaA7 = replicaA6.applyRemoteOperation(op5b.get)
    val replicaB7 = replicaB6.applyRemoteOperation(op5a.get)

    // (2b)
    assertEquals(undoValues(replicaA7), List(1))
    assertEquals(redoAnchors(replicaA7), List(op3a.id))
    assertEquals(replicaA7.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB7), List(2, 3))
    assertEquals(redoAnchors(replicaB7), List(op4.id))
    assertEquals(replicaB7.values(), List(2, 4, 3))

    // op_6 (undo)
    val (replicaB8, op6) = replicaB7.undo()
    val replicaA8        = replicaA7.applyRemoteOperation(op6.get)

    // (3)
    assertEquals(undoValues(replicaA8), List(1))
    assertEquals(redoAnchors(replicaA8), List(op3a.id))
    assertEquals(replicaA8.values(), List(2))
    assertEquals(undoValues(replicaB8), List(2))
    assertEquals(redoAnchors(replicaB8), List(op4.id, op3b.id))
    assertEquals(replicaB8.values(), List(2))

    // op_7_a (set) and op_7_b (undo)
    val (replicaA9, op7a) = replicaA8.set(6)
    val (replicaB9, op7b) = replicaB8.undo()
    val replicaB10        = replicaB9.applyRemoteOperation(op7a)
    val replicaA10        = replicaA9.applyRemoteOperation(op7b.get)

    // (4)
    assertEquals(undoValues(replicaA10), List(1, 6))
    assertEquals(redoAnchors(replicaA10), List())
    assertEquals(replicaA10.values(), List(1, 6))
    assertEquals(undoValues(replicaB10), List())
    assertEquals(redoAnchors(replicaB10), List(op4.id, op3b.id, op2.id))
    assertEquals(replicaB10.values(), List(1, 6))

    // op_8 (redo)
    val (replicaB11, op8) = replicaB10.redo()
    val replicaA11        = replicaA10.applyRemoteOperation(op8.get)

    // (5)
    assertEquals(undoValues(replicaA11), List(1, 6))
    assertEquals(redoAnchors(replicaA11), List())
    assertEquals(replicaA11.values(), List(2))
    assertEquals(undoValues(replicaB11), List(2))
    assertEquals(redoAnchors(replicaB11), List(op4.id, op3b.id))
    assertEquals(replicaB11.values(), List(2))

    // op_9 (redo)
    val (replicaB12, op9) = replicaB11.redo()
    val replicaA12        = replicaA11.applyRemoteOperation(op9.get)

    // (6)
    assertEquals(undoValues(replicaA12), List(1, 6))
    assertEquals(redoAnchors(replicaA12), List())
    assertEquals(replicaA12.values(), List(2, 4, 3))
    assertEquals(undoValues(replicaB12), List(2, 3))
    assertEquals(redoAnchors(replicaB12), List(op4.id))
    assertEquals(replicaB12.values(), List(2, 4, 3))

    // op_10 (redo)
    val (replicaB13, op10) = replicaB12.redo()
    val replicaA13         = replicaA12.applyRemoteOperation(op10.get)

    // (7)
    assertEquals(undoValues(replicaA13), List(1, 6))
    assertEquals(redoAnchors(replicaA13), List())
    assertEquals(replicaA13.values(), List(5))
    assertEquals(undoValues(replicaB13), List(2, 3, 5))
    assertEquals(redoAnchors(replicaB13), List())
    assertEquals(replicaB13.values(), List(5))
  }
}

def undoValues[T](register: UndoRedoOpBased.MVR[T]): List[T] = {
  register.undoStack
    .flatMap(_.ty.getValue)
    .reverse
}

def redoAnchors[T](register: UndoRedoOpBased.MVR[T]): List[Dot] = {
  register.redoStack
    .flatMap(_.ty.getAnchor)
    .reverse
}
