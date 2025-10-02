package test.rdts.bespoke

import rdts.experiments.UndoRedoDeltaBased
import rdts.base.Uid
import rdts.time.Dot
import rdts.base.LocalUid
import rdts.base.Lattice
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.ReplicatedSet

def createTestReplicas[A](n: Int): Array[UndoRedoDeltaBased.Replica[A]] = {
  (1 to n).map(i => Uid.predefined(s"R$i")).map(uid =>
    UndoRedoDeltaBased.Replica.empty(using LocalUid(uid))
  ).toArray
}

class UndoRedoDeltaBasedMVRTest extends munit.FunSuite {
  test("single set operation") {
    val Array(replica) = createTestReplicas[Int](1)
    replica.set(42)
    assertEquals(replica.state.values(), List(42))
  }

  test("sequential set operations same replica") {
    val Array(replica) = createTestReplicas[Int](1)

    replica.set(1)
    assertEquals(replica.state.heads(), List(Dot(replica.id, 0)))

    replica.set(2)
    assertEquals(replica.state.heads(), List(Dot(replica.id, 1)))
    assertEquals(replica.state.values(), List(2))
  }

  test("concurrent set operations") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    // Concurrent sets on different replicas
    val delta1 = replica1.set(1)
    val delta2 = replica2.set(2)

    replica2.receive(delta1)
    replica1.receive(delta2)

    // Both replicas should see both values as siblings
    assertEquals(replica1.state.values().toSet, Set(1, 2))
    assertEquals(replica2.state.values().toSet, Set(1, 2))
  }

  test("three way concurrent sets") {
    val Array(replica1, replica2, replica3) = createTestReplicas[Int](3)

    val delta1 = replica1.set(1)
    val delta2 = replica2.set(2)
    val delta3 = replica3.set(3)

    replica1.receive(delta2).receive(delta3)
    replica2.receive(delta1).receive(delta3)
    replica3.receive(delta1).receive(delta2)

    assertEquals(replica1.state.values().toSet, Set(1, 2, 3))
    assertEquals(replica2.state.values().toSet, Set(1, 2, 3))
    assertEquals(replica3.state.values().toSet, Set(1, 2, 3))
  }

  test("causal dependency resolution") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    // Sequential: op1 → op2
    val delta1 = replica1.set(1)
    replica2.receive(delta1)

    assertEquals(replica1.state.values(), List(1))
    assertEquals(replica2.state.values(), List(1))

    val delta2    = replica2.set(2)
    val replica1b = replica1.receive(delta2)

    assertEquals(replica1.state.values(), List(2))
    assertEquals(replica2.state.values(), List(2))
  }

  test("delete operation") {
    val Array(replica) = createTestReplicas[Int](1)

    replica.set(42)
    replica.delete()

    assertEquals(replica.state.values(), List())
  }

  test("delete with concurrent set") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    val delta1 = replica1.set(1)
    replica2.receive(delta1)

    val delta2a = replica1.delete()
    val delta2b = replica2.set(2)

    replica1.receive(delta2b)
    replica2.receive(delta2a)

    assertEquals(replica1.state.values(), List(2))
    assertEquals(replica2.state.values(), List(2))
  }

  test("concurrent deletes") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    val deltaInit = replica1.set(1)
    replica2.receive(deltaInit)

    val deltaDelete1 = replica1.delete()
    val deltaDelete2 = replica2.delete()
    replica1.receive(deltaDelete2)
    replica2.receive(deltaDelete1)

    assertEquals(replica1.state.values(), List())
    assertEquals(replica2.state.values(), List())
  }

  test("apply same operation twice") {
    val Array(replica) = createTestReplicas[Int](1)

    val delta = replica.set(42)
    // Applying the same delta again should not change the state
    replica.receive(delta)

    assertEquals(replica.state.values(), List(42))
  }

  test("complex concurrent scenario (delta-based)") {
    val Array(replica1, replica2, replica3) = createTestReplicas[Int](3)

    val deltaInit = replica1.set(0)
    replica2.receive(deltaInit)
    replica3.receive(deltaInit)

    val delta1a = replica1.set(1)
    val delta1b = replica2.delete()
    val delta1c = replica3.set(3)

    replica1.receive(delta1b).receive(delta1c)
    replica2.receive(delta1a).receive(delta1c)
    replica3.receive(delta1a).receive(delta1b)

    assertEquals(replica1.state.values(), List(3, 1))
    assertEquals(replica2.state.values(), List(3, 1))
    assertEquals(replica3.state.values(), List(3, 1))
  }

  test("empty register operations") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    assert(replica1.state.values().isEmpty)

    val delta = replica1.delete()
    replica2.receive(delta)

    assert(replica1.state.values().isEmpty)
    assert(replica2.state.values().isEmpty)
  }

  test("heads tracking") {
    val Array(replica1, replica2) = createTestReplicas[Int](2)

    val delta1 = replica1.set(1)
    replica2.receive(delta1)

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 0)))
    assertEquals(replica2.state.heads(), List(Dot(replica1.id, 0)))

    val delta2 = replica2.set(1)
    replica1.receive(delta2)

    assertEquals(replica1.state.heads(), List(Dot(replica2.id, 0)))
    assertEquals(replica2.state.heads(), List(Dot(replica2.id, 0)))

    val delta3a = replica1.set(4)
    val delta3b = replica2.set(3)

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 1)))
    assertEquals(replica2.state.heads(), List(Dot(replica2.id, 1)))
    assertEquals(replica1.state.operation(Dot(replica1.id, 1)).get.predecessors, Set(Dot(replica2.id, 0)))
    assertEquals(replica2.state.operation(Dot(replica2.id, 1)).get.predecessors, Set(Dot(replica2.id, 0)))

    replica1.receive(delta3b)
    replica2.receive(delta3a)

    assertEquals(replica1.state.heads(), List(Dot(replica1.id, 1), Dot(replica2.id, 1)))
    assertEquals(replica2.state.heads(), List(Dot(replica1.id, 1), Dot(replica2.id, 1)))

    val delta4 = replica2.set(5)
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
    val Array(replica) = createTestReplicas[Int](1)

    replica.set(1)
    replica.delete()

    assertEquals(replica.state.values(), List())

    replica.undo()
    assertEquals(replica.state.values(), List(1))

    replica.redo()
    assertEquals(replica.state.values(), List())
  }

  // This test replicates the example from Figure 2 in the paper
  test("undo/redo paper example") {
    val Array(replicaA, replicaB) = createTestReplicas[Int](2)

    // op_1
    val delta1 = replicaA.set(1)
    replicaB.receive(delta1)

    // op_2
    val delta2 = replicaB.set(2)
    replicaA.receive(delta2)

    // op_3_a and op_3_b
    val delta3a = replicaA.set(4)
    val delta3b = replicaB.set(3)
    replicaA.receive(delta3b)
    replicaB.receive(delta3a)

    // op_4
    val delta4 = replicaB.set(5)
    replicaA.receive(delta4)

    // (1)
    assertEquals(replicaA.undoValues, List(1, 4))
    assertEquals(replicaA.redoAnchors, List())
    assertEquals(replicaA.state.values(), List(5))
    assertEquals(replicaB.undoValues, List(2, 3, 5))
    assertEquals(replicaB.redoAnchors, List())
    assertEquals(replicaB.state.values(), List(5))

    // op_5_a and op_5_b (undo)
    val delta5a = replicaA.undo()
    val delta5b = replicaB.undo()

    // (2a)
    assertEquals(replicaA.undoValues, List(1))
    assertEquals(replicaA.redoAnchors, List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(replicaB.undoValues, List(2, 3))
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(4, 3))

    // Exchange undo ops
    replicaA.receive(delta5b)
    replicaB.receive(delta5a)

    // (2b)
    assertEquals(replicaA.undoValues, List(1))
    assertEquals(replicaA.redoAnchors, List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2, 4, 3))
    assertEquals(replicaB.undoValues, List(2, 3))
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(2, 4, 3))

    // op_6 (undo)
    val delta6 = replicaB.undo()
    replicaA.receive(delta6)

    // (3)
    assertEquals(replicaA.undoValues, List(1))
    assertEquals(replicaA.redoAnchors, List(Dot(replicaA.id, 1)))
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(replicaB.undoValues, List(2))
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2), Dot(replicaB.id, 1)))
    assertEquals(replicaB.state.values(), List(2))

    // op_7_a (set) and op_7_b (undo)
    val delta7a = replicaA.set(6)
    val delta7b = replicaB.undo()
    replicaA.receive(delta7b)
    replicaB.receive(delta7a)

    // (4)
    assertEquals(replicaA.undoValues, List(1, 6))
    assertEquals(replicaA.redoAnchors, List())
    assertEquals(replicaA.state.values(), List(1, 6))
    assertEquals(replicaB.undoValues, List())
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2), Dot(replicaB.id, 1), Dot(replicaB.id, 0)))
    assertEquals(replicaB.state.values(), List(1, 6))

    // op_8 (redo)
    val delta8 = replicaB.redo()
    replicaA.receive(delta8)

    // (5)
    assertEquals(replicaA.undoValues, List(1, 6))
    assertEquals(replicaA.redoAnchors, List())
    assertEquals(replicaA.state.values(), List(2))
    assertEquals(replicaB.undoValues, List(2))
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2), Dot(replicaB.id, 1)))
    assertEquals(replicaB.state.values(), List(2))

    // op_9 (redo)
    val delta9 = replicaB.redo()
    replicaA.receive(delta9)

    // (6)
    assertEquals(replicaA.undoValues, List(1, 6))
    assertEquals(replicaA.redoAnchors, List())
    assertEquals(replicaA.state.values(), List(2, 4, 3))
    assertEquals(replicaB.undoValues, List(2, 3))
    assertEquals(replicaB.redoAnchors, List(Dot(replicaB.id, 2)))
    assertEquals(replicaB.state.values(), List(2, 4, 3))

    // op_10 (redo)
    val delta10 = replicaB.redo()
    replicaA.receive(delta10)

    // (7)
    assertEquals(replicaA.undoValues, List(1, 6))
    assertEquals(replicaA.redoAnchors, List())
    assertEquals(replicaA.state.values(), List(5))
    assertEquals(replicaB.undoValues, List(2, 3, 5))
    assertEquals(replicaB.redoAnchors, List())
    assertEquals(replicaB.state.values(), List(5))
  }

  // test("drawing example") {
  //   import rdts.experiments.UndoRedoDeltaBased.MVR
  //   import rdts.datatypes.ObserveRemoveMap

  //   case class Document(nodes: ObserveRemoveMap[NodeId, Node]) {
  //     type Delta = Document

  //     def insert(id: NodeId, node: Node)(using LocalUid): Delta = {
  //       Document(nodes = nodes.update(id, node))
  //     }

  //     def update(id: NodeId, f: Node => Node)(using LocalUid): Delta = {
  //       nodes.inner.get(id) match {
  //         case Some(existing) => Document(nodes = nodes.update(id, f(existing.value)))
  //         case None           => Document(nodes = ObserveRemoveMap.empty)
  //       }
  //     }
  //   }

  //   object Document {
  //     given Lattice[Document] = Lattice.derived

  //     def empty: Document = Document(ObserveRemoveMap.empty)
  //   }

  //   case class NodeId(id: String)

  //   case class Point(x: Float, y: Float)
  //   case class Size(width: Float, height: Float)

  //   enum Color:
  //     case Red
  //     case Green
  //     case Blue

  //   case class Node(
  //       position: MVR[Point] = MVR.empty,
  //       color: MVR[Color] = MVR.empty,
  //       kind: NodeKind,
  //   ) {
  //     def setPosition(p: Point)(using LocalUid): Node = this.copy(position = position.set(p))
  //     def setColor(c: Color)(using LocalUid): Node    = this.copy(color = color.set(c))
  //     def setKind(k: NodeKind)(using LocalUid): Node  = this.copy(kind = k)
  //   }

  //   enum NodeKind:
  //     case Rectangle(size: MVR[Size])
  //     case Circle(radius: MVR[Size])

  //   val replica1 = TestReplica(Uid.predefined("R1"), Document.empty)
  //   val replica2 = TestReplica(Uid.predefined("R2"), Document.empty)

  //   // Insert a rectangle on replica 1
  //   val delta1 = replica1.mod(_.insert(
  //     NodeId("n1"),
  //     Node(MVR.of(Point(0, 0)), MVR.of(Color.Red), NodeKind.Rectangle(MVR.of(Size(100, 100))))
  //   ))
  //   replica2.apply(delta1.anon)

  //   // Move the rectangle on replica 1
  //   val delta2 = replica1.mod(_.update(NodeId("n1"), n => n.setPosition(Point(50, 50))))
  //   replica2.apply(delta2.anon)

  //   // Change the rectangle color on replica 2
  //   val delta3 =
  //     replica2.mod(_.update(NodeId("n1"), n => n.setColor(Color.Green)))
  //   replica1.apply(delta3.anon)

  //   // Reset the position of the rectangle on replica 1
  //   val delta4 = replica1.undo()
  //   replica2.apply(delta4.anon)

  //   // // Reset the color of the rectangle on replica 2
  //   // val delta5 = replica2.undo()
  //   // replica1.apply(delta5.anon)
  // }
}
