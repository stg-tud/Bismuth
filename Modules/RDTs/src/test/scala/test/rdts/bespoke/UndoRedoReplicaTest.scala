package test.rdts.bespoke

import rdts.experiments.UndoRedoReplica
import rdts.base.{Uid, LocalUid, Lattice, Bottom}

object UndoRedoReplicaTest:
  def createReplicas[A](n: Int): Array[UndoRedoReplica[A]] = {
    (1 to n).map(i => Uid.predefined(s"R$i")).map(uid =>
      UndoRedoReplica.empty(using LocalUid(uid))
    ).toArray
  }

class UndoRedoReplicaTest extends munit.FunSuite {
  test("simple undo redo") {
    case class State(value: Int) {
      def setValue(v: Int): State = this.copy(value = v)
    }

    object State {
      given lattice: Lattice[State] with
        def merge(a: State, b: State): State = State(value = math.max(a.value, b.value))

      given bottom: Bottom[State] = Bottom.provide(State(value = 0))
    }

    val Array(replica) = UndoRedoReplicaTest.createReplicas[State](1)

    replica.mod(_.setValue(1))
    assertEquals(replica.state, State(value = 1))

    replica.mod(_.setValue(2))
    assertEquals(replica.state, State(value = 2))

    replica.undo()
    assertEquals(replica.state, State(value = 1))

    replica.undo()
    assertEquals(replica.state, State.bottom.empty)

    replica.redo()
    assertEquals(replica.state, State(value = 1))

    replica.redo()
    assertEquals(replica.state, State(value = 2))
  }

  test("drawing example") {
    import rdts.datatypes.{ReplicatedList, GrowOnlyCounter as Counter, LastWriterWins as LWW, ObserveRemoveMap}

    type ID = String

    case class Document(nodes: ObserveRemoveMap[ID, Node] = ObserveRemoveMap.empty):
      def node_views: Map[ID, NodeView] =
        nodes.inner.view.mapValues(post => NodeView.from(post.value)).toMap

      def add(nodeId: ID, node: Node)(using replicaId: LocalUid): Document =
        Document(nodes.update(nodeId, node))

      def setPosition(nodeId: ID, position: Position)(using replicaId: LocalUid): Document =
        nodes.get(nodeId) match {
          case Some(n) => Document(nodes.update(nodeId, n.copy(position = LWW.now(position))))
          case None    => Document.bottom.empty
        }

      def setColor(nodeId: ID, color: Color)(using replicaId: LocalUid): Document =
        nodes.get(nodeId) match {
          case Some(n) => Document(nodes.update(nodeId, n.copy(color = LWW.now(color))))
          case None    => Document.bottom.empty
        }

    enum Color:
      case Red, Green, Blue, Yellow, Black, White

    case class Position(x: Double, y: Double)

    case class Node(
        position: LWW[Position],
        color: LWW[Color],
        kind: LWW[NodeKind]
    )

    enum NodeKind:
      case Circle(radius: Double)
      case Rectangle(width: Double, height: Double)

    case class NodeView(
        position: Position,
        color: Color,
        kind: NodeKind
    )

    object NodeView {
      def from(node: Node): NodeView =
        NodeView(
          position = node.position.value,
          color = node.color.value,
          kind = node.kind.value
        )
    }

    object Document {
      given lattice: Lattice[Document] = Lattice.derived
      given bottom: Bottom[Document]   = Bottom.derived
    }

    object Node {
      given lattice: Lattice[Node] = Lattice.derived
      given bottom: Bottom[Node]   = Bottom.derived
    }

    object NodeKind {
      given bottom: Bottom[NodeKind] = Bottom.provide(NodeKind.Rectangle(0.0, 0.0))
    }

    object Position {
      given bottom: Bottom[Position] = Bottom.provide(Position(0.0, 0.0))
    }

    object Color {
      given bottom: Bottom[Color] = Bottom.provide(Color.White)
    }

    val Array(replica1, replica2) = UndoRedoReplicaTest.createReplicas[Document](2)

    val node1 = "node1"
    val node2 = "node2"

    val delta1 = replica1.mod(_.add(
      node1,
      Node(LWW.now(Position(0, 0)), LWW.now(Color.Red), LWW.now(NodeKind.Rectangle(width = 50.0, height = 100.0)))
    ))
    val delta2 = replica2.mod(_.add(
      node2,
      Node(LWW.now(Position(100, 100)), LWW.now(Color.Blue), LWW.now(NodeKind.Circle(radius = 25.0)))
    ))

    assertEquals(
      replica1.state.node_views,
      Map(node1 -> NodeView(
        position = Position(0, 0),
        color = Color.Red,
        kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
      ))
    )
    assertEquals(
      replica2.state.node_views,
      Map(node2 -> NodeView(
        position = Position(100, 100),
        color = Color.Blue,
        kind = NodeKind.Circle(radius = 25.0)
      ))
    )

    replica1.receive(delta2)
    replica2.receive(delta1)

    assertEquals(
      replica1.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    val delta3 = replica1.mod(_.setColor(node2, Color.Green))
    val delta4 = replica2.mod(_.setPosition(node1, Position(50, 50)))
    replica1.receive(delta4)
    replica2.receive(delta3)

    assertEquals(
      replica1.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Green,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Green,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    val delta5 = replica1.undo()
    val delta6 = replica2.undo()
    replica1.receive(delta6)
    replica2.receive(delta5)

    // Undo should reset the position of node1 to (0, 0) and the color of node2 to Blue
    assertEquals(
      replica1.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Undo on replica1 should remove node1
    val delta7 = replica1.undo()
    replica2.receive(delta7)
    assertEquals(
      replica1.state.node_views,
      Map(
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Redo should reset the position of node1 to (50, 50)
    val delta8 = replica2.redo()
    replica1.receive(delta8)
    assertEquals(
      replica1.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Redo should be a no-op, since there is nothing on the undo stack
    val delta9 = replica2.redo()
    replica1.receive(delta9)
    assertEquals(
      replica1.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.node_views,
      Map(
        node1 -> NodeView(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> NodeView(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
  }
}
