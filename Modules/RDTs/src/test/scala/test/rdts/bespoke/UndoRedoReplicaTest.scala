package test.rdts.bespoke

import rdts.experiments.UndoRedoReplica
import rdts.base.{LocalUid, Lattice, Bottom}

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

    val aid     = LocalUid.predefined("a")
    val replica = UndoRedoReplica.empty[State]

    replica.mod(_.setValue(1))(using aid)
    assertEquals(replica.state, State(value = 1))

    replica.mod(_.setValue(2))(using aid)
    assertEquals(replica.state, State(value = 2))

    replica.undo()
    assertEquals(replica.state, State(value = 1))

    replica.undo()
    assertEquals(replica.state, State.bottom.empty)

    replica.redo()(using aid)
    assertEquals(replica.state, State(value = 1))

    replica.redo()(using aid)
    assertEquals(replica.state, State(value = 2))
  }

  test("drawing example") {
    import rdts.datatypes.{LastWriterWins as LWW, ObserveRemoveMap}

    type ID = String

    case class Document(nodes: ObserveRemoveMap[ID, Node] = ObserveRemoveMap.empty):
        def materialized_nodes: Map[ID, MaterializedNode] =
          nodes.inner.view.mapValues(post => MaterializedNode.from(post.value)).toMap

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

    case class MaterializedNode(
        position: Position,
        color: Color,
        kind: NodeKind
    )

    object MaterializedNode {
      def from(node: Node): MaterializedNode =
        MaterializedNode(
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

    val aid      = LocalUid.predefined("A")
    val bid      = LocalUid.predefined("B")
    val replica1 = UndoRedoReplica.empty[Document]
    val replica2 = UndoRedoReplica.empty[Document]

    val node1 = "node1"
    val node2 = "node2"

    val d1a = replica1.mod(_.add(
      node1,
      Node(LWW.now(Position(0, 0)), LWW.now(Color.Red), LWW.now(NodeKind.Rectangle(width = 50.0, height = 100.0)))
    ))(using aid)
    val d1b = replica2.mod(_.add(
      node2,
      Node(LWW.now(Position(100, 100)), LWW.now(Color.Blue), LWW.now(NodeKind.Circle(radius = 25.0)))
    ))(using bid)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(node1 -> MaterializedNode(
        position = Position(0, 0),
        color = Color.Red,
        kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
      ))
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(node2 -> MaterializedNode(
        position = Position(100, 100),
        color = Color.Blue,
        kind = NodeKind.Circle(radius = 25.0)
      ))
    )

    replica1.receive(d1b)
    replica2.receive(d1a)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    val d2a = replica1.mod(_.setColor(node2, Color.Green))(using aid)
    val d2b = replica2.mod(_.setPosition(node1, Position(50, 50)))(using bid)
    replica1.receive(d2b)
    replica2.receive(d2a)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Green,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Green,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Undo should reset the position of node1 to (0, 0) and the color of node2 to Blue
    val d3a = replica1.undo()
    val d3b = replica2.undo()
    replica1.receive(d3b)
    replica2.receive(d3a)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(0, 0),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Undo on replica1 should remove node1
    val d4a = replica1.undo()
    replica2.receive(d4a)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Redo should reset the position of node1 to (50, 50)
    val d5b = replica2.redo()(using bid)
    replica1.receive(d5b)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )

    // Redo should be a no-op, since there is nothing on the undo stack
    val d6b = replica2.redo()(using bid)
    replica1.receive(d6b)

    assertEquals(
      replica1.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
    assertEquals(
      replica2.state.materialized_nodes,
      Map(
        node1 -> MaterializedNode(
          position = Position(50, 50),
          color = Color.Red,
          kind = NodeKind.Rectangle(width = 50.0, height = 100.0)
        ),
        node2 -> MaterializedNode(
          position = Position(100, 100),
          color = Color.Blue,
          kind = NodeKind.Circle(radius = 25.0)
        )
      )
    )
  }
}
