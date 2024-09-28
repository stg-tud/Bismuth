package text_rdt

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

object SimpleFugueFactory {
  given simpleFugueFactory: FugueFactory with {
    type ID = SimpleID
    type N = SimpleTreeNode
    type NC = simpleTreeNode.type
    type F = SimpleFugueFactory
    type MSG = Message[ID]

    def create(replicaId: RID): SimpleFugueFactory = {
      SimpleFugueFactory(replicaId)
    }

    given treeNodeContext: NC = simpleTreeNode

    private case class StackEntry(
        side: Side,
        children: Iterator[N]
    )

    extension (factory: SimpleFugueFactory) {
      def insert(i: Int, x: Char): MSG = {
        val leftOrigin =
          if (i == 0) {
            get(null)
          } else {
            factory.atVisibleIndex(i - 1)
          }
        val firstRightChild = leftOrigin.firstRightChild()
        val side = if (firstRightChild == null) {
          Side.Right
        } else {
          Side.Left
        }
        val origin =
          if (firstRightChild == null) {
            leftOrigin
          } else {
            firstRightChild.leftmostDescendant()
          }

        factory.insert(
          null,
          x,
          origin,
          side
        )
      }

      def handleRemoteMessage(message: MSG, editor: Editory): Unit = {
        message match {
          case Message.Insert(id, value, parent, side) =>
            deliveringRemoteInsert(editor, id, value, parent, side)
          case Message.Delete(id) => deliveringRemoteDelete(editor, id)
        }
      }

      private def deliveringRemoteDelete(editor: Editory, id: ID): Unit = {
        val deleted = deliveringLocalDelete(id)
        if (deleted) {
          val index = visibleIndexOf(get(id))
          editor.delete(index)
        }
      }

      private def deliveringRemoteInsert(
          editor: Editory,
          id: ID,
          value: Char,
          parent: ID | Null,
          side: Side
      ): Unit = {
        deliveringLocalInsert(id, value, parent, side)
        val index = visibleIndexOf(get(id))
        editor.insert(index, value)
      }

      def deliveringLocalInsert(
          id: ID,
          value: Char,
          parent: ID | Null,
          side: Side
      ): Unit = {
        val parentTreeNode = factory.get(parent)

        val _ = factory.insert(id, value, parentTreeNode, side)
      }

      def deliveringLocalDelete(id: ID): Boolean = {
        val treeNode = factory.get(
          id
        )
        val deleted = treeNode.value() != null
        factory.delete(treeNode)
        deleted
      }

      def values(): Iterable[Char] = {
        traverse(null)
      }

      private def nodes(): Iterable[N] = {
        nodes(null)
      }

      private def nodesWithTombstones(): Iterable[N] = {
        nodesWithTombstones(null)
      }

      @tailrec
      private def tryProduce(
          node: N,
          stack: mutable.Stack[StackEntry]
      ): Option[(N, N)] = {
        val top = stack.pop()
        if (top.children.isEmpty) {
          top.side match {
            case text_rdt.Side.Left =>
              stack.push(
                StackEntry(
                  Side.Right,
                  node.rightChildren()
                )
              )
              Some((node, node))
            case text_rdt.Side.Right =>
              node.parent() match {
                case null => None
                case value =>
                  tryProduce(value, stack)
              }
          }
        } else {
          val child = top.children.next()
          stack.push(top)
          stack.push(StackEntry(Side.Left, child.leftChildren()))
          tryProduce(child, stack)
        }
      }

      private def nodesWithTombstones(
          start: N
      ): Iterable[N] = {
        val stack = mutable.Stack(StackEntry(Side.Left, start.leftChildren()))
        Iterable.unfold(start)(node => {
          tryProduce(node, stack)
        })
      }

      @targetName("nodesWithTombstones_id")
      private def nodesWithTombstones(
          nodeID: ID | Null
      ): Iterable[N] = {
        nodesWithTombstones(factory.get(nodeID))
      }

      private def nodes(node: N): Iterable[N] = {
        nodesWithTombstones(node).filter(node => node.value() != null)
      }

      @targetName("nodes_id")
      private def nodes(nodeID: ID | Null): Iterable[N] = {
        nodes(factory.get(nodeID))
      }

      @targetName("traverse_id")
      private def traverse(nodeID: ID | Null): Iterable[Char] = {
        nodes(nodeID).map(node => node.value().nn)
      }

      override def atVisibleIndex(i: Int): SimpleTreeNode = {
        val vals = factory.nodes().drop(i)
        vals.iterator.next
      }

      override def textWithDeleted(): Vector[Either[Char, Char]] = ???

      override def text(): String = values().mkString
      override def visibleIndexOf(node: N): Int = {
        nodesWithTombstones()
          .takeWhile(value => value.id() != node.id())
          .count(node => node.value() != null)
      }

      override def dupe(): SimpleFugueFactory = factory

      override def createRootNode(): SimpleTreeNode = {
        val node = SimpleTreeNode(
          Node(
            null,
            null,
            null,
            Side.Right
          ),
          mutable.ArrayBuffer.empty,
          mutable.ArrayBuffer.empty
        )
        factory.tree ++= Seq(
          (
            node.id(),
            node
          )
        )
        node
      }

      override def insert(
          idOrNull: SimpleID | Null,
          value: Char,
          parent: SimpleTreeNode,
          side: Side
      ): MSG = {
        val id = idOrNull match {
          case null =>
            factory.counter += 1
            SimpleID(factory.replicaId, factory.counter)
          case idNotNull => idNotNull
        }

        val node = SimpleTreeNode(
          Node(
            id,
            value,
            parent,
            side
          ),
          mutable.ArrayBuffer.empty,
          mutable.ArrayBuffer.empty
        )
        val _ = factory.tree.put(node.id(), node)

        if (node.side() == Side.Right) {
          var i = parent
            .rightChildren()
            .indexWhere(rightSib => node.id().nn < rightSib.id().nn)
          if (i == -1) {
            i = parent.rightChildren().length
          }
          parent.rightChildrenBuffer.insert(i, node)
        } else {
          var i =
            parent
              .leftChildren()
              .indexWhere(leftSib => node.id().nn < leftSib.id().nn)
          if (i == -1) {
            i = parent.leftChildren().length
          }
          parent.leftChildrenBuffer.insert(i, node)
        }
        Message.Insert(node.node.id.nn, value, parent.node.id, side)
      }

      override def delete(node: SimpleTreeNode): MSG = {
        node.node = node.node.copy(value = null)
        Message.Delete(node.node.id.nn)
      }

      override def get(id: SimpleID | Null): SimpleTreeNode = {
        factory.tree(id)
      }
    }

  }
}

final case class SimpleFugueFactory(replicaId: RID) {
  var tree: mutable.HashMap[SimpleID | Null, SimpleTreeNode] =
    mutable.HashMap()
  private var counter = 0
}
