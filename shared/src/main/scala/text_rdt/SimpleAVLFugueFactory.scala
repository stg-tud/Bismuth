package text_rdt

import text_rdt.avl.{AVLTree, AVLTreeNode, AVLTreeNodeValueSize}
import text_rdt.avl2.{AVL2Tree, AVL2TreeNode}

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import text_rdt.canEqualNullableNull
import text_rdt.canEqualNullNullable

given simpleAVLTreeNodeSize: AVLTreeNodeValueSize[SimpleAVLTreeNode] with {
  extension (node: SimpleAVLTreeNode) {
    def size: Int = {
      node.value match {
        case null  => 0
        case value => 1
      }
    }
  }
}

object SimpleAVLFugueFactory {

  given simpleAVLFugueFactory: FugueFactory with {
    type ID = SimpleID
    type N = AVLTreeNode[SimpleAVLTreeNode]
    type NC = SimpleAVLTreeNode.simpleAVLTreeNode.type
    type F = SimpleAVLFugueFactory
    type MSG = Message[ID]

    def create(replicaId: RID): SimpleAVLFugueFactory = {
      SimpleAVLFugueFactory(replicaId)
    }

    given treeNodeContext: NC = SimpleAVLTreeNode.simpleAVLTreeNode

    given canEqualNode: CanEqual[N, N] =
      AVLTreeNode.canEqual[SimpleAVLTreeNode]

    extension (factory: SimpleAVLFugueFactory) {
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

      private def deliveringLocal(msg: Message[ID]): Unit = {
        msg match {
          case Message.Insert(id, value, parent, side) =>
            deliveringLocalInsert(id, value, parent, side)
          case Message.Delete(id) => val _ = deliveringLocalDelete(id)
        }
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

      override def atVisibleIndex(i: Int): AVLTreeNode[SimpleAVLTreeNode] = {
        val result = factory.avlTree.nodeAtIndex(i)._1.nn
        result
      }

      override def text(): String =
        factory.avlTree.values().filter(_.value != null).map(_.value).mkString

      override def visibleIndexOf(node: N): Int = {
        val result = node.indexOfNode()
        result
      }

      override def dupe(): SimpleAVLFugueFactory = factory

      override def createRootNode(): AVLTreeNode[SimpleAVLTreeNode] = {
        val node = AVLTreeNode[text_rdt.SimpleAVLTreeNode](self => {
          val leftmostDescendantCacheRoot =
            AVL2Tree[AVLTreeNode[SimpleAVLTreeNode]](null, self)
          val leftmostDescendantCacheNode =
            AVL2TreeNode(0, leftmostDescendantCacheRoot)
          leftmostDescendantCacheRoot.insert(
            null,
            leftmostDescendantCacheNode,
            Side.Right
          )
          val rightmostDescendantCacheRoot =
            AVL2Tree[AVLTreeNode[SimpleAVLTreeNode]](null, self)
          val rightmostDescendantCacheNode =
            AVL2TreeNode(0, rightmostDescendantCacheRoot)
          rightmostDescendantCacheRoot.insert(
            null,
            rightmostDescendantCacheNode,
            Side.Right
          )

          SimpleAVLTreeNode(
            null,
            null,
            null,
            Side.Right,
            null,
            null,
            0,
            leftmostDescendantCacheNode,
            rightmostDescendantCacheNode
          )
        })(using
          simpleAVLTreeNodeSize
            .asInstanceOf[AVLTreeNodeValueSize[SimpleAVLTreeNode]]
        )

        factory.tree ++= Seq(
          (
            node.id(),
            node
          )
        )
        factory.avlTree.insert(null, node, Side.Right)
        node
      }

      override def insert(
          idOrNull: SimpleID | Null,
          value: Char,
          parent: AVLTreeNode[SimpleAVLTreeNode],
          side: Side
      ): MSG = {
        val id = idOrNull match {
          case null =>
            factory.counter += 1
            SimpleID(factory.replicaId, factory.counter)
          case idNotNull => idNotNull
        }

        val node = AVLTreeNode[SimpleAVLTreeNode](self =>
          SimpleAVLTreeNode(
            id,
            value,
            parent,
            side,
            null,
            null,
            parent.value.depth + 1,
            null.asInstanceOf[AVL2TreeNode[AVLTreeNode[
              SimpleAVLTreeNode
            ]]],
            null.asInstanceOf[AVL2TreeNode[AVLTreeNode[
              SimpleAVLTreeNode
            ]]]
          ),
        )(using
          simpleAVLTreeNodeSize
            .asInstanceOf[AVLTreeNodeValueSize[SimpleAVLTreeNode]]
        )
        val _ = factory.tree.put(node.id(), node)

        if (side == Side.Right) {
          var i = parent
            .rightChildren()
            .indexWhere(rightSib => id < rightSib.id().nn)
          if (i == -1) {
            i = parent.rightChildren().length
          }
          if (
            i == 0 || parent.value.rightChildrenBuffer == null || parent.value.rightChildrenBuffer.nn.isEmpty
          ) {
            factory.avlTree.insertBasedOn(parent, node, Side.Right)
          } else {
            val target = parent.value.rightChildrenBuffer.nn(i - 1)
            factory.avlTree.insertBasedOn(
              target
                .rightmostDescendant(),
              node,
              Side.Right
            )
          }
          node.value.leftmostDescendantCache = DescendantCacheHelper.empty(node)
          node.value.rightmostDescendantCache = DescendantCacheHelper.append(
            parent.value.rightmostDescendantCache,
            i == parent
              .rightChildren()
              .length,
            node,
            1
          )
          parent.value.rightChildrenBuffer match {
            case null =>
              parent.value.rightChildrenBuffer =
                new mutable.ArrayBuffer(1).nn.append(node)
            case value => value.insert(i, node)
          }
          if (Helper.ENABLE) {
            val _ = parent.rightmostDescendant()
            val _ = parent.leftmostDescendant()
            val _ = node.leftmostDescendant()
            val _ = node.rightmostDescendant()
            parent.value.rightChildrenBuffer.nn.foreach(_.leftmostDescendant())
            parent.value.rightChildrenBuffer.nn.foreach(_.rightmostDescendant())
          }
        } else {
          var i =
            parent
              .leftChildren()
              .indexWhere(leftSib => id < leftSib.id().nn)
          if (i == -1) {
            i = parent.leftChildren().length
          }
          if (
            (parent.value.leftChildrenBuffer != null && i == parent.value.leftChildrenBuffer.nn.size) || parent.value.leftChildrenBuffer == null || parent.value.leftChildrenBuffer.nn.isEmpty
          ) {
            factory.avlTree.insertBasedOn(parent, node, Side.Left)
          } else {
            val target = parent.value.leftChildrenBuffer.nn(i)
            factory.avlTree.insertBasedOn(
              target
                .leftmostDescendant(),
              node,
              Side.Left
            )
          }
          node.value.rightmostDescendantCache =
            DescendantCacheHelper.empty(node)
          node.value.leftmostDescendantCache = DescendantCacheHelper.append(
            parent.value.leftmostDescendantCache,
            i == 0,
            node,
            1
          )
          parent.value.leftChildrenBuffer match {
            case null =>
              parent.value.leftChildrenBuffer =
                new mutable.ArrayBuffer(1).nn.append(node)
            case value => value.insert(i, node)
          }
          if (Helper.ENABLE) {
            val _ = parent.rightmostDescendant()
            val _ = parent.leftmostDescendant()
            val _ = node.leftmostDescendant()
            val _ = node.rightmostDescendant()
            parent.value.leftChildrenBuffer.nn.foreach(_.leftmostDescendant())
            parent.value.leftChildrenBuffer.nn.foreach(_.rightmostDescendant())
          }
        }
        Message.Insert(node.value.id.nn, value, parent.value.id, side)
      }

      override def delete(node: AVLTreeNode[SimpleAVLTreeNode]): MSG = {
        if (node.value.value != null) {
          node.value.value = null
          node.fixSizeRecursively(-1)
        }
        Message.Delete(node.value.id.nn)
      }

      override def get(
          id: SimpleID | Null
      ): AVLTreeNode[SimpleAVLTreeNode] = {
        factory.tree(id)
      }
    }
  }
}

final case class SimpleAVLFugueFactory(replicaId: RID) {
  val avlTree: AVLTree[SimpleAVLTreeNode] =
    AVLTree(null)(using
      simpleAVLTreeNodeSize
        .asInstanceOf[AVLTreeNodeValueSize[SimpleAVLTreeNode]]
    )
  var tree: mutable.HashMap[SimpleID | Null, AVLTreeNode[SimpleAVLTreeNode]] =
    mutable.HashMap()
  private var counter: Int = 0
}
