package text_rdt

import text_rdt.Helper.myAssert
import text_rdt.complexId

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

final case class ComplexFugueFactory(replicaId: RID) {

  var tree
      : mutable.HashMap[SimpleID | Null, mutable.ArrayBuffer[ComplexTreeNode]] =
    mutable.HashMap()
  private var counter = 0

  def addNodeToTree(node: ComplexTreeNodeSingle): Unit = {
    myAssert(node.index == node.complexTreeNode.offset)
    node.complexTreeNode.invariant()
    val _ = tree.updateWith(
      node.complexTreeNode.id
    ) {
      case None => Some(mutable.ArrayBuffer(node.complexTreeNode))
      case Some(value) =>
        if (Helper.ENABLE) {
          val duplicatedNode = value.find(n =>
            n.offset <= node.complexTreeNode.offset && node.complexTreeNode.offset < n.offset + n
              .valueSize()
              ||
              n.offset <= (node.complexTreeNode.offset + node.complexTreeNode
                .valueSize() - 1) && (node.complexTreeNode.offset + node.complexTreeNode
                .valueSize() - 1) < n.offset + n
                .valueSize()
          )
          myAssert(
            duplicatedNode.isEmpty,
            s"node $node is duplicated by $duplicatedNode"
          )
        }

        value.addOne(node.complexTreeNode)
        Some(value)
    }
  }

}

object ComplexFugueFactory {

  given complexFugueFactory: FugueFactory with {
    type ID = ComplexID
    type N = ComplexTreeNodeSingle
    type NC = ComplexTreeNodeSingle.complexTreeNodeSingle.type
    type F = ComplexFugueFactory
    type MSG = Message[ID]

    def create(replicaId: RID): ComplexFugueFactory = {
      ComplexFugueFactory(replicaId)
    }

    given canEqualNode: CanEqual[N, N] = ComplexTreeNodeSingle.canEqual

    given treeNodeContext: NC = ComplexTreeNodeSingle.complexTreeNodeSingle

    private case class StackEntry(
        side: Side,
        children: Iterator[ComplexTreeNode]
    )

    extension (factory: ComplexFugueFactory) {
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

      override def dupe(): ComplexFugueFactory = factory

      override def createRootNode(): ComplexTreeNodeSingle = {
        val rootTreeNode = ComplexTreeNodeSingle(
          ComplexTreeNode(
            null,
            0,
            Right(1),
            true,
            null,
            Side.Right,
            mutable.ArrayBuffer.empty,
            mutable.ArrayBuffer.empty
          ),
          0
        )
        factory.addNodeToTree(rootTreeNode)
        rootTreeNode
      }

      override def insert(
          idOrNull: ComplexID | Null,
          value: Char,
          parent: ComplexTreeNodeSingle,
          side: Side
      ): MSG = {
        val id = idOrNull match {
          case null =>
            factory.counter += 1
            ComplexID(factory.replicaId, factory.counter, 0)
          case idNotNull => idNotNull
        }

        if (
          side == Side.Right &&
          parent.complexTreeNode.valueSize() == parent.valueIndex() + 1
        ) {
          parent.complexTreeNode.values match {
            case Left(values) =>
              if (
                parent.complexTreeNode.allowAppend && parent
                  .side() == Side.Right && parent.complexTreeNode.rightChildrenBuffer.isEmpty && parent
                  .id() != null && parent.id().nn.rid == id.rid
              ) {
                myAssert(parent.side() == Side.Right)
                values.append(value)

                return Message.Insert(
                  ComplexID(
                    parent.complexTreeNode.id.nn.rid,
                    parent.complexTreeNode.id.nn.counter,
                    parent.index + 1
                  ),
                  value,
                  ComplexID(
                    parent.complexTreeNode.id.nn.rid,
                    parent.complexTreeNode.id.nn.counter,
                    parent.index
                  ),
                  side
                )
              }
            case default =>
          }

          val nodeToAppend = ComplexTreeNodeSingle(
            ComplexTreeNode(
              id.toSimpleID,
              id.offset,
              Left(mutable.ArrayBuffer(value)),
              true,
              parent,
              side,
              mutable.ArrayBuffer.empty,
              mutable.ArrayBuffer.empty
            ),
            id.offset
          )

          val buffer = parent.complexTreeNode.rightChildrenBuffer
          var i = buffer
            .indexWhere(rightSib => nodeToAppend.id().nn < rightSib.id().nn)
          if (i == -1) {
            i = buffer.length
          }
          buffer
            .insert(i, nodeToAppend)

          factory.addNodeToTree(nodeToAppend)

          return Message.Insert(
            ComplexID(
              nodeToAppend.complexTreeNode.id.nn.rid,
              nodeToAppend.complexTreeNode.id.nn.counter,
              nodeToAppend.index
            ),
            value,
            parent.complexTreeNode.id match {
              case null => null
              case id   => ComplexID(id.rid, id.counter, parent.index)
            },
            side
          )
        }

        if (
          side == Side.Left &&
          parent.valueIndex() == 0
        ) {
          val nodeToPrepend = ComplexTreeNodeSingle(
            ComplexTreeNode(
              id.toSimpleID,
              id.offset,
              Left(mutable.ArrayBuffer(value)),
              true,
              parent,
              side,
              mutable.ArrayBuffer.empty,
              mutable.ArrayBuffer.empty
            ),
            id.offset
          )

          val buffer = parent.complexTreeNode.leftChildrenBuffer
          var i = buffer
            .indexWhere(sib => nodeToPrepend.id().nn < sib.id().nn)
          if (i == -1) {
            i = buffer.length
          }
          buffer
            .insert(i, nodeToPrepend)

          factory.addNodeToTree(nodeToPrepend)
          return Message.Insert(
            ComplexID(
              nodeToPrepend.complexTreeNode.id.nn.rid,
              nodeToPrepend.complexTreeNode.id.nn.counter,
              nodeToPrepend.index
            ),
            value,
            ComplexID(
              parent.complexTreeNode.id.nn.rid,
              parent.complexTreeNode.id.nn.counter,
              parent.index
            ),
            side
          )
        }

        val splitIndex = side match {
          case text_rdt.Side.Left  => parent.valueIndex()
          case text_rdt.Side.Right => parent.valueIndex() + 1
        }
        val (nodeToSplit, nodeRightSplit) = split(parent, splitIndex)

        val nodeToInsert = ComplexTreeNodeSingle(
          ComplexTreeNode(
            id.toSimpleID,
            id.offset,
            Left(mutable.ArrayBuffer(value)),
            true,
            side match {
              case text_rdt.Side.Left  => nodeRightSplit
              case text_rdt.Side.Right => nodeToSplit
            },
            side,
            mutable.ArrayBuffer.empty,
            mutable.ArrayBuffer.empty
          ),
          id.offset
        )

        val bufferInsert = side match {
          case text_rdt.Side.Left =>
            nodeRightSplit.complexTreeNode.leftChildrenBuffer
          case text_rdt.Side.Right =>
            nodeToSplit.complexTreeNode.rightChildrenBuffer
        }
        var iInsert = bufferInsert
          .indexWhere(rightSib => nodeToInsert.id().nn < rightSib.id().nn)
        if (iInsert == -1) {
          iInsert = bufferInsert.length
        }
        bufferInsert
          .insert(iInsert, nodeToInsert)

        factory.addNodeToTree(nodeToInsert)
        Message.Insert(
          ComplexID(
            nodeToInsert.complexTreeNode.id.nn.rid,
            nodeToInsert.complexTreeNode.id.nn.counter,
            nodeToInsert.index
          ),
          value,
          ComplexID(
            parent.complexTreeNode.id.nn.rid,
            parent.complexTreeNode.id.nn.counter,
            parent.index
          ),
          side
        )
      }

      private def split(
          nodeToSplit: ComplexTreeNodeSingle,
          splitIndex: Int
      ): (ComplexTreeNodeSingle, ComplexTreeNodeSingle) = {
        nodeToSplit.complexTreeNode.values match {
          case Left(values) =>
            val (leftValues, rightValues) = values.splitAt(splitIndex)
            myAssert(leftValues.nonEmpty)
            myAssert(rightValues.nonEmpty)
            nodeToSplit.complexTreeNode.values = Left(leftValues)
            nodeToSplit.complexTreeNode.allowAppend = false

            val nodeRightSplit = ComplexTreeNodeSingle(
              ComplexTreeNode(
                nodeToSplit.complexTreeNode.id,
                nodeToSplit.complexTreeNode.offset + splitIndex,
                Left(rightValues),
                true,
                nodeToSplit,
                Side.Right,
                mutable.ArrayBuffer.empty,
                mutable.ArrayBuffer.empty
              ),
              nodeToSplit.complexTreeNode.offset + splitIndex
            )
            nodeRightSplit.complexTreeNode.rightChildrenBuffer =
              nodeToSplit.complexTreeNode.rightChildrenBuffer.map(c => {
                c.complexTreeNode.parent = nodeRightSplit
                c
              })
            nodeToSplit.complexTreeNode.rightChildrenBuffer =
              mutable.ArrayBuffer.empty

            val bufferRightSplit =
              nodeToSplit.complexTreeNode.rightChildrenBuffer
            var iRightSplit = bufferRightSplit
              .indexWhere(rightSib => nodeRightSplit.id().nn < rightSib.id().nn)
            if (iRightSplit == -1) {
              iRightSplit = bufferRightSplit.length
            }
            bufferRightSplit
              .insert(iRightSplit, nodeRightSplit)

            factory.addNodeToTree(nodeRightSplit)

            (nodeToSplit, nodeRightSplit)
          case Right(value) =>
            throw new IllegalStateException("splitting here is not allowed")
        }
      }

      override def delete(node: ComplexTreeNodeSingle): MSG = {
        if (node.complexTreeNode.values.isRight) {} else if (
          node.complexTreeNode.values.swap.toOption.get.length == 1
        ) {
          node.complexTreeNode.values = Right(1)
        } else if (node.valueIndex() == 0) {
          val (nodeToSplit, nodeRightSplit) = split(node, 1)
          nodeToSplit.complexTreeNode.values = Right(1)
        } else if (node.complexTreeNode.valueSize() == node.valueIndex() + 1) {
          val (nodeToSplit, nodeRightSplit) = split(node, node.valueIndex())
          nodeRightSplit.complexTreeNode.values = Right(1)
        } else {
          myAssert(node.complexTreeNode.valueSize() >= 3)
          val (firstNode, nodeRightSplit) = split(node, node.valueIndex())
          val (secondNode, thirdNode) =
            split(nodeRightSplit, 1)
          secondNode.complexTreeNode.values = Right(1)
        }

        Message.Delete(
          ComplexID(
            node.complexTreeNode.id.nn.rid,
            node.complexTreeNode.id.nn.counter,
            node.index
          )
        )
      }

      override def get(id: ComplexID | Null): ComplexTreeNodeSingle = {
        val complexTreeNode =
          factory
            .tree(id match {
              case null  => null
              case value => SimpleID(value.rid, value.counter)
            })
            .find(n =>
              id match {
                case null => n.id == null
                case nodeId =>
                  n.offset <= nodeId.offset && nodeId.offset < n.offset + n
                    .valueSize()
              }
            )
            .get
        ComplexTreeNodeSingle(
          complexTreeNode,
          id match {
            case null  => 0
            case value => value.offset
          }
        )
      }

      @tailrec
      private def nodesRecursive(
          node: ComplexTreeNode,
          stack: mutable.Stack[StackEntry]
      ): Option[(Iterator[Char], ComplexTreeNode)] = {
        val top = stack.pop()
        if (top.children.isEmpty) {
          top.side match {
            case text_rdt.Side.Left =>
              stack.push(
                StackEntry(
                  Side.Right,
                  node.rightChildrenBuffer.iterator.map(v => v.complexTreeNode)
                )
              )
              node.values match {
                case Left(values) =>
                  Some((values.iterator, node))
                case Right(value) =>
                  nodesRecursive(node, stack)
              }
            case text_rdt.Side.Right =>
              node.parent match {
                case null => None
                case value =>
                  nodesRecursive(value.complexTreeNode, stack)
              }
          }
        } else {
          val child = top.children.next()
          stack.push(top)
          stack.push(
            StackEntry(
              Side.Left,
              child.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
            )
          )
          nodesRecursive(child, stack)
        }
      }

      override def textWithDeleted(): Vector[Either[Char, Char]] = ???

      override def text(): String = {
        val root = get(null).complexTreeNode
        val stack = mutable.Stack(
          StackEntry(
            Side.Left,
            root.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
          )
        )
        Iterable
          .unfold(root)(node => {
            nodesRecursive(node, stack)
          })
          .flatten
          .mkString
      }

      @tailrec
      private def findElementAtIndex(
          index: Int,
          node: ComplexTreeNode,
          stack: mutable.Stack[StackEntry]
      ): N = {
        val top = stack.pop()
        if (top.children.isEmpty) {
          top.side match {
            case text_rdt.Side.Left =>
              var newIndex: Int = -1
              node.values match {
                case Left(values) =>
                  if (index < values.length) {
                    return ComplexTreeNodeSingle(node, index + node.offset)
                  } else {
                    newIndex = index - values.length
                  }
                case Right(value) =>
                  newIndex = index
              }
              stack.push(
                StackEntry(
                  Side.Right,
                  node.rightChildrenBuffer.iterator.map(v => v.complexTreeNode)
                )
              )
              findElementAtIndex(newIndex, node, stack)
            case text_rdt.Side.Right =>
              node.parent match {
                case null =>
                  throw new IllegalArgumentException("index does not exist")
                case value =>
                  findElementAtIndex(index, value.complexTreeNode, stack)
              }
          }
        } else {
          val child = top.children.next()
          stack.push(top)
          stack.push(
            StackEntry(
              Side.Left,
              child.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
            )
          )
          findElementAtIndex(index, child, stack)
        }
      }

      override def atVisibleIndex(i: Int): complexFugueFactory.this.N = {
        val root = get(null).complexTreeNode
        val stack = mutable.Stack(
          StackEntry(
            Side.Left,
            root.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
          )
        )
        findElementAtIndex(i, root, stack)
      }

      @tailrec
      private def calculateVisibleIndexOf(
          nodeToFind: ComplexTreeNodeSingle,
          index: Int,
          node: ComplexTreeNode,
          stack: mutable.Stack[StackEntry]
      ): Int = {
        val top = stack.pop()
        if (top.children.isEmpty) {
          top.side match {
            case text_rdt.Side.Left =>
              if (node.eq(nodeToFind.complexTreeNode)) {
                return index + nodeToFind.visibleValueIndex()
              }
              stack.push(
                StackEntry(
                  Side.Right,
                  node.rightChildrenBuffer.iterator.map(v => v.complexTreeNode)
                )
              )
              calculateVisibleIndexOf(
                nodeToFind,
                index + node.visibleValueSize(),
                node,
                stack
              )
            case text_rdt.Side.Right =>
              node.parent match {
                case null =>
                  throw new IllegalArgumentException("index does not exist")
                case value =>
                  calculateVisibleIndexOf(
                    nodeToFind,
                    index,
                    value.complexTreeNode,
                    stack
                  )
              }
          }
        } else {
          val child = top.children.next()
          stack.push(top)
          stack.push(
            StackEntry(
              Side.Left,
              child.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
            )
          )
          calculateVisibleIndexOf(nodeToFind, index, child, stack)
        }
      }

      override def visibleIndexOf(node: ComplexTreeNodeSingle): Int = {
        val root = get(null).complexTreeNode
        val stack = mutable.Stack(
          StackEntry(
            Side.Left,
            root.leftChildrenBuffer.iterator.map(v => v.complexTreeNode)
          )
        )
        calculateVisibleIndexOf(node, 0, root, stack)
      }
    }
  }
}
