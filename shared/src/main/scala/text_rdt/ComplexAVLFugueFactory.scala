package text_rdt

import text_rdt.Helper.myAssert
import text_rdt.avl.{AVLTreeNode, AVLTreeNodeValueSize}

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import text_rdt.avl.AVLTree
import text_rdt.avl2.AVL2TreeNode

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

given complexAVLTreeNodeSize: AVLTreeNodeValueSize[ComplexAVLTreeNode] with {
  extension (node: ComplexAVLTreeNode) {
    def size: Int = {
      node._values match {
        case null =>
          0
        case values =>
          node.to - node.offset + 1
      }
    }
  }
}

final case class ComplexAVLFugueFactory(replicaId: RID) {

  val avlTree: AVLTree[ComplexAVLTreeNode] =
    AVLTree(null)(using
      complexAVLTreeNodeSize
        .asInstanceOf[AVLTreeNodeValueSize[ComplexAVLTreeNode]]
    )
  var tree: mutable.HashMap[
    SimpleID | Null,
    (
        StringBuilder | Null,
        AVLTreeNode[
          ComplexAVLTreeNode
        ] |
          mutable.SortedSet[AVLTreeNode[
            ComplexAVLTreeNode
          ]]
    )
  ] =
    mutable.HashMap()
  private var counter = 0

  def addNode(
      node: AVLTreeNode[ComplexAVLTreeNode]
  ): Unit = {
    val _ = tree.updateWith(node.value.rid match {
      case null => null
      case rid  => SimpleID(rid, node.value.counter)
    })(oldValue => {
      oldValue match {
        case None => {
          Some(node.value._values, node)
        }
        case Some(buffer, oldNode: AVLTreeNode[ComplexAVLTreeNode]) => {
          Some(buffer, mutable.SortedSet(oldNode).addOne(node))
        }
        case value @ Some(
              buffer,
              oldNodes: mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]]
            ) => {
          oldNodes.addOne(node)
          value
        }
      }
    })
  }
}

object ComplexAVLFugueFactory {

  given complexAVLFugueFactory: FugueFactory with {
    override type ID = ComplexID
    override type N = ComplexAVLTreeNodeSingle
    override type NC = ComplexAVLTreeNodeSingle.complexAVLTreeNodeSingle.type
    override type F = ComplexAVLFugueFactory
    override type MSG = ComplexAVLMessage

    def create(replicaId: RID): ComplexAVLFugueFactory = {
      ComplexAVLFugueFactory(replicaId)
    }

    override def appendMessage(
        buffer: mutable.ArrayBuffer[ComplexAVLMessage],
        messageToAppend: ComplexAVLMessage
    ): Unit = {
      val last = buffer.last
      last match {
        case insert @ ComplexAVLMessage.Insert(
              rid,
              counter,
              offset,
              valueOrValues,
              parentRid,
              parentCounter,
              parentOffset,
              side
            ) => {
          val valueLength = valueOrValues.size
          messageToAppend match {
            case ComplexAVLMessage.Insert(
                  appendRid,
                  appendCounter,
                  appendOffset,
                  appendValueOrValues,
                  appendParentRid,
                  appendParentCounter,
                  appendParentOffset,
                  appendSide
                ) => {
              if (
                rid == appendParentRid && counter == appendParentCounter && offset + valueLength - 1 == appendParentOffset && side == Side.Right && appendSide == Side.Right
              ) {
                insert.valueOrValues.addAll(appendValueOrValues)
                return
              } else {
                insert.valueOrValues = StringBuilder(
                  insert.valueOrValues.length
                ).append(insert.valueOrValues)
                assert(
                  insert.valueOrValues.length == insert.valueOrValues.capacity
                )
              }
            }
            case other =>
          }
        }
        case delete @ ComplexAVLMessage.Delete(rid, counter, offset, count) => {
          messageToAppend match {
            case ComplexAVLMessage.Delete(
                  appendRid,
                  appendCounter,
                  appendOffset,
                  appendCount
                ) => {
              if (
                rid == appendRid && counter == appendCounter && offset + count == appendOffset
              ) {
                delete.count += appendCount
                return
              }
              if (
                rid == appendRid && counter == appendCounter && appendOffset + appendCount == offset
              ) {
                delete.offset = appendOffset
                delete.count += appendCount
                return
              }
            }
            case other =>
          }
        }
      }

      buffer.addOne(messageToAppend)
    }

    given treeNodeContext: NC =
      ComplexAVLTreeNodeSingle.complexAVLTreeNodeSingle

    given canEqualNode: CanEqual[N, N] = ComplexAVLTreeNodeSingle.canEqual

    private case class StackEntry(
        side: Side,
        children: Iterator[AVLTreeNode[ComplexAVLTreeNode]]
    )

    extension (factory: ComplexAVLFugueFactory) {
      final def insert(i: Int, x: Char): MSG = {
        val leftOrigin =
          if (i == 0) {
            get(null)
          } else {
            atVisibleIndex(i - 1)
          }

        val firstRightChild = leftOrigin.firstRightChild()
        var side: Side | Null = null
        val origin = if (firstRightChild == null) {
          side = Side.Right
          leftOrigin
        } else {
          side = Side.Left
          firstRightChild.leftmostDescendant()
        }

        factory.insert(
          null,
          x,
          origin,
          side.asInstanceOf[Side]
        )
      }

      def handleRemoteMessage(message: MSG, editor: Editory): Unit = {
        message match {
          case ComplexAVLMessage.Insert(
                rid,
                counter,
                offset,
                valueOrValues,
                parentRid,
                parentCounter,
                parentOffset,
                side
              ) => {
            for ((value, i) <- valueOrValues.zipWithIndex) {
              val id = ComplexID(rid, counter, offset + i)
              val parent = if (i == 0) {
                parentRid match {
                  case null => null
                  case rid  => ComplexID(rid, parentCounter, parentOffset)
                }
              } else {
                ComplexID(rid, counter, offset + i - 1)
              }
              val parentTreeNode = factory.get(parent)

              val _ = factory.insert(id, value, parentTreeNode, side)
              val index = visibleIndexOf(get(id))
              editor.insert(index, value)
            }
          }
          case ComplexAVLMessage.Delete(rid, counter, offset, count) =>
            for (i <- (offset until offset + count).reverse) {
              val id = ComplexID(rid, counter, i)
              val treeNode = factory.get(
                id
              )
              val deleted = treeNode.value() != null
              factory.delete(treeNode)
              if (deleted) {
                val index = visibleIndexOf(get(id))
                editor.delete(index)
              }
            }
        }
      }

      override def dupe(): ComplexAVLFugueFactory = factory

      override def createRootNode(): ComplexAVLTreeNodeSingle = {
        val rootTreeNode = ComplexAVLTreeNodeSingle(
          AVLTreeNode(
            ComplexAVLTreeNode(
              null,
              0,
              null,
              0,
              0,
              Side.Right,
              null,
              null,
              false
            ),
            null
          )(using
            complexAVLTreeNodeSize
              .asInstanceOf[AVLTreeNodeValueSize[ComplexAVLTreeNode]]
          ),
          0
        )

        factory.avlTree.insert(null, rootTreeNode.complexTreeNode, Side.Right)
        factory
          .addNode(rootTreeNode.complexTreeNode)

        rootTreeNode.complexTreeNode.value.leftmostDescendantCache =
          DescendantCacheHelper.empty(rootTreeNode.complexTreeNode)
        rootTreeNode.complexTreeNode.value.rightmostDescendantCache =
          DescendantCacheHelper.empty(rootTreeNode.complexTreeNode)

        if (Helper.ENABLE) {
          val _ = rootTreeNode.rightmostDescendant()
          val _ = rootTreeNode.leftmostDescendant()
        }

        rootTreeNode
      }

      private def insertRight(
          parent: AVLTreeNode[ComplexAVLTreeNode],
          nodeToAppend: AVLTreeNode[ComplexAVLTreeNode]
      ): Unit = {
        val value = parent.value
        val afterEmpty = value.rightChildrenBuffer match {
          case null =>
            value.rightChildrenBuffer = nodeToAppend
            factory.avlTree.insertBasedOn(
              parent,
              nodeToAppend,
              Side.Right
            )
            true
          case nonNull =>
            val rightChildrenBuffer = nonNull match {
              case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
                val b = mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]](
                  singleRightChild
                )(using
                  ComplexAVLTreeNode.avlNodeOrdering
                )
                value.rightChildrenBuffer = b
                b
              case rightChildrenBuffer: mutable.SortedSet[AVLTreeNode[
                    ComplexAVLTreeNode
                  ]] =>
                rightChildrenBuffer
            }
            val before = rightChildrenBuffer.maxBefore(nodeToAppend)
            val afterEmpty = rightChildrenBuffer.minAfter(nodeToAppend).isEmpty

            val base = if (rightChildrenBuffer.nn.isEmpty || before.isEmpty) {
              parent
            } else {
              ComplexAVLTreeNodeSingle(
                before.get,
                before.get.value.to
              )
                .rightmostDescendant()
                .complexTreeNode
            }
            rightChildrenBuffer.addOne(nodeToAppend)

            factory.avlTree.insertBasedOn(
              base,
              nodeToAppend,
              Side.Right
            )
            afterEmpty
        }

        factory
          .addNode(nodeToAppend)

        myAssert(nodeToAppend.value.leftmostDescendantCache == null)
        nodeToAppend.value.leftmostDescendantCache =
          DescendantCacheHelper.empty(nodeToAppend)
        myAssert(nodeToAppend.value.rightmostDescendantCache == null)
        nodeToAppend.value.rightmostDescendantCache =
          DescendantCacheHelper.append(
            parent.value.rightmostDescendantCache,
            afterEmpty,
            nodeToAppend,
            parent.value.to - parent.value.offset + 1
          )

        if (Helper.ENABLE) {
          val _ = parent.leftmostDescendant()
          val _ = parent.rightmostDescendant()
          val _ = nodeToAppend.leftmostDescendant()
          val _ = nodeToAppend.rightmostDescendant()
        }
      }

      private def insertLeft(
          parent: AVLTreeNode[ComplexAVLTreeNode],
          nodeToPrepend: AVLTreeNode[ComplexAVLTreeNode]
      ): Unit = {
        val value = parent.value
        val beforeEmpty = value.leftChildrenBuffer match {
          case null =>
            value.leftChildrenBuffer = nodeToPrepend
            factory.avlTree.insertBasedOn(
              parent,
              nodeToPrepend,
              Side.Left
            )
            true
          case nonNull =>
            val leftChildrenBuffer = nonNull match {
              case singleLeftChild: AVLTreeNode[ComplexAVLTreeNode] =>
                val b = mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]](
                  singleLeftChild
                )(using
                  ComplexAVLTreeNode.avlNodeOrdering
                )
                value.leftChildrenBuffer = b
                b
              case leftChildrenBuffer: mutable.SortedSet[AVLTreeNode[
                    ComplexAVLTreeNode
                  ]] =>
                leftChildrenBuffer
            }

            val beforeEmpty =
              leftChildrenBuffer.maxBefore(nodeToPrepend).isEmpty
            val after = leftChildrenBuffer.minAfter(nodeToPrepend)

            val base =
              if (leftChildrenBuffer.isEmpty || after.isEmpty) {
                parent
              } else {
                ComplexAVLTreeNodeSingle(
                  after.get,
                  after.get.value.offset
                )
                  .leftmostDescendant()
                  .complexTreeNode
              }

            leftChildrenBuffer
              .addOne(nodeToPrepend)

            factory.avlTree.insertBasedOn(
              base,
              nodeToPrepend,
              Side.Left
            )

            beforeEmpty
        }

        factory
          .addNode(nodeToPrepend)

        myAssert(nodeToPrepend.value.rightmostDescendantCache == null)
        nodeToPrepend.value.rightmostDescendantCache =
          DescendantCacheHelper.empty(nodeToPrepend)
        myAssert(nodeToPrepend.value.leftmostDescendantCache == null)
        nodeToPrepend.value.leftmostDescendantCache =
          DescendantCacheHelper.append(
            parent.value.leftmostDescendantCache,
            beforeEmpty,
            nodeToPrepend,
            1
          )

        if (Helper.ENABLE) {
          val _ = parent.leftmostDescendant()
          val _ = parent.rightmostDescendant()
          val _ = nodeToPrepend.leftmostDescendant()
          val _ = nodeToPrepend.rightmostDescendant()
        }
      }

      override def insert(
          idOrNull: ComplexID | Null,
          value: Char,
          parent: ComplexAVLTreeNodeSingle,
          side: Side
      ): MSG = {
        insertInternal(
          idOrNull,
          value,
          parent.complexTreeNode,
          parent.index,
          side
        )
      }

      def insertInternal(
          idOrNull: ComplexID | Null,
          value: Char,
          parent: AVLTreeNode[ComplexAVLTreeNode],
          index: Int,
          side: Side
      ): MSG = {
        val rid: RID = if (idOrNull == null) {
          factory.replicaId
        } else {
          idOrNull.rid
        }
        val counter: Int = if (idOrNull == null) {
          factory.counter += 1
          factory.counter
        } else {
          idOrNull.counter
        }
        val offset: Int = if (idOrNull == null) {
          0
        } else {
          idOrNull.offset
        }

        val isRightEdge = side == Side.Right && ComplexAVLTreeNodeSingle(
          parent,
          index
        ).isRightEdge
        if (isRightEdge) {
          val values = parent.value._values
          if (values != null && parent.value.allowAppend) {
            val rightChildrenBuffer =
              parent.value.rightChildrenBuffer
            if (
              (rightChildrenBuffer == null || (rightChildrenBuffer.isInstanceOf[
                mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]]
              ] && rightChildrenBuffer
                .asInstanceOf[
                  mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]]
                ]
                .isEmpty)) && parent.value.rid.nn == rid
            ) {
              val _ = values.append(
                value
              )
              parent.value.to += 1
              parent.fixSizeRecursively(1)

              return ComplexAVLMessage.Insert(
                parent.value.rid.nn,
                parent.value.counter,
                index + 1,
                StringBuilder(value.toString()),
                parent.value.rid.nn,
                parent.value.counter,
                index,
                side
              )
            }
          }
        }

        val isLeftEdge = side == Side.Left && ComplexAVLTreeNodeSingle(
          parent,
          index
        ).isLeftEdge

        var p: AVLTreeNode[ComplexAVLTreeNode] | Null = null
        var bufferOfNodeToAppendOrNull: StringBuilder | Null = null

        if (isRightEdge) {
          if (
            parent.value.allowAppend && parent.value.rid.nn == rid && parent.value.counter == counter
          ) {
            parent.value.allowAppend = false
            p = parent
            bufferOfNodeToAppendOrNull = factory
              .tree(parent.value.rid match {
                case null => null
                case rid  => SimpleID(rid, parent.value.counter)
              })
              ._1
              .nn
          } else {
            p = parent
            bufferOfNodeToAppendOrNull = new StringBuilder(2)
          }
        } else if (isLeftEdge) {
          p = parent
          bufferOfNodeToAppendOrNull = new StringBuilder(1)
        } else {
          val splitIndex = side match {
            case text_rdt.Side.Left  => index
            case text_rdt.Side.Right => index + 1
          }
          val (nodeToSplit, nodeRightSplit) =
            split(parent, splitIndex)
          p = side match {
            case text_rdt.Side.Left  => nodeRightSplit
            case text_rdt.Side.Right => nodeToSplit
          }
          bufferOfNodeToAppendOrNull = new StringBuilder(1)
        }
        val bufferOfNodeToAppend = bufferOfNodeToAppendOrNull.nn
        bufferOfNodeToAppend.addOne(value)

        val nodeToAppend = ComplexAVLTreeNodeSingle(
          AVLTreeNode(
            ComplexAVLTreeNode(
              rid,
              counter,
              bufferOfNodeToAppend,
              offset,
              offset,
              side,
              null,
              null,
              true
            ),
            null
          )(using
            complexAVLTreeNodeSize
              .asInstanceOf[AVLTreeNodeValueSize[ComplexAVLTreeNode]]
          ),
          offset
        )

        if (isRightEdge || (!isLeftEdge && side == Side.Right)) {
          insertRight(p, nodeToAppend.complexTreeNode)
        } else if (isLeftEdge || (!isRightEdge && side == Side.Left)) {
          insertLeft(p, nodeToAppend.complexTreeNode)
        } else {
          throw IllegalStateException()
        }

        ComplexAVLMessage.Insert(
          nodeToAppend.complexTreeNode.value.rid.nn,
          nodeToAppend.complexTreeNode.value.counter,
          nodeToAppend.index,
          StringBuilder(value.toString()),
          parent.value.rid,
          parent.value.counter,
          index,
          side
        )
      }

      private def split(
          nodeToSplit: AVLTreeNode[ComplexAVLTreeNode],
          splitIndex: Int
      ): (
          AVLTreeNode[ComplexAVLTreeNode],
          AVLTreeNode[ComplexAVLTreeNode]
      ) = {
        assert(nodeToSplit.value.offset < splitIndex)
        assert(splitIndex <= nodeToSplit.value.to)

        val oldTo = nodeToSplit.value.to
        nodeToSplit.value.to = splitIndex - 1

        if (nodeToSplit.value._values != null) {
          nodeToSplit.fixSizeRecursively(
            nodeToSplit.value.to - oldTo
          )
        }

        val nodeRightSplit =
          AVLTreeNode(
            ComplexAVLTreeNode(
              nodeToSplit.value.rid,
              nodeToSplit.value.counter,
              nodeToSplit.value._values,
              splitIndex,
              oldTo,
              Side.Right,
              null,
              null,
              nodeToSplit.value.allowAppend
            ),
            null
          )(using
            complexAVLTreeNodeSize
              .asInstanceOf[AVLTreeNodeValueSize[ComplexAVLTreeNode]]
          )
        nodeToSplit.value.allowAppend = false

        nodeRightSplit.value.rightChildrenBuffer =
          nodeToSplit.value.rightChildrenBuffer match {
            case null => null
            case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
              singleRightChild
            case value: mutable.SortedSet[AVLTreeNode[
                  ComplexAVLTreeNode
                ]] =>
              value
          }
        nodeToSplit.value.rightChildrenBuffer = null

        val value = nodeToSplit.value
        val afterEmpty = value.rightChildrenBuffer match {
          case null =>
            value.rightChildrenBuffer = nodeRightSplit
            factory.avlTree.insertBasedOn(
              nodeToSplit,
              nodeRightSplit,
              Side.Right
            )
            true
          case nonNull =>
            val rightChildrenBuffer = nonNull match {
              case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
                val b =
                  mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]](
                    singleRightChild
                  )(using
                    ComplexAVLTreeNode.avlNodeOrdering
                  )
                value.rightChildrenBuffer = b
                b
              case rightChildrenBuffer: mutable.SortedSet[AVLTreeNode[
                    ComplexAVLTreeNode
                  ]] =>
                rightChildrenBuffer
            }
            val before = rightChildrenBuffer.maxBefore(nodeRightSplit)
            val afterEmpty =
              rightChildrenBuffer.minAfter(nodeRightSplit).isEmpty

            val base =
              if (rightChildrenBuffer.nn.isEmpty || before.isEmpty) {
                nodeToSplit
              } else {
                ComplexAVLTreeNodeSingle(
                  before.get,
                  before.get.value.to
                )
                  .rightmostDescendant()
                  .complexTreeNode
              }
            rightChildrenBuffer.addOne(nodeRightSplit)

            factory.avlTree.insertBasedOn(
              base,
              nodeRightSplit,
              Side.Right
            )
            afterEmpty
        }

        factory
          .addNode(nodeRightSplit)

        myAssert(nodeRightSplit.value.leftmostDescendantCache == null)
        nodeRightSplit.value.leftmostDescendantCache =
          DescendantCacheHelper.empty(nodeRightSplit)
        val rightmostDescendantCacheNode =
          AVL2TreeNode(
            nodeToSplit.value.rightmostDescendantCache.value + (splitIndex - nodeToSplit.value.offset),
            nodeToSplit.value.rightmostDescendantCache
          )
        nodeToSplit.value.rightmostDescendantCache
          .tree()
          .insert(
            nodeToSplit.value.rightmostDescendantCache,
            rightmostDescendantCacheNode,
            Side.Right
          )
        myAssert(nodeRightSplit.value.rightmostDescendantCache == null)
        nodeRightSplit.value.rightmostDescendantCache =
          rightmostDescendantCacheNode

        if (
          nodeToSplit.value.rightmostDescendantCache
            .tree()
            .descendant == nodeToSplit
        ) {
          nodeToSplit.value.rightmostDescendantCache
            .tree()
            .descendant = nodeRightSplit
        }

        if (Helper.ENABLE) {
          val _ = nodeToSplit.leftmostDescendant()
          val _ = nodeToSplit.rightmostDescendant()
          val _ = nodeRightSplit.leftmostDescendant()
          val _ = nodeRightSplit.rightmostDescendant()
        }

        (nodeToSplit, nodeRightSplit)
      }

      override def delete(node: ComplexAVLTreeNodeSingle): MSG = {
        deleteInternal(node.complexTreeNode, node.index)
      }

      def deleteInternal(
          node: AVLTreeNode[ComplexAVLTreeNode],
          index: Int
      ): MSG = {
        if (node.value._values == null) {} else if (
          node.value.to - node.value.offset == 0
        ) {
          node.value._values = null
          node.fixSizeRecursively(-1)
        } else if (ComplexAVLTreeNodeSingle(node, index).isLeftEdge) {
          val tmp = factory
            .tree(SimpleID(node.value.rid.nn, node.value.counter))
            ._2
          tmp match {
            case single: AVLTreeNode[ComplexAVLTreeNode] => {
              val (nodeToSplit, nodeRightSplit) =
                split(node, index + 1)
              nodeToSplit.value._values = null
              nodeToSplit.fixSizeRecursively(-1)
            }
            case sortedSet: mutable.SortedSet[AVLTreeNode[
                  ComplexAVLTreeNode
                ]] => {
              val maybeParent = sortedSet
                .rangeUntil(
                  AVLTreeNode(
                    ComplexAVLTreeNode(
                      node.value.rid,
                      node.value.counter,
                      null,
                      node.value.offset,
                      -1,
                      Side.Left,
                      null,
                      null,
                      false
                    ),
                    null
                  )
                )
                .lastOption

              maybeParent match {
                case None => {
                  val (nodeToSplit, nodeRightSplit) =
                    split(node, index + 1)
                  nodeToSplit.value._values = null
                  nodeToSplit.fixSizeRecursively(-1)
                }
                case Some(parent) =>
                  assert(parent.value.to + 1 == node.value.offset)
                  parent.value.rightChildrenBuffer match {
                    case singleRightChild: text_rdt.avl.AVLTreeNode[
                          text_rdt.ComplexAVLTreeNode
                        ]
                        if (
                          singleRightChild.eq(node) &&
                            parent.value._values == null &&
                            singleRightChild.value._values != null
                            &&
                            parent.value.rid.eq(
                              singleRightChild.value.rid
                            ) && parent.value.counter == singleRightChild.value.counter
                            && singleRightChild.value.leftChildrenBuffer == null
                        ) => {
                      parent.value.to += 1
                      assert(sortedSet.remove(singleRightChild))
                      singleRightChild.value.offset += 1
                      singleRightChild.value.rightmostDescendantCache.value += 1
                      singleRightChild.fixSizeRecursively(-1)
                      val _ = sortedSet.addOne(
                        singleRightChild
                      )
                    }
                    case default => {
                      val (nodeToSplit, nodeRightSplit) =
                        split(node, index + 1)
                      nodeToSplit.value._values = null
                      nodeToSplit.fixSizeRecursively(-1)
                    }
                  }
              }
            }
          }
        } else if (ComplexAVLTreeNodeSingle(node, index).isRightEdge) {
          node.value.rightChildrenBuffer match {
            case singleRightChild: text_rdt.avl.AVLTreeNode[
                  text_rdt.ComplexAVLTreeNode
                ]
                if (
                  singleRightChild.value._values == null &&
                    node.value._values != null
                    &&
                    node.value.rid.eq(
                      singleRightChild.value.rid
                    ) && node.value.counter == singleRightChild.value.counter
                    && singleRightChild.value.leftChildrenBuffer == null
                ) => {
              node.value.to -= 1
              node.fixSizeRecursively(-1)
              val tmp = factory
                .tree(SimpleID(node.value.rid.nn, node.value.counter))
                ._2
              tmp match {
                case single: AVLTreeNode[ComplexAVLTreeNode] => {
                  ???
                }
                case sortedSet: mutable.SortedSet[AVLTreeNode[
                      ComplexAVLTreeNode
                    ]] => {
                  assert(sortedSet.remove(singleRightChild))
                  singleRightChild.value.offset -= 1
                  singleRightChild.value.rightmostDescendantCache.value -= 1
                  val _ = sortedSet.addOne(
                    singleRightChild
                  )
                }
              }
            }
            case default => {
              val (nodeToSplit, nodeRightSplit) =
                split(node, index)
              nodeRightSplit.value._values = null
              nodeToSplit.value.allowAppend = false
              nodeRightSplit.fixSizeRecursively(-1)
            }
          }
        } else {
          myAssert(
            node.value.to - node.value.offset >= 2
          )
          val (firstNode, nodeRightSplit) =
            split(node, index)
          val (secondNode, thirdNode) =
            split(nodeRightSplit, index + 1)
          secondNode.value._values = null
          secondNode.fixSizeRecursively(-1)
        }

        ComplexAVLMessage.Delete(node.value.rid.nn, node.value.counter, index)
      }

      final override def get(
          id: ComplexID | Null
      ): ComplexAVLTreeNodeSingle = {
        val tmp = factory
          .tree(id match {
            case null  => null
            case value => SimpleID(value.rid, value.counter)
          })
          ._2
        id match {
          case null =>
            ComplexAVLTreeNodeSingle(
              tmp match {
                case single: AVLTreeNode[ComplexAVLTreeNode] => single
                case other2: mutable.SortedSet[AVLTreeNode[
                      ComplexAVLTreeNode
                    ]] =>
                  other2.firstKey
              },
              0
            )
          case nodeId =>
            val complexTreeNode = tmp match {
              case single: AVLTreeNode[ComplexAVLTreeNode] => single
              case set: mutable.SortedSet[AVLTreeNode[ComplexAVLTreeNode]] => {
                set
                  .rangeUntil(
                    AVLTreeNode(
                      ComplexAVLTreeNode(
                        nodeId.rid,
                        nodeId.counter,
                        null,
                        nodeId.offset + 1,
                        -1,
                        Side.Left,
                        null,
                        null,
                        false
                      ),
                      null
                    )
                  )
                  .last
              }
            }

            myAssert(
              complexTreeNode.value.offset <= nodeId.offset && nodeId.offset <= complexTreeNode.value.to,
              s"$tmp $complexTreeNode $nodeId"
            )
            ComplexAVLTreeNodeSingle(complexTreeNode, nodeId.offset)
        }
      }

      override def text(): String = {
        val result = factory.avlTree
          .values()
          .filter(_._values != null)
          .flatMap(v => v._values.nn.view.slice(v.offset, v.to + 1))
          .mkString
        result
      }

      final override def atVisibleIndex(
          i: Int
      ): complexAVLFugueFactory.this.N = {
        val result = factory.avlTree.nodeAtIndex(i)
        val resultNode = ComplexAVLTreeNodeSingle(
          result._1.nn,
          result._1.nn.value.offset + result._2
        )
        resultNode
      }

      override def visibleIndexOf(node: ComplexAVLTreeNodeSingle): Int = {
        val result = node.complexTreeNode
          .indexOfNode() + (node.index - node.complexTreeNode.value.offset)
        result
      }
    }
  }
}
