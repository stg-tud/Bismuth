package text_rdt

import text_rdt.Helper.myAssert
import text_rdt.avl.AVLTreeNode

import scala.collection.mutable
import scala.collection.View
import text_rdt.avl2.AVL2TreeNode
import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.collection.mutable.SortedSet

final case class ComplexAVLTreeNode(
    rid: RID | Null,
    counter: Int,
    var _values: StringBuilder | Null,
    var offset: Int,
    var to: Int,
    side: Side,
    var leftChildrenBuffer: mutable.SortedSet[
      AVLTreeNode[ComplexAVLTreeNode]
    ] | AVLTreeNode[ComplexAVLTreeNode] | Null,
    var rightChildrenBuffer: mutable.SortedSet[
      AVLTreeNode[ComplexAVLTreeNode]
    ] | AVLTreeNode[ComplexAVLTreeNode] | Null,
    var allowAppend: Boolean
) {
  var leftmostDescendantCache: AVL2TreeNode[AVLTreeNode[
    ComplexAVLTreeNode
  ]] = scala.compiletime.uninitialized
  var rightmostDescendantCache: AVL2TreeNode[AVLTreeNode[
    ComplexAVLTreeNode
  ]] = scala.compiletime.uninitialized

  def values: View[Char] = {
    _values.nn.view.slice(offset, to + 1)
  }

  inline def invariant(): Unit = {}

  override def toString: String = pprintNonAVL.apply(this).plainText
}

object ComplexAVLTreeNode {
  given avlNodeOrdering: Ordering[AVLTreeNode[ComplexAVLTreeNode]] with {

    def compare(
        x: text_rdt.avl.AVLTreeNode[text_rdt.ComplexAVLTreeNode],
        y: text_rdt.avl.AVLTreeNode[text_rdt.ComplexAVLTreeNode]
    ): Int = (
      x.value.rid.nn,
      x.value.counter,
      x.value.offset
    ) compare (y.value.rid.nn, y.value.counter, y.value.offset)
  }
}

extension (treeNode: AVLTreeNode[ComplexAVLTreeNode]) {
  def leftmostDescendant(): AVLTreeNode[ComplexAVLTreeNode] = {
    ComplexAVLTreeNodeSingle(
      treeNode,
      treeNode.value.offset
    ).leftmostDescendant().complexTreeNode
  }

  def rightmostDescendant(): AVLTreeNode[ComplexAVLTreeNode] = {
    ComplexAVLTreeNodeSingle(
      treeNode,
      treeNode.value.to
    ).rightmostDescendant().complexTreeNode
  }
}

final case class ComplexAVLTreeNodeSingle(
    complexTreeNode: AVLTreeNode[ComplexAVLTreeNode],
    index: Int
) {
  myAssert(
    complexTreeNode.value.offset <= index,
    s"${complexTreeNode.value.offset} <= $index"
  )

  myAssert(
    index <= complexTreeNode.value.to,
    s"$index <= ${complexTreeNode.value.to}"
  )

  final def isRightEdge: Boolean = {
    index == complexTreeNode.value.to
  }

  final def isLeftEdge: Boolean = {
    index == complexTreeNode.value.offset
  }

  override def equals(_x: Any): Boolean = {
    val x = _x.asInstanceOf[ComplexAVLTreeNodeSingle]
    this.complexTreeNode.eq(x.complexTreeNode) && this.index == x.index
  }

  override def toString: String = pprintNonAVL.apply(this).plainText
}

object ComplexAVLTreeNodeSingle {

  given complexAVLTreeNodeSingle: TreeNodey[ComplexAVLTreeNodeSingle] with {
    override type ID = ComplexID

    extension (treeNode: ComplexAVLTreeNodeSingle) {

      override def parent(): ComplexAVLTreeNodeSingle | Null = {
        ???
      }

      override def parentId(): ComplexID | Null = {
        ???
      }

      override def id(): ComplexID | Null =
        treeNode.complexTreeNode.value.rid match {
          case null => null
          case rid: RID =>
            ComplexID(
              rid,
              treeNode.complexTreeNode.value.counter,
              treeNode.index
            )
        }

      override def value(): Char | Null =
        treeNode.complexTreeNode.value._values match {
          case null =>
            null
          case values =>
            values(treeNode.index)
        }

      final override def firstLeftChild(): ComplexAVLTreeNodeSingle | Null = {
        if (treeNode.isLeftEdge) {
          treeNode.complexTreeNode.value.leftChildrenBuffer match {
            case null => null
            case singleLeftChild: AVLTreeNode[ComplexAVLTreeNode] =>
              ComplexAVLTreeNodeSingle(
                singleLeftChild,
                singleLeftChild.value.offset
              )
            case leftChildrenBuffer: mutable.SortedSet[
                  AVLTreeNode[ComplexAVLTreeNode]
                ] =>
              if (leftChildrenBuffer.nonEmpty) {
                val v = leftChildrenBuffer.head
                ComplexAVLTreeNodeSingle(v, v.value.offset)
              } else {
                null
              }
          }
        } else {
          null
        }
      }

      override def leftChildren(): Iterator[ComplexAVLTreeNodeSingle] = {
        if (treeNode.isLeftEdge) {
          treeNode.complexTreeNode.value.leftChildrenBuffer match {
            case null => Iterator.empty
            case singleLeftChild: AVLTreeNode[ComplexAVLTreeNode] =>
              Iterator(
                ComplexAVLTreeNodeSingle(
                  singleLeftChild,
                  singleLeftChild.value.offset
                )
              )
            case leftChildrenBuffer: mutable.SortedSet[
                  AVLTreeNode[ComplexAVLTreeNode]
                ] =>
              leftChildrenBuffer.iterator.map(v =>
                ComplexAVLTreeNodeSingle(v, v.value.offset)
              )
          }
        } else {
          Iterator.empty
        }
      }

      final override def firstRightChild(): ComplexAVLTreeNodeSingle | Null = {
        if (!treeNode.isRightEdge) {
          ComplexAVLTreeNodeSingle(
            treeNode.complexTreeNode,
            treeNode.index + 1
          )
        } else {
          treeNode.complexTreeNode.value.rightChildrenBuffer match {
            case null => null
            case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
              ComplexAVLTreeNodeSingle(
                singleRightChild,
                singleRightChild.value.offset
              )
            case rightChildrenBuffer: mutable.SortedSet[
                  AVLTreeNode[ComplexAVLTreeNode]
                ] =>
              val v = rightChildrenBuffer.head
              ComplexAVLTreeNodeSingle(v, v.value.offset)
          }
        }
      }

      override def rightChildren(): Iterator[ComplexAVLTreeNodeSingle] = {
        if (!treeNode.isRightEdge) {
          Iterator(
            ComplexAVLTreeNodeSingle(
              treeNode.complexTreeNode,
              treeNode.index + 1
            )
          )
        } else {
          treeNode.complexTreeNode.value.rightChildrenBuffer match {
            case null => Iterator.empty
            case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
              Iterator(
                ComplexAVLTreeNodeSingle(
                  singleRightChild,
                  singleRightChild.value.offset
                )
              )
            case rightChildrenBuffer: SortedSet[
                  AVLTreeNode[ComplexAVLTreeNode]
                ] =>
              rightChildrenBuffer.iterator.map(v =>
                ComplexAVLTreeNodeSingle(v, v.value.offset)
              )
          }
        }
      }

      final override def leftmostDescendant(): ComplexAVLTreeNodeSingle = {
        val fastDescendant = if (treeNode.isLeftEdge) {
          val node = treeNode.complexTreeNode.value.leftmostDescendantCache
            .tree()
            .descendant
          ComplexAVLTreeNodeSingle(
            node,
            node.value.offset
          )
        } else {
          ComplexAVLTreeNodeSingle(treeNode.complexTreeNode, treeNode.index)
        }
        fastDescendant
      }

      def rightmostDescendant(): ComplexAVLTreeNodeSingle = {
        val fastDescendantNode =
          treeNode.complexTreeNode.value.rightmostDescendantCache
            .tree()
            .descendant
        val fastDescendant =
          ComplexAVLTreeNodeSingle(
            fastDescendantNode,
            fastDescendantNode.value.to
          )
        if (Helper.ENABLE) {
          val expected =
            treeNode.complexTreeNode.value.rightChildrenBuffer match {
              case null =>
                ComplexAVLTreeNodeSingle(
                  treeNode.complexTreeNode,
                  treeNode.complexTreeNode.value.to
                )
              case singleRightChild: AVLTreeNode[ComplexAVLTreeNode] =>
                ComplexAVLTreeNodeSingle(
                  singleRightChild,
                  singleRightChild.value.to
                ).rightmostDescendant()
              case value: mutable.SortedSet[
                    AVLTreeNode[ComplexAVLTreeNode]
                  ] =>
                if (value.isEmpty) {
                  ComplexAVLTreeNodeSingle(
                    treeNode.complexTreeNode,
                    treeNode.complexTreeNode.value.to
                  )
                } else {
                  ComplexAVLTreeNodeSingle(
                    value.last,
                    value.last.value.to
                  ).rightmostDescendant()
                }
            }
          myAssert(
            fastDescendant == expected,
            s"for ${treeNode} got ${fastDescendant} expected ${expected}"
          )
        }
        fastDescendant
      }

      override def side(): Side = {
        if (treeNode.isLeftEdge) {
          treeNode.complexTreeNode.value.side
        } else {
          Side.Right
        }
      }
    }
  }
}
