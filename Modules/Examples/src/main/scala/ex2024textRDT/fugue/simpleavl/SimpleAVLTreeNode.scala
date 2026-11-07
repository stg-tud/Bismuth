package ex2024textRDT.fugue.simpleavl

import ex2024textRDT.Helper.myAssert
import ex2024textRDT.debug.pprintNonAVL
import ex2024textRDT.fugue.avl.AVLTreeNode
import ex2024textRDT.fugue.avl2.AVL2TreeNode
import ex2024textRDT.fugue.{*}
import ex2024textRDT.{*}

import scala.collection.mutable

object SimpleAVLTreeNode {
  given simpleAVLTreeNode: TreeNodey[AVLTreeNode[SimpleAVLTreeNode]] with {
    type ID = SimpleID

    extension (treeNode: AVLTreeNode[SimpleAVLTreeNode]) {
      def parent(): AVLTreeNode[SimpleAVLTreeNode] | Null =
        treeNode.value.parent

      def parentId(): SimpleID | Null =
        treeNode.value.parent.nn.id()

      def id(): SimpleID | Null = treeNode.value.id

      def value(): Char | Null =
        treeNode.value.value

      def leftChildren(): Iterator[AVLTreeNode[SimpleAVLTreeNode]] =
        treeNode.value.leftChildrenBuffer match {
          case null  => Iterator.empty
          case value => value.iterator
        }

      def rightChildren(): Iterator[AVLTreeNode[SimpleAVLTreeNode]] =
        treeNode.value.rightChildrenBuffer match {
          case null  => Iterator.empty
          case value => value.iterator
        }

      def side(): Side = treeNode.value.side

      override def leftmostDescendant(): AVLTreeNode[SimpleAVLTreeNode] = {
        val fastDescendant =
          treeNode.value.leftmostDescendantCache.tree().descendant
        myAssert(fastDescendant.eq(treeNode.value.leftChildrenBuffer match {
          case null  => treeNode
          case value =>
            if value.isEmpty then {
              treeNode
            } else {
              value.head.leftmostDescendant()
            }
        }))
        fastDescendant
      }

      final def rightmostDescendant(): AVLTreeNode[SimpleAVLTreeNode] = {
        val fastDescendant =
          treeNode.value.rightmostDescendantCache.tree().descendant
        myAssert(
          fastDescendant.eq(treeNode.value.rightChildrenBuffer match {
            case null  => treeNode
            case value =>
              if value.isEmpty then {
                treeNode
              } else {
                value.last.rightmostDescendant()
              }
          }),
          s"got ${fastDescendant.value.id} expected ${(treeNode.value.rightChildrenBuffer match {
              case null  => treeNode
              case value =>
                if value.isEmpty then {
                  treeNode
                } else {
                  value.last.rightmostDescendant()
                }
            }).value.id}"
        )
        fastDescendant
      }
    }
  }

}

final case class SimpleAVLTreeNode(
    id: SimpleID | Null,
    var value: Char | Null,
    parent: AVLTreeNode[SimpleAVLTreeNode] | Null,
    side: Side,
    var leftChildrenBuffer: mutable.ArrayBuffer[AVLTreeNode[
      SimpleAVLTreeNode
    ]] | Null,
    var rightChildrenBuffer: mutable.ArrayBuffer[AVLTreeNode[
      SimpleAVLTreeNode
    ]] | Null,
    depth: Int,
    var leftmostDescendantCache: AVL2TreeNode[AVLTreeNode[
      SimpleAVLTreeNode
    ]],
    var rightmostDescendantCache: AVL2TreeNode[AVLTreeNode[
      SimpleAVLTreeNode
    ]]
) {
  myAssert(leftChildrenBuffer == null)
  myAssert(rightChildrenBuffer == null)

  override def equals(x: Any): Boolean = this eq x.asInstanceOf[Object]

  override def hashCode(): Int = System.identityHashCode(this)

  override def toString: String = pprintNonAVL.apply(this).plainText
}
