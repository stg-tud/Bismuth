package text_rdt

import scala.collection.mutable

given simpleTreeNode: TreeNodey[SimpleTreeNode] with {
  type ID = SimpleID

  extension (treeNode: SimpleTreeNode) {
    def parent(): SimpleTreeNode | Null = treeNode.node.parent

    def parentId(): SimpleID | Null = treeNode.node.parent match {
      case null   => null
      case parent => parent.id()
    }

    def id(): SimpleID | Null = treeNode.node.id

    def value(): Char | Null = treeNode.node.value

    def leftChildren(): Iterator[SimpleTreeNode] =
      treeNode.leftChildrenBuffer.iterator

    def rightChildren(): Iterator[SimpleTreeNode] =
      treeNode.rightChildrenBuffer.iterator

    def side(): Side = treeNode.node.side
  }
}

final case class SimpleTreeNode(
    var node: Node,
    leftChildrenBuffer: mutable.ArrayBuffer[SimpleTreeNode],
    rightChildrenBuffer: mutable.ArrayBuffer[SimpleTreeNode]
) {
  override def equals(x: Any): Boolean = this eq x.asInstanceOf[Object]

  override def hashCode(): Int = System.identityHashCode(this)

  override def toString: String = pprintNonAVL.apply(this).plainText
}

object SimpleTreeNode {
  given canEqual: CanEqual[SimpleTreeNode, SimpleTreeNode] =
    CanEqual.derived
}
