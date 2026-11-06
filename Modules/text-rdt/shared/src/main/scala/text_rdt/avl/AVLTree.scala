package text_rdt.avl

import text_rdt.Helper.myAssert
import text_rdt.{Helper, Side, pprintAVL}

import scala.annotation.tailrec
import scala.compiletime.uninitialized

trait AVLTreeNodeValueSize[T] {

  extension (node: T) {

    def size: Int
  }
}

extension [T](optionalAvlNode: AVLTreeNode[T] | Null) {

  def height: Int = {
    optionalAvlNode match {
      case null  => 0
      case value => value.height
    }
  }

  def calculatedHeight(): Int = {
    optionalAvlNode match {
      case null  => 0
      case value => value.calculatedHeight()
    }
  }

  def deepSize: Int = {
    optionalAvlNode match {
      case null  => 0
      case value => value.deepSize
    }
  }

  def calculatedDeepSize(): Int = {
    optionalAvlNode match {
      case null  => 0
      case value => value.calculatedDeepSize()
    }
  }

  def foreach(method: AVLTreeNode[T] => Unit): Unit = {
    optionalAvlNode match {
      case null  =>
      case value => method(value)
    }
  }

  def forall(method: AVLTreeNode[T] => Boolean): Boolean = {
    optionalAvlNode match {
      case null  => true
      case value => method(value)
    }
  }
}

object AVLTreeNode {

  def apply[T: AVLTreeNodeValueSize](
      gen: AVLTreeNode[T] => T
  ): AVLTreeNode[T] = {
    val result = new AVLTreeNode()
    result.value = gen(result)
    result.fixHeight()
    result.fixDeepSize()
    result
  }

  def apply[T: AVLTreeNodeValueSize](
      value: T,
      parent: AVLTreeNode[T] | Null
  ): AVLTreeNode[T] = {
    val result = new AVLTreeNode()
    result.value = value
    result.parent = parent
    result.fixHeight()
    result.fixDeepSize()
    result
  }
}

case class NodeAtIndexResult[T](node: AVLTreeNode[T] | Null, offset: Int)

final class AVLTreeNode[T: AVLTreeNodeValueSize] {

  final var value: T = uninitialized

  var deepSize: Int = -1
  var parent: AVLTreeNode[T] | Null =
    uninitialized
  var left: AVLTreeNode[T] | Null =
    uninitialized
  var right: AVLTreeNode[T] | Null = uninitialized
  var height: Int = 1

  override def equals(x: Any): Boolean = this eq x.asInstanceOf[Object]

  override def hashCode(): Int = System.identityHashCode(this)

  override def toString: String = pprintAVL.apply(this).plainText

  def calculatedDeepSize(): Int = {
    left.calculatedDeepSize() + value.size + right.calculatedDeepSize()
  }

  def calculatedHeight(): Int = {
    val result = Math.max(
      left.calculatedHeight(),
      right.calculatedHeight()
    ) + 1
    myAssert(result >= 1)
    result
  }

  def invariantSpecifiedHeight(): Unit = {
    if (Helper.ENABLE) {
      myAssert(
        height == calculatedHeight(),
        s"$height == ${calculatedHeight()} for $this"
      )
    }
  }

  def invariantSpecifiedHeightRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      invariantSpecifiedHeightRecursiveDownInternal()
    }
  }

  def invariantBalancedRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      invariantBalanced()
      left.foreach(_.invariantBalancedRecursiveDown())
      right.foreach(_.invariantBalancedRecursiveDown())
    }
  }

  def invariantBalanced(): Unit = {
    if (Helper.ENABLE) {
      myAssert(
        Math.abs(
          left.height
            - right.height
        ) <= 1,
        s"for $this"
      )
    }
  }

  def invariantAvl(): Unit = {
    if (Helper.ENABLE) {
      invariantParentLinks()
      invariantSize()
      invariantSpecifiedHeight()
      invariantBalanced()
    }
  }

  def invariantAvlRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      invariantAvlRecursiveDownInternal()
    }
  }

  def invariant(): Unit = {
    if (Helper.ENABLE) {
      invariantInternal()
    }
  }

  private def invariantInternal(): Unit = {
    invariantParentLinks()
    left.foreach(_.invariantInternal())
    right.foreach(_.invariantInternal())
  }

  def invariantParentLinks(): Unit = {
    if (Helper.ENABLE) {
      myAssert(left.forall(c => c.parent.eq(this)))
      myAssert(right.forall(c => c.parent.eq(this)))
      myAssert(
        parent.forall(parent => parent.left.eq(this) || parent.right.eq(this)),
        s"parent: $parent, this: $this"
      )
    }
  }

  private def invariantSize(): Unit = {
    if (Helper.ENABLE) {
      myAssert(
        deepSize == calculatedDeepSize(),
        s"$deepSize == ${calculatedDeepSize()} for $this"
      )
    }
  }

  private def invariantSizeRecursiveDownInternal(): Unit = {
    invariantSize()
    left.foreach(_.invariantSizeRecursiveDown())
    right.foreach(_.invariantSizeRecursiveDown())

  }

  private def invariantSizeRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      invariantSizeRecursiveDownInternal()
    }
  }

  @tailrec
  def nodeAtIndex(offset: Int): NodeAtIndexResult[T] = {
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
    var remainingOffset: Int = offset
    remainingOffset = left match {
      case null => remainingOffset
      case value =>
        if (remainingOffset < value.deepSize) {
          return value.nodeAtIndex(remainingOffset)
        } else {
          remainingOffset - value.deepSize
        }
    }
    remainingOffset = if (remainingOffset < value.size) {
      return NodeAtIndexResult(this, remainingOffset)
    } else {
      remainingOffset - value.size
    }
    remainingOffset = right match {
      case null => remainingOffset
      case value =>
        if (remainingOffset < value.deepSize) {
          return value.nodeAtIndex(remainingOffset)
        } else {
          remainingOffset - value.deepSize
        }
    }
    throw new IndexOutOfBoundsException()
  }

  def fixHeight(): Unit = {
    if (Helper.ENABLE) {
      left.foreach(_.invariantSpecifiedHeightRecursiveDown())
      right.foreach(_.invariantSpecifiedHeightRecursiveDown())
    }
    height = 1 + Math.max(
      left.height,
      right.height
    )
    if (Helper.ENABLE) {
      invariantSpecifiedHeightRecursiveDown()
    }
  }

  def fixDeepSize(): Unit = {
    if (Helper.ENABLE) {
      left.foreach(_.invariantSizeRecursiveDown())
      right.foreach(_.invariantSizeRecursiveDown())
    }
    deepSize = left.deepSize + value.size + right.deepSize
    if (Helper.ENABLE) {
      invariantSizeRecursiveDown()
    }
  }

  @tailrec
  def fixSizeRecursively(diff: Int): Unit = {
    deepSize += diff
    if (Helper.ENABLE) {
      this.invariantSizeRecursiveDownInternal()
    }
    parent match {
      case null  =>
      case value => value.fixSizeRecursively(diff)
    }
  }

  def replaceWith(tree: AVLTree[T], replacement: AVLTreeNode[T]): Unit = {
    this.parent match {
      case null =>
        tree.root = replacement
        replacement.parent = null
      case value =>
        if (value.left eq this) {
          value.left = replacement
        } else if (value.right eq this) {
          value.right = replacement
        } else {
          myAssert(false)
        }
        replacement.parent = value
    }
  }

  def rotateLeft(tree: AVLTree[T]): Unit = {
    if (Helper.ENABLE) {
      myAssert(right != null)
    }

    val rightGet = right.nn
    val temp = rightGet
    this.replaceWith(tree, temp)
    right = rightGet.left
    rightGet.left match {
      case null =>
      case v    => v.parent = this
    }
    temp.left = this
    this.parent = temp

    this.fixDeepSize()
    temp.fixDeepSize()

    this.fixHeight()
    temp.fixHeight()

    if (Helper.ENABLE) {
      temp.invariantSpecifiedHeightRecursiveDown()
      temp.invariantSizeRecursiveDown()
    }
  }

  def rotateRight(tree: AVLTree[T]): Unit = {
    if (Helper.ENABLE) {
      myAssert(left != null)
      tree.invariant()
    }

    val leftGet = left.nn
    val temp = leftGet
    this.replaceWith(tree, temp)
    left = leftGet.right
    leftGet.right match {
      case null =>
      case v    => v.parent = this
    }
    temp.right = this
    this.parent = temp

    this.fixDeepSize()
    temp.fixDeepSize()

    this.fixHeight()
    temp.fixHeight()

    if (Helper.ENABLE) {
      temp.invariantSpecifiedHeightRecursiveDown()
      temp.invariantSizeRecursiveDown()
      tree.invariant()
    }
  }

  def indexOfNode(): Int = {
    indexOfNode(0)
  }

  @tailrec
  def indexOfNode(acc: Int): Int = {
    side() match {
      case null => acc + left.deepSize
      case Side.Left =>
        parent.nn.indexOfNode(acc - value.size - right.deepSize)
      case Side.Right =>
        parent.nn.indexOfNode(acc + left.deepSize + parent.nn.value.size)
    }
  }

  private def side(): Side | Null = {
    parent match {
      case null   => null
      case parent => sideGet()
    }
  }

  private def sideGet(): Side = {
    if (parent.nn.left eq this) {
      Side.Left
    } else if (parent.nn.right eq this) {
      Side.Right
    } else {
      assert(false)
    }
  }

  def values(): Seq[T] = {
    (left match {
      case null =>
        Seq.empty
      case left =>
        left.values()
    })
      ++ Seq(value)
      ++ (right match {
        case null =>
          Seq.empty
        case right =>
          right.values()
      })
  }

  def collectNodes(): Set[AVLTreeNode[T]] = {
    (left match {
      case null => Set.empty
      case left => left.collectNodes()
    }) ++ Set(this) ++ (right match {
      case null  => Set.empty
      case right => right.collectNodes()
    })
  }

  private def invariantSpecifiedHeightRecursiveDownInternal(): Unit = {
    invariantSpecifiedHeight()
    left.foreach(_.invariantSpecifiedHeightRecursiveDown())
    right.foreach(_.invariantSpecifiedHeightRecursiveDown())
  }

  private def invariantAvlRecursiveDownInternal(): Unit = {
    invariantAvl()
    left.foreach(_.invariantAvlRecursiveDownInternal())
    right.foreach(_.invariantAvlRecursiveDownInternal())
  }

  @tailrec
  private def leftmostDescendant(): AVLTreeNode[T] = {
    left match {
      case null => this
      case left => left.leftmostDescendant()
    }
  }

  @tailrec
  private def rightmostDescendant(): AVLTreeNode[T] = {
    right match {
      case null  => this
      case right => right.rightmostDescendant()
    }
  }

  private def rebalanceRight(tree: AVLTree[T]): AVLTreeNode[T] = {
    if (Helper.ENABLE) {
      tree.invariant()
      left.foreach(_.invariantAvlRecursiveDown())
      right.foreach(_.invariantAvlRecursiveDown())
    }
    val result = this
    if (right.height - left.height == 2) {
      if (right.nn.right.height > right.nn.left.height) {
        this.rotateLeft(tree)
      } else {
        if (Helper.ENABLE) {
          myAssert(right.nn.left.height > right.nn.right.height)
        }
        this.right.nn.rotateRight(tree)
        this.rotateLeft(tree)
      }
    } else {
      this.fixDeepSize()
      this.fixHeight()
    }
    if (Helper.ENABLE) {
      tree.invariant()
      result.invariantAvlRecursiveDown()
    }
    result
  }

  private def rebalanceLeft(tree: AVLTree[T]): AVLTreeNode[T] = {
    if (Helper.ENABLE) {
      tree.invariant()
      left.foreach(_.invariantAvlRecursiveDown())
      right.foreach(_.invariantAvlRecursiveDown())
    }
    val result = this
    if (left.height - right.height == 2) {
      if (left.nn.left.height > left.nn.right.height) {
        this.rotateRight(tree)
      } else {
        if (Helper.ENABLE) {
          myAssert(left.nn.right.height > left.nn.left.height)
        }
        this.left.nn.rotateLeft(tree)
        this.rotateRight(tree)
      }
    } else {
      this.fixHeight()
      this.fixDeepSize()
    }
    if (Helper.ENABLE) {
      tree.invariant()
      result.invariantAvlRecursiveDown()
    }
    result
  }

  @tailrec
  private def retrace(
      tree: AVLTree[T],
      side: Side
  ): Unit = {
    if (Helper.ENABLE) {
      tree.invariant()
    }
    side match {
      case text_rdt.Side.Left =>
        val newLast = rebalanceLeft(tree)
        if (Helper.ENABLE) {
          newLast.invariantAvlRecursiveDown()
        }
        parent match {
          case null =>
          case value =>
            if (Helper.ENABLE) {
              tree.invariant()
            }
            value.retrace(tree, this.sideGet())
        }
      case text_rdt.Side.Right =>
        val newLast = rebalanceRight(tree)
        if (Helper.ENABLE) {
          newLast.invariantAvlRecursiveDown()
        }
        parent match {
          case null =>
          case value =>
            if (Helper.ENABLE) {
              tree.invariant()
            }
            value.retrace(tree, this.sideGet())
        }
    }
  }

  private[avl] def insert(
      tree: AVLTree[T],
      nodeToInsert: AVLTreeNode[T],
      side: Side
  ): Unit = {
    if (Helper.ENABLE) {
      tree.invariant()
      tree.invariantAvlRecursiveDown()
    }
    side match {
      case text_rdt.Side.Left =>
        left match {
          case null =>
            left = nodeToInsert
            nodeToInsert.parent = this
            if (Helper.ENABLE) {
              tree.invariant()
            }
            this.retrace(tree, Side.Left)
            if (Helper.ENABLE) {
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
          case value =>
            val rDesc = value.rightmostDescendant()
            rDesc.right = nodeToInsert
            nodeToInsert.parent = rDesc
            if (Helper.ENABLE) {
              tree.invariant()
            }
            rDesc.retrace(tree, Side.Right)
            if (Helper.ENABLE) {
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
        }
      case text_rdt.Side.Right =>
        right match {
          case null =>
            right = nodeToInsert
            nodeToInsert.parent = this
            if (Helper.ENABLE) {
              tree.invariant()
            }
            this.retrace(tree, Side.Right)
            if (Helper.ENABLE) {
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
          case value =>
            val lDesc = value.leftmostDescendant()
            lDesc.left = nodeToInsert
            nodeToInsert.parent = lDesc
            if (Helper.ENABLE) {
              tree.invariant()
            }
            lDesc.retrace(tree, Side.Left)
            if (Helper.ENABLE) {
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
        }
    }
  }
}

final case class AVLTree[T: AVLTreeNodeValueSize](
    var root: AVLTreeNode[T] | Null
) {
  override def toString: String = pprintAVL.apply(this).plainText

  def insert(
      nodeToInsertBasedOn: AVLTreeNode[T] | Null,
      nodeToInsert: AVLTreeNode[T],
      side: Side
  ): Unit = {
    if (Helper.ENABLE) {
      invariant()
      invariantAvlRecursiveDown()
    }
    nodeToInsertBasedOn match {
      case null =>
        assert(root == null)
        root = nodeToInsert
      case value => insertBasedOn(value, nodeToInsert, side)
    }
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
  }

  def invariant(): Unit = {
    if (Helper.ENABLE) {
      root.foreach(_.invariant())
    }
  }

  def insertBasedOn(
      nodeToInsertBasedOn: AVLTreeNode[T],
      nodeToInsert: AVLTreeNode[T],
      side: Side
  ): Unit = {
    if (Helper.ENABLE) {
      invariant()
      invariantAvlRecursiveDown()
    }
    nodeToInsertBasedOn.insert(this, nodeToInsert, side)
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
  }

  def nodeAtIndex(offset: Int): NodeAtIndexResult[T] = {
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
    val result = root match {
      case null => NodeAtIndexResult(null, 0)
      case root =>
        val result = root.nodeAtIndex(offset)
        if (Helper.ENABLE) {
          myAssert(result.node.nn.value.size != 0)
        }
        result
    }
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
    result
  }

  def values(): Seq[T] = {
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
    val result = root match {
      case null => Seq.empty
      case root => root.values()
    }
    if (Helper.ENABLE) {
      invariantAvlRecursiveDown()
    }
    result
  }

  def invariantAvlRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      root.foreach(_.invariantAvlRecursiveDown())
    }
  }
}
