package text_rdt.avl2

import text_rdt.Helper.myAssert
import text_rdt.{Helper, Side, pprintAVL}
import scala.math.Ordered.orderingToOrdered
import scala.annotation.tailrec

object AVL2TreeNode {

  def apply[V](
      value: Int,
      parent: AVL2TreeNode[V] | AVL2Tree[V],
      left: AVL2TreeNode[V] | Null = null,
      right: AVL2TreeNode[V] | Null = null
  ): AVL2TreeNode[V] = {
    new AVL2TreeNode(
      value,
      parent,
      left,
      right,
      1 + Math.max(left.height, right.height)
    )
  }

  extension [V](optionalAvlNode: AVL2TreeNode[V] | Null) {

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

    def foreach(method: AVL2TreeNode[V] => Unit): Unit = {
      optionalAvlNode match {
        case null  =>
        case value => method(value)
      }
    }

    def forall(method: AVL2TreeNode[V] => Boolean): Boolean = {
      optionalAvlNode match {
        case null  => true
        case value => method(value)
      }
    }
  }

  private def join[V](
      tree: AVL2Tree[V],
      left: AVL2TreeNode[V] | Null,
      value: AVL2TreeNode[V],
      right: AVL2TreeNode[V] | Null
  ): AVL2TreeNode[V] = {
    if (Helper.ENABLE) {

      myAssert(left.ne(value))
      myAssert(right.ne(value))
      myAssert(
        (left == null && right == null) || left.ne(right),
        s"left $left right $right"
      )
    }
    if (left.height > right.height + 1) {
      joinRightAVL(tree, left, value, right)
    } else if (right.height > left.height + 1) {
      joinLeftAVL(tree, left, value, right)
    } else {
      value.left = left
      left.foreach(_.parent = value)
      value.right = right
      right.foreach(_.parent = value)
      value.fixHeight()
      value
    }
  }

  private def joinLeftAVL[V](
      tree: AVL2Tree[V],
      left: AVL2TreeNode[V] | Null,
      k: AVL2TreeNode[V],
      right: AVL2TreeNode[V] | Null
  ): AVL2TreeNode[V] = {
    val r = right.nn.right
    val kprime = right.nn
    val c = right.nn.left
    if (c.height <= left.height + 1) {
      val tprime = k
      tprime.right = c
      tprime.left = left
      tprime.left.foreach(_.parent = tprime)
      tprime.right.foreach(_.parent = tprime)
      tprime.fixHeight()
      if (tprime.height <= r.height + 1) {
        kprime.right = r
        kprime.left = tprime
        tprime.replaceWith(tree, kprime)
        kprime.left.foreach(_.parent = kprime)
        kprime.right.foreach(_.parent = kprime)
        kprime.fixHeight()
        kprime
      } else {
        kprime.right = r
        val tmp = tprime.rotateLeft(tree)
        tmp.replaceWith(tree, kprime)
        kprime.left = tmp
        kprime.left.foreach(_.parent = kprime)
        kprime.right.foreach(_.parent = kprime)
        kprime.fixHeight()
        kprime.rotateRight(tree)
      }
    } else {
      var tprime = k
      tprime = joinLeftAVL(tree, left, tprime, c)
      val tprimeprime = kprime
      kprime.right = r
      tprime.replaceWith(tree, kprime)
      kprime.left = tprime
      kprime.left.foreach(_.parent = kprime)
      kprime.right.foreach(_.parent = kprime)
      kprime.fixHeight()
      if (tprime.height <= r.height + 1) {
        tprimeprime
      } else {
        tprimeprime.rotateRight(tree)
      }
    }
  }

  private def joinRightAVL[V](
      tree: AVL2Tree[V],
      left: AVL2TreeNode[V] | Null,
      k: AVL2TreeNode[V],
      right: AVL2TreeNode[V] | Null
  ): AVL2TreeNode[V] = {
    val l = left.nn.left
    val kprime = left.nn
    val c = left.nn.right
    if (c.height <= right.height + 1) {
      val tprime = k
      tprime.left = c
      tprime.right = right
      tprime.left.foreach(_.parent = tprime)
      tprime.right.foreach(_.parent = tprime)
      tprime.fixHeight()
      if (tprime.height <= l.height + 1) {
        kprime.left = l
        kprime.right = tprime
        tprime.replaceWith(tree, kprime)
        kprime.left.foreach(_.parent = kprime)
        kprime.right.foreach(_.parent = kprime)
        kprime.fixHeight()
        kprime
      } else {
        kprime.left = l
        val tmp = tprime.rotateRight(tree)
        tmp.replaceWith(tree, kprime)
        kprime.right = tmp
        kprime.left.foreach(_.parent = kprime)
        kprime.right.foreach(_.parent = kprime)
        kprime.fixHeight()
        kprime.rotateLeft(tree)
      }
    } else {
      var tprime = k
      tprime = joinRightAVL(tree, c, tprime, right)
      val tprimeprime = kprime
      kprime.left = l
      tprime.replaceWith(tree, kprime)
      kprime.right = tprime
      kprime.left.foreach(_.parent = kprime)
      kprime.right.foreach(_.parent = kprime)
      kprime.fixHeight()
      if (tprime.height <= l.height + 1) {
        tprimeprime
      } else {
        tprimeprime.rotateLeft(tree)
      }
    }
  }
}

final case class AVL2TreeNode[V] private (
    var value: Int,
    var parent: AVL2TreeNode[V] | AVL2Tree[V],
    var left: AVL2TreeNode[V] | Null,
    var right: AVL2TreeNode[V] | Null,
    var height: Int
) {

  override def equals(x: Any): Boolean = this eq x.asInstanceOf[Object]

  override def hashCode(): Int = System.identityHashCode(this)

  override def toString: String = pprintAVL.apply(this).plainText

  def calculatedHeight(): Int = {
    myAssert(this.ne(left))
    myAssert(this.ne(right))
    myAssert(
      (left == null && right == null) || left.ne(right),
      s"left $left right $right"
    )
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
      myAssert(
        left.forall(c => c.parent.eq(this)),
        s"$left expected parent $this but got ${left.nn.parent}"
      )
      myAssert(
        right.forall(c => c.parent.eq(this)),
        s"$right expected parent $this but got ${right.nn.parent}"
      )
      myAssert(
        parent match {
          case noParent: AVL2Tree[V] => true
          case parent: AVL2TreeNode[V] =>
            parent.forall(parent =>
              parent.left.eq(this) || parent.right.eq(this)
            )
        },
        s"parent: $parent, this: $this"
      )
    }
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

  def replaceWith(
      tree: AVL2Tree[V],
      replacement: AVL2TreeNode[V]
  ): Unit = {
    this.parent match {
      case noParent: AVL2Tree[V] =>
        tree.root = replacement
        replacement.parent = tree
      case value: AVL2TreeNode[V] =>
        if (value.left.eq(this)) {
          value.left = replacement
        } else if (value.right.eq(this)) {
          value.right = replacement
        } else {
          assert(false)
        }
        replacement.parent = value
    }
  }

  def rotateLeft(tree: AVL2Tree[V]): AVL2TreeNode[V] = {
    if (Helper.ENABLE) {

      tree.invariant()
      this.invariant()
    }
    assert(right != null)

    val rightGet = right.nn
    val temp = rightGet
    this.replaceWith(tree, temp)
    right = rightGet.left
    rightGet.left.foreach(_.parent = this)
    temp.left = this
    this.parent = temp

    this.fixHeight()
    temp.fixHeight()

    if (Helper.ENABLE) {

      temp.invariantSpecifiedHeightRecursiveDown()
    }
    temp
  }

  def rotateRight(tree: AVL2Tree[V]): AVL2TreeNode[V] = {
    if (Helper.ENABLE) {

      tree.invariant()
      this.invariant()
    }
    assert(left != null)

    val leftGet = left.nn
    val temp = leftGet
    this.replaceWith(tree, temp)
    left = leftGet.right
    leftGet.right.foreach(_.parent = this)
    temp.right = this
    this.parent = temp

    this.fixHeight()
    temp.fixHeight()

    if (Helper.ENABLE) {

      temp.invariantSpecifiedHeightRecursiveDown()
      tree.invariant()
    }
    temp
  }

  def split(
      treeForSmaller: AVL2Tree[V],
      tree: AVL2Tree[V],
      key: Int
  ): (AVL2TreeNode[V] | Null, AVL2TreeNode[V] | Null) = {
    if (key.compare(value) < 0) {
      val (leftprime, rightprime) = left match {
        case null  => (null, null)
        case value => value.split(treeForSmaller, tree, key)
      }
      val rightResult = AVL2TreeNode.join(tree, rightprime, this, right)
      (leftprime, rightResult)
    } else {
      val (leftprime, rightprime) = right match {
        case null  => (null, null)
        case value => value.split(treeForSmaller, tree, key)
      }
      val leftResult = AVL2TreeNode.join(treeForSmaller, left, this, leftprime)
      (leftResult, rightprime)
    }
  }

  def insert(
      tree: AVL2Tree[V],
      nodeToInsert: AVL2TreeNode[V],
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
            myAssert(nodeToInsert.value < this.value)
            left = nodeToInsert
            nodeToInsert.parent = this
            if (Helper.ENABLE) {
              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
            }
            this.retrace(tree, Side.Left)
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} after retrace when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
          case value =>
            val rDesc = value.rightmostDescendant()
            myAssert(rDesc.value < nodeToInsert.value)
            rDesc.right = nodeToInsert
            nodeToInsert.parent = rDesc
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
            }
            rDesc.retrace(tree, Side.Right)
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} after retrace when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
        }
      case text_rdt.Side.Right =>
        right match {
          case null =>
            myAssert(this.value < nodeToInsert.value)
            right = nodeToInsert
            nodeToInsert.parent = this
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
            }
            this.retrace(tree, Side.Right)
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} after retrace when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
          case value =>
            val lDesc = value.leftmostDescendant()
            if (Helper.ENABLE) {

              myAssert(nodeToInsert.value < lDesc.value)
            }
            lDesc.left = nodeToInsert
            nodeToInsert.parent = lDesc
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
            }
            lDesc.retrace(tree, Side.Left)
            if (Helper.ENABLE) {

              myAssert(
                tree.values() == tree.values().sorted,
                s"${tree.values()} == ${tree.values().sorted} after retrace when inserting ${nodeToInsert.value}"
              )
              tree.invariant()
              tree.invariantAvlRecursiveDown()
            }
        }
    }
  }

  def values(): Seq[Int] = {
    (left match {
      case null => Seq.empty
      case left => left.values()
    }) ++ Seq(value) ++ (right match {
      case null  => Seq.empty
      case right => right.values()
    })
  }

  def collectNodes(): Set[AVL2TreeNode[V]] = {
    (left match {
      case null => Set.empty
      case left => left.collectNodes()
    }) ++ Set(this) ++ (right match {
      case null  => Set.empty
      case right => right.collectNodes()
    })
  }

  @tailrec
  private def isRightmostNode: Boolean = {
    parent match {
      case parent: AVL2TreeNode[V] =>
        if (parent.left.eq(this)) {
          false
        } else {
          parent.isRightmostNode
        }
      case tree: AVL2Tree[V] => true
    }
  }

  def isLastNode: Boolean = {
    if (right != null) {
      return false
    }
    isRightmostNode
  }

  @tailrec
  final def tree(): AVL2Tree[V] = {
    parent match {
      case value: AVL2TreeNode[V] => value.tree()
      case value: AVL2Tree[V]     => value
    }
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
  private def leftmostDescendant(): AVL2TreeNode[V] = {
    left match {
      case null => this
      case left => left.leftmostDescendant()
    }
  }

  @tailrec
  private def rightmostDescendant(): AVL2TreeNode[V] = {
    right match {
      case null  => this
      case right => right.rightmostDescendant()
    }
  }

  private def rebalanceRight(tree: AVL2Tree[V]): AVL2TreeNode[V] = {
    if (Helper.ENABLE) {
      tree.invariant()
      left.foreach(_.invariantAvlRecursiveDown())
      right.foreach(_.invariantAvlRecursiveDown())
    }
    val result = this
    if (right.height - left.height == 2) {
      if (right.nn.right.height > right.nn.left.height) {
        val _ = this.rotateLeft(tree)
      } else {
        if (Helper.ENABLE) {
          myAssert(right.nn.left.height > right.nn.right.height)
        }
        val _ = this.right.nn.rotateRight(tree)
        val _ = this.rotateLeft(tree)
      }
    } else {
      this.fixHeight()
    }
    if (Helper.ENABLE) {
      tree.invariant()
      result.invariantAvlRecursiveDown()
    }
    result
  }

  private def rebalanceLeft(tree: AVL2Tree[V]): AVL2TreeNode[V] = {
    if (Helper.ENABLE) {

      tree.invariant()
      left.foreach(_.invariantAvlRecursiveDown())
      right.foreach(_.invariantAvlRecursiveDown())
    }
    val result = this
    if (left.height - right.height == 2) {
      if (left.nn.left.height > left.nn.right.height) {
        val _ = this.rotateRight(tree)
      } else {
        if (Helper.ENABLE) {

          myAssert(left.nn.right.height > left.nn.left.height)
        }
        val _ = this.left.nn.rotateLeft(tree)
        val _ = this.rotateRight(tree)
      }
    } else {
      this.fixHeight()
    }
    if (Helper.ENABLE) {

      tree.invariant()
      result.invariantAvlRecursiveDown()
    }
    result
  }

  @tailrec
  private def retrace(
      tree: AVL2Tree[V],
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
          case noParent: AVL2Tree[V] =>
          case value: AVL2TreeNode[V] =>
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
          case noParent: AVL2Tree[V] =>
          case value: AVL2TreeNode[V] =>
            if (Helper.ENABLE) {

              tree.invariant()
            }
            value.retrace(tree, this.sideGet())
        }
    }
  }

  private def sideGet(): Side = {
    if (parent.asInstanceOf[AVL2TreeNode[V]].left.eq(this)) {
      Side.Left
    } else if (parent.asInstanceOf[AVL2TreeNode[V]].right.eq(this)) {
      Side.Right
    } else {
      assert(false)
    }
  }
}

final case class AVL2Tree[V](
    var root: AVL2TreeNode[V] | Null,
    var descendant: V
) {
  override def toString: String = pprintAVL.apply(this).plainText

  def split(
      key: Int,
      treeForSmaller: AVL2Tree[V]
  ): (AVL2Tree[V], AVL2Tree[V]) = {
    if (Helper.ENABLE) {

      this.invariant()
      this.invariantAvlRecursiveDown()
      treeForSmaller.invariant()
      treeForSmaller.invariantAvlRecursiveDown()
      myAssert(root.ne(this))
      myAssert(
        this.values() == this.values().sorted,
        s"${this.values()} == ${this.values().sorted} before split at $key"
      )
      myAssert(
        treeForSmaller.values() == treeForSmaller.values().sorted,
        s"${treeForSmaller.values()} == ${treeForSmaller.values().sorted} before split at $key"
      )
    }
    root match {
      case null => (treeForSmaller, this)
      case value =>
        val (left, right) = value.split(treeForSmaller, this, key)
        left match {
          case null =>
            treeForSmaller.root = null
          case value =>
            treeForSmaller.root = value
            value.parent = treeForSmaller
        }
        right match {
          case null =>
            this.root = null
          case value =>
            this.root = value
            value.parent = this
        }
        if (Helper.ENABLE) {

          this.invariant()
          this.invariantAvlRecursiveDown()
          treeForSmaller.invariant()
          treeForSmaller.invariantAvlRecursiveDown()
          myAssert(
            this.values() == this.values().sorted,
            s"${this.values()} == ${this.values().sorted} after split at $key"
          )
          myAssert(
            treeForSmaller.values() == treeForSmaller.values().sorted,
            s"${treeForSmaller.values()} == ${treeForSmaller.values().sorted} after split at $key"
          )
        }
        (treeForSmaller, this)
    }
  }

  def insert(
      nodeToInsertBasedOn: AVL2TreeNode[V] | Null,
      nodeToInsert: AVL2TreeNode[V],
      side: Side
  ): Unit = {
    if (Helper.ENABLE) {

      invariant()
      invariantAvlRecursiveDown()
    }
    nodeToInsertBasedOn match {
      case null =>
        myAssert(root == null)
        root = nodeToInsert
      case value => insertBasedOn(value, nodeToInsert, side)
    }
    if (Helper.ENABLE) {

      invariantAvlRecursiveDown()
    }
  }

  def insertBasedOn(
      nodeToInsertBasedOn: AVL2TreeNode[V],
      nodeToInsert: AVL2TreeNode[V],
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

  def invariant(): Unit = {
    if (Helper.ENABLE) {
      root.foreach(_.invariant())
    }
  }

  def values(): Seq[Int] = {
    val result = root match {
      case null => Seq.empty
      case root => root.values()
    }
    result
  }

  def collectNodes(): Set[AVL2TreeNode[V]] = {
    root match {
      case null => Set.empty
      case root => root.collectNodes()
    }
  }

  def invariantAvlRecursiveDown(): Unit = {
    if (Helper.ENABLE) {
      root.foreach(_.invariantAvlRecursiveDown())
    }
  }
}
