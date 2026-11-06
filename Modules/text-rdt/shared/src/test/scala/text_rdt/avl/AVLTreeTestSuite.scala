package text_rdt.avl

import munit.diff.Printer
import munit.{FunSuite, Location}
import text_rdt.{Helper, Side}

given effe: AVLTreeNodeValueSize[String] with {

  extension (string: String) {
    override def size: Int = string.length()
  }
}

class AVLTreeTestSuite extends FunSuite {

  override val printer: Printer = Printer.apply { case value: AVLTreeNode[?] =>
    value.toString
  }

  def nodeIndexBijective[T](
      tree: AVLTree[T],
      index: Int,
      nodeWithOffset: Option[(AVLTreeNode[T], Int)]
  )(using loc: Location): Unit = {
    nodeWithOffset match {
      case None        =>
      case Some(value) => assertEquals(value._1.indexOfNode() + value._2, index)
    }
  }

  test("assertions enabled") {
    assertEquals(Helper.ENABLE, true)
  }

  test("empty node nodeAtIndex") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("", null)
    tree.root = root
    val nodeR = AVLTreeNode[String]("b", null)
    tree.insert(root, nodeR, Side.Right)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(left = null, value = "", right = AVLTreeNode(left = null, value = "b", right = null))"""
    )
  }

  test("1 01 0001: Rotate Left") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    val nodeR =
      AVLTreeNode("b", root)
    val nodeRR = AVLTreeNode("c", nodeR)
    tree.root = root
    root.right = nodeR
    nodeR.right = nodeRR
    root.rotateLeft(tree)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(
  left = AVLTreeNode(left = null, value = "a", right = null),
  value = "b",
  right = AVLTreeNode(left = null, value = "c", right = null)
)"""
    )
  }

  test("empty") {
    val tree = AVLTree[String](null)
    assertEquals(tree.root, null)
  }

  test("0 -> 1") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    tree.insert(null, root, Side.Left)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(left = null, value = "a", right = null)"""
    )
  }

  test("0 -> 1") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    tree.insert(null, root, Side.Right)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(left = null, value = "a", right = null)"""
    )
  }

  test("1 -> 1 01") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    tree.root = root
    val nodeR = AVLTreeNode("b", null)
    tree.insert(root, nodeR, Side.Right)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(left = null, value = "a", right = AVLTreeNode(left = null, value = "b", right = null))"""
    )
  }

  test("1 -> 1 10") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    tree.root = root
    val nodeL = AVLTreeNode("b", null)
    tree.insert(root, nodeL, Side.Left)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(left = AVLTreeNode(left = null, value = "b", right = null), value = "a", right = null)"""
    )
  }

  test("1 01 -> 1 01 0001") {
    val tree = AVLTree[String](null)
    val root = AVLTreeNode("a", null)
    val nodeR = AVLTreeNode("b", root)
    root.right = nodeR
    tree.root = root
    root.deepSize = 2
    root.height = 2
    val nodeRR = AVLTreeNode("c", null)
    tree.insert(nodeR, nodeRR, Side.Right)
    assertEquals(
      tree.root.toString(),
      """AVLTreeNode(
  left = AVLTreeNode(left = null, value = "a", right = null),
  value = "b",
  right = AVLTreeNode(left = null, value = "c", right = null)
)"""
    )
    assertEquals(tree.values(), Seq("a", "b", "c"))
    assertEquals(root.indexOfNode(), 0)
    assertEquals(nodeR.indexOfNode(), 1)
    assertEquals(nodeRR.indexOfNode(), 2)
  }
}
