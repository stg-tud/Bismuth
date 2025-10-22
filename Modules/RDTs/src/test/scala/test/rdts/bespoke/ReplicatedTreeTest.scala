package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedTree

import scala.language.implicitConversions
import rdts.time.Dot
import rdts.base.Lattice.assertEquals

class ReplicatedTreeTest extends munit.FunSuite {
  test("insert") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    val v0 = ReplicatedTree.empty[String]

    assert(v0.isEmpty)

    val v1 = v0 `merge` v0.insert(ReplicatedTree.rootDot, "ROOT")(using aid)

    {
      val root = treeView(v1)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 0)
    }

    val parent = v1.children(ReplicatedTree.rootDot).head.dot

    val v2 = v1 `merge` v1.insert(parent, "A")(using aid) `merge` v1.insert(parent, "B")(using bid)

    {
      assertEquals(v2.size, 3)

      val root = treeView(v2)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 2)

      val n1 = root.children(0)
      assertEquals(n1.value, "A")
      assertEquals(n1.children.size, 0)

      val n2 = root.children(1)
      assertEquals(n2.value, "B")
      assertEquals(n2.children.size, 0)
    }

    val a  = v2.children(parent).find(_.value == "A").get
    val b  = v2.children(parent).find(_.value == "B").get
    val v3 = v2 `merge` v2.move(a.dot, b.dot)

    {
      assertEquals(v3.size, 3)

      val root = treeView(v3)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 1)

      val n1 = root.children(0)
      assertEquals(n1.value, "B")
      assertEquals(n1.children.size, 1)

      val n2 = n1.children(0)
      assertEquals(n2.value, "A")
      assertEquals(n2.children.size, 0)
    }
  }
}

case class TreeViewNode[A](value: A, children: List[TreeViewNode[A]])

def treeView[A](tree: ReplicatedTree[A]): TreeViewNode[A] = {
  def treeViewChildren(tree: ReplicatedTree[A], parent: Dot): List[TreeViewNode[A]] =
    tree.children(parent).map { node =>
      TreeViewNode(node.value, treeViewChildren(tree, node.dot))
    }.toList

  val node = tree.root().get

  TreeViewNode(node.value, treeViewChildren(tree, node.dot))
}
