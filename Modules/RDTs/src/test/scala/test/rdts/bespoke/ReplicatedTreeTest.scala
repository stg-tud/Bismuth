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

  test("concurrent move") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[String]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, "ROOT")(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, "A")(using aid)
    tree = tree `merge` tree.insert(parent, "B")(using bid)
    val a = tree.children(parent).find(_.value == "A").get.dot
    val b = tree.children(parent).find(_.value == "B").get.dot

    tree = tree `merge` tree.insert(a, "A1")(using aid)
    tree = tree `merge` tree.insert(b, "B1")(using bid)

    val a1 = tree.children(a).find(_.value == "A1").get.dot
    val b1 = tree.children(b).find(_.value == "B1").get.dot

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 2)

      val n1 = root.children(0)
      assertEquals(n1.value, "A")
      assertEquals(n1.children.size, 1)

      val n2 = root.children(1)
      assertEquals(n2.value, "B")
      assertEquals(n2.children.size, 1)

      val n3 = n1.children(0)
      assertEquals(n3.value, "A1")
      assertEquals(n3.children.size, 0)

      val n4 = n2.children(0)
      assertEquals(n4.value, "B1")
      assertEquals(n4.children.size, 0)
    }

    val delta1a = tree.move(a1, b1)
    val delta1b = tree.move(b1, a1)

    val v1a = tree `merge` delta1a
    val v1b = tree `merge` delta1b

    {
      assertEquals(v1a.size, 5)
      assertEquals(v1b.size, 5)

      assertEquals(v1a.children(a1).size, 0)
      assertEquals(v1a.children(b1).size, 1)
      assertEquals(v1a.children(b1).head.value, "A1")

      assertEquals(v1b.children(b1).size, 0)
      assertEquals(v1b.children(a1).size, 1)
      assertEquals(v1b.children(a1).head.value, "B1")
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 2)

      val n1 = root.children(0)
      assertEquals(n1.value, "A")
      assertEquals(n1.children.size, 1)

      val n2 = root.children(1)
      assertEquals(n2.value, "B")
      assertEquals(n2.children.size, 0)

      val n3 = n1.children(0)
      assertEquals(n3.value, "A1")
      assertEquals(n3.children.size, 1)

      val n4 = n3.children(0)
      assertEquals(n4.value, "B1")
      assertEquals(n4.children.size, 0)
    }
  }

  test("concurrent move kleppman example") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[String]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, "ROOT")(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, "A")(using aid)
    tree = tree `merge` tree.insert(parent, "B")(using bid)
    val a = tree.children(parent).find(_.value == "A").get.dot
    val b = tree.children(parent).find(_.value == "B").get.dot
    tree = tree `merge` tree.insert(a, "C")(using aid)

    val delta1a = tree.move(b, a)
    val delta1b = tree.move(a, b)

    val v1a = tree `merge` delta1a
    val v1b = tree `merge` delta1b

    {
      val rootA = treeView(v1a)
      assertEquals(rootA.value, "ROOT")
      assertEquals(rootA.children.size, 1)
      val n1a = rootA.children(0)
      assertEquals(n1a.value, "A")
      assertEquals(n1a.children.size, 2)
      val n2a = n1a.children.find(_.value == "B").get
      assertEquals(n2a.value, "B")
      assertEquals(n2a.children.size, 0)
      val n3a = n1a.children.find(_.value == "C").get
      assertEquals(n3a.value, "C")
      assertEquals(n3a.children.size, 0)

      val rootB = treeView(v1b)
      assertEquals(rootB.value, "ROOT")
      assertEquals(rootB.children.size, 1)
      val n1b = rootB.children(0)
      assertEquals(n1b.value, "B")
      assertEquals(n1b.children.size, 1)
      val n2b = n1b.children(0)
      assertEquals(n2b.value, "A")
      assertEquals(n2b.children.size, 1)
      val n3b = n2b.children(0)
      assertEquals(n3b.value, "C")
      assertEquals(n3b.children.size, 0)
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      val root = treeView(tree)
      assertEquals(root.value, "ROOT")
      assertEquals(root.children.size, 1)
      val n1 = root.children(0)
      assertEquals(n1.value, "A")
      assertEquals(n1.children.size, 2)

      val n2 = n1.children.find(_.value == "B").get
      assertEquals(n2.value, "B")
      assertEquals(n2.children.size, 0)

      val n3 = n1.children.find(_.value == "C").get
      assertEquals(n3.value, "C")
      assertEquals(n3.children.size, 0)
    }
  }
}

case class TreeViewNode[A](value: A, children: List[TreeViewNode[A]])

def treeView[A](tree: ReplicatedTree[A]): TreeViewNode[A] = {
  def treeViewChildren(tree: ReplicatedTree[A], parent: Dot): List[TreeViewNode[A]] =
    tree.children(parent).map { node =>
      TreeViewNode(node.value, treeViewChildren(tree, node.dot))
    }.toList

  val node = tree.root.get

  TreeViewNode(node.value, treeViewChildren(tree, node.dot))
}
