package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedTree

import scala.language.implicitConversions
import rdts.time.Dot

class ReplicatedTreeTest extends munit.FunSuite {
  test("insert") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    val v0 = ReplicatedTree.empty[String]

    assert(v0.isEmpty)

    val v1 = v0 `merge` v0.insert(ReplicatedTree.rootDot, "ROOT")(using aid)

    assertEquals(v1.size, 1)
    assertEquals(v1.children(ReplicatedTree.rootDot).size, 1)
    assertEquals(v1.children(ReplicatedTree.rootDot).head.value, "ROOT")

    val parent = v1.children(ReplicatedTree.rootDot).head.dot

    val v2 = v1 `merge` v1.insert(parent, "A")(using aid) `merge` v1.insert(parent, "B")(using bid)

    {
      assertEquals(v2.size, 3)

      val root = v2.node(parent).get
      assertEquals(root.value, "ROOT")
      assertEquals(v2.children(root.dot).size, 2)

      val a = v2.children(root.dot).find(_.value == "A").get
      val b = v2.children(root.dot).find(_.value == "B").get
      assertEquals(a.value, "A")
      assertEquals(b.value, "B")
    }

    val a  = v2.children(parent).find(_.value == "A").get
    val b  = v2.children(parent).find(_.value == "B").get
    val v3 = v2 `merge` v2.move(a.dot, b.dot)

    {
      val parent = v1.children(ReplicatedTree.rootDot).head.dot
      assertEquals(v3.size, 3)

      val root = v3.node(parent).get
      assertEquals(root.value, "ROOT")

      assertEquals(v3.children(root.dot).size, 1)
      val b = v3.children(root.dot).head
      assertEquals(b.value, "B")

      assertEquals(v3.children(b.dot).size, 1)
      val a = v3.children(b.dot).head
      assertEquals(a.value, "A")
    }
  }
}

def treeView[A](tree: ReplicatedTree[A]): Set[Set[Set[A]]] = {
  def collectChildren(dot: Dot): Set[Set[A]] = {
    val children = tree.children(dot)
    if children.isEmpty then Set()
    else {
      children.map { child =>
        Set(child.value)
      }.toSet
    }
  }

  if tree.isEmpty then Set.empty
  else {
    var set: Set[Set[Set[A]]] = Set()
    set = set + Set(Set(tree.node(ReplicatedTree.rootDot).get.value))
    set = set + collectChildren(ReplicatedTree.rootDot)
    set
  }
}
