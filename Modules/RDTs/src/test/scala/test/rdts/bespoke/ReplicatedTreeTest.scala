package test.rdts.bespoke
import rdts.base.Lattice.syntax.merge
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.ReplicatedTree

import scala.language.implicitConversions
import rdts.time.Dot
import munit.Assertions
import scala.util.Random
import rdts.base.Lattice
import rdts.datatypes.LastWriterWins as LWW

class ReplicatedTreeTest extends munit.FunSuite {
  test("insert") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    val v0 = ReplicatedTree.empty[LWW[String]]

    assert(v0.isEmpty)

    val v1 = v0 `merge` v0.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)

    {
      assertEquals(v1.size, 1)

      val root = treeView(v1)
      root.assertValue("ROOT")
      root.assertChildren(Set.empty)
    }

    val parent = v1.children(ReplicatedTree.rootDot).head.dot

    val v2 = v1 `merge` v1.insert(parent, LWW.now("A"))(using aid) `merge` v1.insert(parent, LWW.now("B"))(using bid)

    {
      assertEquals(v2.size, 3)

      val root = treeView(v2)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("A")
      n1.assertChildren(Set.empty)

      val n2 = root.child("B")
      n2.assertChildren(Set.empty)
    }

    val a  = v2.children(parent).find(_.value.value == "A").get
    val b  = v2.children(parent).find(_.value.value == "B").get
    val v3 = v2 `merge` v2.move(a.dot, b.dot)

    {
      assertEquals(v3.size, 3)

      val root = treeView(v3)
      root.assertValue("ROOT")
      root.assertChildren(Set("B"))

      val n1 = root.child("B")
      n1.assertChildren(Set("A"))

      val n2 = n1.child("A")
      n2.assertChildren(Set.empty)
    }
  }

  test("concurrent move") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot

    tree = tree `merge` tree.insert(a, LWW.now("A1"))(using aid)
    tree = tree `merge` tree.insert(b, LWW.now("B1"))(using bid)

    val a1 = tree.children(a).find(_.value.value == "A1").get.dot
    val b1 = tree.children(b).find(_.value.value == "B1").get.dot

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("A")
      n1.assertChildren(Set("A1"))

      val n2 = root.child("B")
      n2.assertChildren(Set("B1"))

      val n3 = n1.child("A1")
      n3.assertChildren(Set.empty)

      val n4 = n2.child("B1")
      n4.assertChildren(Set.empty)
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
      assertEquals(v1a.children(b1).head.value.value, "A1")

      assertEquals(v1b.children(b1).size, 0)
      assertEquals(v1b.children(a1).size, 1)
      assertEquals(v1b.children(a1).head.value.value, "B1")
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("A")
      n1.assertChildren(Set("A1"))

      val n2 = root.child("B")
      n2.assertChildren(Set.empty)

      val n3 = n1.child("A1")
      n3.assertChildren(Set("B1"))

      val n4 = n3.child("B1")
      n4.assertChildren(Set.empty)
    }
  }

  test("concurrent move kleppman example") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    // Create initial tree: ROOT -> A,B ; A -> C
    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot
    tree = tree `merge` tree.insert(a, LWW.now("C"))(using aid)

    // Move A under B and B under A concurrently
    val delta1a = tree.move(b, a)
    val delta1b = tree.move(a, b)

    val v1a = tree `merge` delta1a
    val v1b = tree `merge` delta1b

    {
      assertEquals(v1a.size, 4)
      assertEquals(v1b.size, 4)

      val rootA = treeView(v1a)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("A"))

      val n1a = rootA.child("A")
      n1a.assertChildren(Set("B", "C"))

      val rootB = treeView(v1b)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("B"))

      val n1b = rootB.child("B")
      n1b.assertChildren(Set("A"))

      val n2b = n1b.child("A")
      n2b.assertChildren(Set("C"))
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 4)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A"))

      val n1 = root.child("A")
      n1.assertChildren(Set("B", "C"))

      val n2 = n1.child("B")
      n2.assertChildren(Set.empty)

      val n3 = n1.child("C")
      n3.assertChildren(Set.empty)
    }
  }

  test("associativity") {
    given Lattice[Int] = math.max

    val (expected, deltas) = randomTree(50)
    for _ <- 0 until 10 do {
      val shuffledDeltas = Random.shuffle(deltas)
      val result         = shuffledDeltas.foldLeft(ReplicatedTree.empty[Int])(_ `merge` _)
      assertEquals(result, expected)
    }
  }

  test("parent cycle") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    // Create initial tree: ROOT -> C -> A,B
    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val root = tree.root.get.dot
    tree = tree `merge` tree.insert(root, LWW.now("C"))(using aid)
    var c = tree.children(root).find(_.value.value == "C").get.dot
    tree = tree `merge` tree.insert(c, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(c, LWW.now("B"))(using aid)
    val a = tree.children(c).find(_.value.value == "A").get.dot
    val b = tree.children(c).find(_.value.value == "B").get.dot

    {
      assertEquals(tree.size, 4)
      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("C"))

      val c = root.child("C")
      c.assertChildren(Set("A", "B"))
    }

    // Create a cycle by concurrently moving A under B and B under A
    val deltaA = tree.move(a, b)
    val deltaB = tree.move(b, a)
    val a1     = tree `merge` deltaA
    val b1     = tree `merge` deltaB

    {
      assertEquals(a1.size, 4)
      assertEquals(b1.size, 4)

      val rootA = treeView(a1)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("C"))

      val cA = rootA.child("C")
      cA.assertChildren(Set("B"))

      val bA = cA.child("B")
      bA.assertChildren(Set("A"))

      val rootB = treeView(b1)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("C"))

      val cB = rootB.child("C")
      cB.assertChildren(Set("A"))

      val aB = cB.child("A")
      aB.assertChildren(Set("B"))
    }

    val a2 = tree `merge` deltaB
    val b2 = tree `merge` deltaA

    {
      assertEquals(a2.size, 4)
      assertEquals(b2.size, 4)

      val rootA = treeView(a2)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("C"))

      val cA = rootA.child("C")
      cA.assertChildren(Set("A"))

      val aA = cA.child("A")
      aA.assertChildren(Set("B"))

      val rootB = treeView(b2)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("C"))

      val cB = rootB.child("C")
      cB.assertChildren(Set("B"))

      val bB = cB.child("B")
      bB.assertChildren(Set("A"))
    }
  }

  test("move after parent cycle") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    // Create initial tree: ROOT -> C, D ; C -> A,B
    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val root = tree.root.get.dot
    tree = tree `merge` tree.insert(root, LWW.now("C"))(using aid)
    tree = tree `merge` tree.insert(root, LWW.now("D"))(using aid)
    var c = tree.children(root).find(_.value.value == "C").get.dot
    var d = tree.children(root).find(_.value.value == "D").get.dot
    tree = tree `merge` tree.insert(c, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(c, LWW.now("B"))(using aid)
    val a = tree.children(c).find(_.value.value == "A").get.dot
    val b = tree.children(c).find(_.value.value == "B").get.dot

    {
      assertEquals(tree.size, 5)
      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("C", "D"))

      val c = root.child("C")
      c.assertChildren(Set("A", "B"))
    }

    // Create a cycle by concurrently moving A under B and B under A
    tree = tree `merge` tree.move(a, b) `merge` tree.move(b, a)

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("C", "D"))

      val c = root.child("C")
      c.assertChildren(Set("B"))

      val b = c.child("B")
      b.assertChildren(Set("A"))
    }

    // Now move A under D
    tree = tree `merge` tree.move(a, d)

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("C", "D"))

      val c = root.child("C")
      c.assertChildren(Set("B"))

      val d = root.child("D")
      d.assertChildren(Set("A"))
    }
  }

  test("move with concurrent deletion") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot

    {
      assertEquals(tree.size, 3)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))
    }

    val delta1a = tree.move(b, a)
    val delta1b = tree.delete(b)

    val v1a = tree `merge` delta1a
    val v1b = tree `merge` delta1b

    {
      assertEquals(v1a.size, 3)
      val rootA = treeView(v1a)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("A"))

      val n1a = rootA.child("A")
      n1a.assertChildren(Set("B"))

      assertEquals(v1b.size, 2)
      val rootB = treeView(v1b)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("A"))
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 2)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A"))
    }
  }

  test("move with concurrent target deletion".ignore) {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot

    {
      assertEquals(tree.size, 3)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))
    }

    val delta1a = tree.move(b, a)
    val delta1b = tree.delete(a)

    val v1a = tree `merge` delta1a
    val v1b = tree `merge` delta1b

    {
      assertEquals(v1a.size, 3)
      val rootA = treeView(v1a)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("A"))

      val n1a = rootA.child("A")
      n1a.assertChildren(Set("B"))

      assertEquals(v1b.size, 2)
      val rootB = treeView(v1b)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("B"))
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 2)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("B"))
    }
  }

  test("nested deletion") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot
    tree = tree `merge` tree.insert(a, LWW.now("A1"))(using aid)
    tree = tree `merge` tree.insert(a, LWW.now("A2"))(using aid)
    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("A")
      n1.assertChildren(Set("A1", "A2"))
    }

    tree = tree `merge` tree.delete(a)

    {
      assertEquals(tree.size, 2)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("B"))
    }
  }

  test("concurrent move with nested deletion") {
    val aid = Uid.predefined("a")
    val bid = Uid.predefined("b")

    var tree = ReplicatedTree.empty[LWW[String]]
    tree = tree `merge` tree.insert(ReplicatedTree.rootDot, LWW.now("ROOT"))(using aid)
    val parent = tree.root.get.dot
    tree = tree `merge` tree.insert(parent, LWW.now("A"))(using aid)
    tree = tree `merge` tree.insert(parent, LWW.now("B"))(using bid)
    val a = tree.children(parent).find(_.value.value == "A").get.dot
    val b = tree.children(parent).find(_.value.value == "B").get.dot
    tree = tree `merge` tree.insert(a, LWW.now("C"))(using aid)
    val c = tree.children(a).find(_.value.value == "C").get.dot
    tree = tree `merge` tree.insert(c, LWW.now("D"))(using aid)
    val d = tree.children(c).find(_.value.value == "D").get.dot

    {
      assertEquals(tree.size, 5)

      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("A")
      n1.assertChildren(Set("C"))

      val n2 = n1.child("C")
      n2.assertChildren(Set("D"))
    }

    val delta1a = tree.move(c, b)
    val delta1b = tree.delete(d)
    val v1a     = tree `merge` delta1a
    val v1b     = tree `merge` delta1b

    {
      assertEquals(v1a.size, 5)
      val rootA = treeView(v1a)
      rootA.assertValue("ROOT")
      rootA.assertChildren(Set("A", "B"))

      val n1A = rootA.child("B")
      n1A.assertChildren(Set("C"))

      val n2A = n1A.child("C")
      n2A.assertChildren(Set("D"))

      assertEquals(v1b.size, 4)
      val rootB = treeView(v1b)
      rootB.assertValue("ROOT")
      rootB.assertChildren(Set("A", "B"))

      val n1B = rootB.child("A")
      n1B.assertChildren(Set("C"))
    }

    tree = tree `merge` delta1a `merge` delta1b

    {
      assertEquals(tree.size, 4)
      val root = treeView(tree)
      root.assertValue("ROOT")
      root.assertChildren(Set("A", "B"))

      val n1 = root.child("B")
      n1.assertChildren(Set("C"))
    }
  }
}

def randomTree(treeSize: Int): (ReplicatedTree[Int], List[ReplicatedTree[Int]]) = {
  given Lattice[Int] = math.max
  val id             = LocalUid.predefined("test")
  var tree           = ReplicatedTree.empty[Int]
  val root           = tree.insert(ReplicatedTree.rootDot, 0)(using id)
  var deltas         = List(root)

  tree = tree `merge` root

  for _ <- 0 until treeSize - 1 do {
    val randomIndex = Random.nextInt(tree.size)
    val randomNode  = tree.nodes.map(_.dot).toList(randomIndex)
    val delta       = tree.insert(randomNode, Random.nextInt(treeSize))(using id)
    deltas = deltas :+ delta
    tree = tree `merge` delta
  }

  (tree, deltas)
}

case class TreeViewNode[A](value: A, children: Set[TreeViewNode[A]]) {
  inline def assertChildren(expected: Set[A]) = {
    val childValues = children.map(_.value)
    Assertions.assertEquals(childValues, expected)
  }

  inline def assertValue(expected: A) =
    Assertions.assertEquals(value, expected)

  inline def child(value: A): TreeViewNode[A] =
    children.find(_.value == value).getOrElse(throw new NoSuchElementException(s"Child with value $value not found"))
}

def treeView[A](tree: ReplicatedTree[LWW[A]]): TreeViewNode[A] = {
  def treeViewChildren(tree: ReplicatedTree[LWW[A]], parent: Dot): Set[TreeViewNode[A]] =
    tree.children(parent).map { node =>
      TreeViewNode(node.value.value, treeViewChildren(tree, node.dot))
    }.toSet

  val node = tree.root.get

  TreeViewNode(node.value.value, treeViewChildren(tree, node.dot))
}
