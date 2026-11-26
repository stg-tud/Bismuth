package test.rdts.bespoke

import rdts.base.Lattice
import rdts.experiments.MerkleSearchTree
import rdts.experiments.MerkleSearchTree.{Empty, Node, Tree}

import scala.util.Random

class MerkleSearchTreeTest extends munit.FunSuite {

  given maxLattice: Lattice[Int] = math.max

  type MST = MerkleSearchTree[String, Int]

  def build(entries: Iterable[(String, Int)]): MST =
    entries.foldLeft(MerkleSearchTree.empty[String, Int]) { case (t, (k, v)) => t.put(k, v) }

  def randomEntries(rnd: Random, n: Int): Map[String, Int] =
    Iterator.continually(rnd.alphanumeric.take(1 + rnd.nextInt(8)).mkString -> rnd.nextInt(1000)).take(n).toMap

  /** Checks ordering, level and child count invariants of the canonical form; returns the keys in order. */
  def checkStructure(tree: Tree[String, Int], parentLevel: Int): List[String] = tree match
      case Empty                => Nil
      case n: Node[String, Int] =>
        assert(n.level < parentLevel)
        assertEquals(n.children.size, n.keys.size + 1)
        n.keys.foreach(k => assertEquals(MerkleSearchTree.levelOf(MerkleSearchTree.Encoder.string.encode(k)), n.level))
        val keys = n.children.indices.toList.flatMap { i =>
          checkStructure(n.children(i), n.level) ++ n.keys.lift(i).toList
        }
        assertEquals(keys, keys.sorted)
        assertEquals(n.size, keys.size)
        keys

  def check(tree: MST, expected: Map[String, Int]): Unit = {
    assertEquals(checkStructure(tree.root, Int.MaxValue), expected.keys.toList.sorted)
    assertEquals(tree.toMap, expected)
    assertEquals(tree.size, expected.size)
    expected.foreach((k, v) => assertEquals(tree.get(k), Some(v)))
  }

  test("empty") {
    val t = MerkleSearchTree.empty[String, Int]
    assert(t.isEmpty)
    assertEquals(t.get("a"), None)
    assertEquals(t.iterator.toList, Nil)
    assertEquals(t.remove("a"), t)
  }

  test("put, get, overwrite") {
    val t = MerkleSearchTree("a" -> 1, "b" -> 2, "c" -> 3).put("b", 20)
    check(t, Map("a" -> 1, "b" -> 20, "c" -> 3))
    assertEquals(t.get("d"), None)
  }

  test("shape is independent of insertion order") {
    val rnd     = Random(42)
    val entries = randomEntries(rnd, 2000)
    val a       = build(entries.toList.sortBy(_._1))
    val b       = build(rnd.shuffle(entries.toList))
    val c       = build(entries.toList.sortBy(_._1).reverse)
    check(a, entries)
    assertEquals(a.rootHash, b.rootHash)
    assertEquals(a.rootHash, c.rootHash)
    assert(a.root.asInstanceOf[Node[String, Int]].level >= 1, "expected more than one level for 2000 keys")
  }

  test("remove yields the same tree as never inserting") {
    val rnd      = Random(7)
    val entries  = randomEntries(rnd, 1500)
    val removed  = rnd.shuffle(entries.keys.toList).take(700)
    val full     = build(rnd.shuffle(entries.toList))
    val afterDel = removed.foldLeft(full)(_.remove(_))
    val expected = entries -- removed
    check(afterDel, expected)
    assertEquals(afterDel.rootHash, build(expected).rootHash)
    assertEquals(afterDel.remove("not a key"), afterDel)

    val all = entries.keys.foldLeft(full)(_.remove(_))
    assert(all.isEmpty)
    assertEquals(all.rootHash, MerkleSearchTree.empty[String, Int].rootHash)
  }

  test("different contents have different hashes") {
    val t = MerkleSearchTree("a" -> 1, "b" -> 2)
    assertNotEquals(t.rootHash, t.put("b", 3).rootHash)
    assertNotEquals(t.rootHash, t.put("c", 3).rootHash)
    assertNotEquals(t.rootHash, t.remove("a").rootHash)
  }

  test("range") {
    val rnd     = Random(3)
    val entries = randomEntries(rnd, 1000)
    val t       = build(entries)
    val sorted  = entries.toList.sortBy(_._1)
    for _ <- 1 to 200 do
        val List(from, until) = List.fill(2)(rnd.alphanumeric.take(1 + rnd.nextInt(3)).mkString).sorted: @unchecked
        assertEquals(t.range(from, until).toList, sorted.filter((k, _) => k >= from && k < until), s"[$from, $until)")
    assertEquals(t.range("z", "a").toList, Nil)
  }

  test("merge is pointwise and canonical") {
    val rnd   = Random(11)
    val left  = randomEntries(rnd, 800)
    val right = randomEntries(rnd, 800) ++ left.take(300).view.mapValues(_ + 1)
    val a     = build(left)
    val b     = build(right)

    val expected = (left.keySet ++ right.keySet).map { k =>
      k -> (left.get(k).toList ++ right.get(k).toList).max
    }.toMap

    val ab = Lattice.merge(a, b)
    val ba = Lattice.merge(b, a)
    check(ab, expected)
    assertEquals(ab.rootHash, build(expected).rootHash)
    assertEquals(ab, ba)
    assertEquals(Lattice.merge(ab, a), ab)
    assert(Lattice.merge(a, a) eq a)
    assertEquals(Lattice.merge(a, MerkleSearchTree.empty[String, Int]), a)
    assertEquals(Lattice.merge(MerkleSearchTree.empty[String, Int], a), a)
  }

  test("merge is associative") {
    val rnd           = Random(5)
    val List(a, b, c) = List.fill(3)(build(randomEntries(rnd, 300))): @unchecked
    assertEquals(Lattice.merge(Lattice.merge(a, b), c), Lattice.merge(a, Lattice.merge(b, c)))
  }

  test("update merges values") {
    val t = MerkleSearchTree("a" -> 5).update("a", 3).update("b", 1)
    assertEquals(t.toMap, Map("a" -> 5, "b" -> 1))
    assertEquals(t.update("a", 9).get("a"), Some(9))
  }
}
