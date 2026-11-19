package test.rdts.bespoke

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.datatypes.{DotRemoveMap, LastWriterWins}
import rdts.time.Dots

class DotRemoveMapTest extends munit.FunSuite {

  type M = DotRemoveMap[LastWriterWins[String]]

  val a: ReplicaId = ReplicaId(Uid.predefined("a"))
  val b: ReplicaId = ReplicaId(Uid.predefined("b"))

  val empty: M = DotRemoveMap.empty

  def read(m: M): Set[String] = m.values.map(_.read).toSet

  /** all ways of merging the given deltas (all permutations, left fold and right fold) */
  def assertConsistent(deltas: List[M], expected: Set[String])(using munit.Location): Unit =
    val results = deltas.permutations.toList.flatMap { perm =>
      List(perm.reduceLeft(_ `merge` _), perm.reduceRight(_ `merge` _))
    }
    assertEquals(results.map(read).toSet, Set(expected))
    assertEquals(results.toSet.size, 1, s"states differ: ${results.toSet}")

  test("insert uses fresh ids") {
    val first  = empty.insert(LastWriterWins.now("x"))(using a)
    val state  = empty `merge` first
    val second = state.insert(LastWriterWins.now("y"))(using a)
    assertEquals(first.ids.size, 1)
    assertEquals(second.ids.size, 1)
    assertNotEquals(first.ids, second.ids)
    assertEquals(read(state `merge` second), Set("x", "y"))
  }

  test("nextId is the id used by insert") {
    val id = empty.nextId(using a)
    assertEquals(empty.insert(LastWriterWins.now("x"))(using a).ids, Set(id))
  }

  test("concurrent inserts are distinct elements") {
    val x = empty.insert(LastWriterWins.now("x"))(using a)
    val y = empty.insert(LastWriterWins.now("y"))(using b)
    assertConsistent(List(x, y), Set("x", "y"))
  }

  test("update merges into the existing element") {
    val state  = empty `merge` empty.insert(LastWriterWins.now("x"))(using a)
    val id     = state.ids.head
    val update = state.transform(id)(_.write("x2"))
    assertEquals((state `merge` update).get(id).map(_.read), Some("x2"))
  }

  test("update or transform of a missing id does nothing") {
    val id = empty.nextId(using a)
    assertEquals(empty.update(id, LastWriterWins.now("x")), empty)
    assertEquals(empty.transform(id)(_.write("x")), empty)
  }

  test("remove wins against a concurrent update") {
    val insert = empty.insert(LastWriterWins.now("x"))(using a)
    val state  = empty `merge` insert
    val id     = state.ids.head
    val update = state.transform(id)(_.write("x2"))
    val remove = state.remove(id)
    assertConsistent(List(insert, update, remove), Set.empty)
  }

  test("remove only affects the removed element") {
    val x      = empty.insert(LastWriterWins.now("x"))(using a)
    val y      = empty.insert(LastWriterWins.now("y"))(using b)
    val remove = (empty `merge` x).remove(x.ids.head)
    assertConsistent(List(x, y, remove), Set("y"))
  }

  test("removed ids are never reused") {
    val insert = empty.insert(LastWriterWins.now("x"))(using a)
    val state  = (empty `merge` insert) `merge` insert.remove(insert.ids.head)
    assert(state.elements.isEmpty)
    val next = state.insert(LastWriterWins.now("y"))(using a)
    assert(!state.removed.contains(Dots.from(next.ids)))
    assertEquals(read(state `merge` next), Set("y"))
  }

  test("clear and removeBy") {
    val x     = empty.insert(LastWriterWins.now("x"))(using a)
    val y     = empty.insert(LastWriterWins.now("y"))(using b)
    val state = x `merge` y
    assertEquals(read(state `merge` state.removeBy(_.read == "x")), Set("y"))
    assertEquals(read(state `merge` state.clear()), Set.empty)
  }

  test("redundancy") {
    import rdts.base.Redundancy
    val insert = empty.insert(LastWriterWins.now("x"))(using a)
    val state  = empty `merge` insert
    val id     = state.ids.head
    val update = state.transform(id)(_.write("x2"))
    assert(Redundancy[M].isRedundant(update, insert))
    assert(Redundancy[M].isRedundant(state.remove(id), insert))
    assert(!Redundancy[M].isRedundant(state.insert(LastWriterWins.now("y"))(using a), insert))
  }

  test("lattice laws") {
    val x      = empty.insert(LastWriterWins.now("x"))(using a)
    val y      = empty.insert(LastWriterWins.now("y"))(using b)
    val xy     = x `merge` y
    val states = List(empty, x, y, xy, xy.remove(x.ids.head), xy.transform(y.ids.head)(_.write("y2")), xy.clear())
    for s1 <- states; s2 <- states do
        assertEquals(s1 `merge` s2, s2 `merge` s1)
        assertEquals(s1 `merge` s1, s1)
        for s3 <- states do assertEquals((s1 `merge` s2) `merge` s3, s1 `merge` (s2 `merge` s3))
  }
}
