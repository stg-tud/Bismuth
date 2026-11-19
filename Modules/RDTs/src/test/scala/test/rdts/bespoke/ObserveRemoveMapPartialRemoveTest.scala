package test.rdts.bespoke

import rdts.base.{Lattice, ReplicaId, Uid}
import rdts.datatypes.{MultiVersionRegister, ObserveRemoveMap}
import rdts.time.Dot

/** Keys with multiple dots (concurrent inserts of the same key), removes that only observed some of them,
  * and the collapse of multiple dots on update.
  */
class ObserveRemoveMapPartialRemoveTest extends munit.FunSuite {

  type M = ObserveRemoveMap[String, MultiVersionRegister[Int]]

  // fixed ids so that the dot order is known: a < b
  val a: ReplicaId = ReplicaId(Uid.predefined("a"))
  val b: ReplicaId = ReplicaId(Uid.predefined("b"))

  def write(base: M, v: Int)(using r: ReplicaId): M =
    base.update("k", base.get("k").getOrElse(MultiVersionRegister.empty[Int]).write(v))

  def read(m: M): Option[Set[Int]] = m.get("k").map(_.read)

  /** all ways of merging the given deltas (all permutations, left fold and right fold) */
  def allMergeOrders(deltas: List[M]): List[M] =
    deltas.permutations.toList.flatMap { perm =>
      List(perm.reduceLeft(_ `merge` _), perm.reduceRight(_ `merge` _))
    }

  def assertConsistent(deltas: List[M], expected: Option[Set[Int]])(using munit.Location): Unit =
    val results = allMergeOrders(deltas)
    assertEquals(results.map(read).toSet, Set(expected))
    assertEquals(results.toSet.size, 1, s"states differ: ${results.toSet}")

  // replica a writes 1, removes it (without having seen b); replica b concurrently writes 2
  val empty: M   = ObserveRemoveMap.empty
  val deltaA: M  = write(empty, 1)(using a)
  val removeA: M = deltaA.remove("k")
  val deltaB: M  = write(empty, 2)(using b)

  test("concurrent inserts keep both dots, merged on read") {
    val merged = deltaA `merge` deltaB
    assertEquals(merged.inner("k").size, 2)
    assertEquals(read(merged), Some(Set(1, 2)))
  }

  test("partial remove: all merge orders agree, only the unobserved insert survives") {
    assertConsistent(List(deltaA, deltaB, removeA), Some(Set(2)))
  }

  test("update of a present key reuses its dot") {
    val state  = empty `merge` deltaA
    val update = write(state, 3)(using b)
    assertEquals(update.inner("k").keySet, state.inner("k").keySet)
    assertEquals(read(state `merge` update), Some(Set(3)))
  }

  test("remove wins against a concurrent update of a present key") {
    val state  = empty `merge` deltaA
    val update = write(state, 3)(using b)
    val remove = state.remove("k")
    assertConsistent(List(deltaA, update, remove), None)
  }

  test("insert wins against a concurrent remove that did not observe it") {
    val state    = empty `merge` deltaA
    val remove   = state.remove("k")
    val reinsert = write(empty, 4)(using b)
    assertConsistent(List(deltaA, remove, reinsert), Some(Set(4)))
  }

  test("re-insert after remove creates a fresh dot") {
    val state   = (empty `merge` deltaA) `merge` removeA
    val reinsert = write(state, 5)(using a)
    assert(!removeA.removed.contains(reinsert.dotsOf("k")))
    assertEquals(read(state `merge` reinsert), Some(Set(5)))
  }

  test("update collapses multiple dots into the newest one") {
    val state    = deltaA `merge` deltaB
    val newest   = state.inner("k").keys.max(using Dot.ordering)
    val collapse = write(state, 3)(using a)
    assertEquals(newest.place, b.uid)
    assertEquals(collapse.inner("k").keySet, Set(newest))
    assertEquals(collapse.removed, state.dotsOf("k").subtract(newest.dots))
    val after = state `merge` collapse
    assertEquals(after.inner("k").keySet, Set(newest))
    assertEquals(read(after), Some(Set(3)))
  }

  test("collapse is consistent with a concurrent remove of the older dot") {
    val state    = deltaA `merge` deltaB
    val collapse = write(state, 3)(using b)
    assertConsistent(List(deltaA, deltaB, removeA, collapse), Some(Set(3)))
  }

  test("collapse is consistent with a concurrent remove of all dots (remove wins)") {
    val state    = deltaA `merge` deltaB
    val collapse = write(state, 3)(using b)
    val remove   = state.remove("k")
    assertConsistent(List(deltaA, deltaB, collapse, remove), None)
  }

  test("collapse is consistent with a concurrent insert it did not observe") {
    val c: ReplicaId = ReplicaId(Uid.predefined("c"))
    val state        = deltaA `merge` deltaB
    val collapse     = write(state, 3)(using a)
    val deltaC       = write(empty, 7)(using c)
    assertConsistent(List(deltaA, deltaB, collapse, deltaC), Some(Set(3, 7)))
  }

  test("lattice laws") {
    val ab       = deltaA `merge` deltaB
    val collapse = write(ab, 3)(using a)
    val states   = List(empty, deltaA, deltaB, removeA, ab, ab.remove("k"), collapse, deltaA `merge` removeA)
    for x <- states; y <- states do
        assertEquals(x `merge` y, y `merge` x, s"comm: $x $y")
        assertEquals(x `merge` x, x, s"idem: $x")
        for z <- states do
            assertEquals((x `merge` y) `merge` z, x `merge` (y `merge` z), s"assoc: $x $y $z")
  }
}
