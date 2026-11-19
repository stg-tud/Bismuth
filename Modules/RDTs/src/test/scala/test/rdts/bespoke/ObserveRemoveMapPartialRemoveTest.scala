package test.rdts.bespoke

import rdts.base.{ReplicaId, Uid}
import rdts.datatypes.{MultiVersionRegister, ObserveRemoveMap}

/** Keys with multiple dots (every update adds a dot), and removes that only observed some of them. */
class ObserveRemoveMapPartialRemoveTest extends munit.FunSuite {

  type M = ObserveRemoveMap[String, MultiVersionRegister[Int]]

  val a: ReplicaId = ReplicaId(Uid.predefined("a"))
  val b: ReplicaId = ReplicaId(Uid.predefined("b"))

  def write(base: M, v: Int)(using r: ReplicaId): M =
    base.update("k", base.get("k").getOrElse(MultiVersionRegister.empty[Int]).write(v))

  def read(m: M): Option[Set[Int]] = m.get("k").map(_.read)

  /** all ways of merging the given deltas (all permutations, left fold and right fold) */
  def assertConsistent(deltas: List[M], expected: Option[Set[Int]])(using munit.Location): Unit =
    val results = deltas.permutations.toList.flatMap { perm =>
      List(perm.reduceLeft(_ `merge` _), perm.reduceRight(_ `merge` _))
    }
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

  test("every update adds a fresh dot and keeps the existing ones") {
    val state  = empty `merge` deltaA
    val update = write(state, 3)(using b)
    assertEquals(update.inner("k").keySet.intersect(state.inner("k").keySet), Set.empty)
    val after = state `merge` update
    assertEquals(after.inner("k").size, 2)
    assertEquals(read(after), Some(Set(3)))
  }

  test("update wins against a concurrent remove, with only the value of the update") {
    val state  = empty `merge` deltaA
    val update = write(state, 3)(using b)
    val remove = state.remove("k")
    assertConsistent(List(deltaA, update, remove), Some(Set(3)))
  }

  test("remove of all observed dots removes the key") {
    val state  = (empty `merge` deltaA) `merge` write(empty `merge` deltaA, 3)(using b)
    val remove = state.remove("k")
    assertEquals(state `merge` remove, empty `merge` remove)
    assertEquals(read(state `merge` remove), None)
  }

  test("lattice laws") {
    val ab     = deltaA `merge` deltaB
    val states = List(empty, deltaA, deltaB, removeA, ab, ab.remove("k"), write(ab, 3)(using a), deltaA `merge` removeA)
    for x <- states; y <- states do
        assertEquals(x `merge` y, y `merge` x, s"comm: $x $y")
        assertEquals(x `merge` x, x, s"idem: $x")
        for z <- states do
            assertEquals((x `merge` y) `merge` z, x `merge` (y `merge` z), s"assoc: $x $y $z")
  }
}
