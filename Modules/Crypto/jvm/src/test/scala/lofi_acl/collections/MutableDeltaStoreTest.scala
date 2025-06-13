package lofi_acl.collections

import munit.FunSuite
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dot, Dots}

class MutableDeltaStoreTest extends FunSuite {

  private val a = Uid("a")
  private val b = Uid("b")
  private val c = Uid("c")
  private val d = Uid("d")

  test("writePrefix prunes deltas") {
    val store = MutableDeltaStore[Set[Int]]()

    store.writeIfNotPresent(Dots.single(Dot(a, 0)), Set(0))
    store.writeIfNotPresent(Dots.single(Dot(a, 1)), Set(1))
    store.writeIfNotPresent(Dots.single(Dot(a, 2)), Set(2))
    store.writeIfNotPresent(Dots.single(Dot(a, 5)), Set(5))
    store.writeIfNotPresent(Dots.single(Dot(b, 42)), Set(42))
    store.writeIfNotPresent(Dots.single(Dot(c, 21)), Set(21))
    store.writeIfNotPresent(Dots.single(Dot(c, 4711)), Set(4711))
    store.writeIfNotPresent(Dots.single(Dot(d, 7)), Set(7))

    val aRange = new ArrayRanges(Array(0, 6), 2)
    val bRange = new ArrayRanges(Array(0, 7), 2) // (b,42) is not included
    val cRange = new ArrayRanges(Array(4711, 4712, 10_000, 20_000), 4)
    // d is not included
    val prefixDots = Dots(Map(a -> aRange, b -> bRange, c -> cRange))
    store.writePrefix(prefixDots, Set.empty)

    assertEquals(store.readAvailableDeltas(Dots.single(Dot(d, 7))), Seq(Dots.single(Dot(d, 7)) -> Set(7)))
    assertEquals(store.readAvailableDeltas(prefixDots), Seq(prefixDots -> Set.empty))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 0))), Seq(prefixDots -> Set.empty))
    assertEquals(
      store.readAvailableDeltas(Dots.single(Dot(c, 10_022)).add(Dot(d, 7))),
      Seq(Dots.single(Dot(d, 7)) -> Set(7), prefixDots -> Set.empty)
    )
    assertEquals(
      store.readAvailableDeltas(Dots.single(Dot(c, 10_022)).add(Dot(d, 7)).add(Dot(b, 42))).toSet,
      Set(Dots.single(Dot(d, 7)) -> Set(7), prefixDots -> Set.empty, Dots.single(Dot(b, 42)) -> Set(42))
    )
  }

  test("write is readable without prefix") {
    val store = MutableDeltaStore[Set[Int]]()

    store.writeIfNotPresent(Dots.single(Dot(a, 0)), Set(0))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 0))), Seq(Dots.single(Dot(a, 0)) -> Set(0)))

    store.writeIfNotPresent(Dots.single(Dot(b, 42)), Set(42))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 0))), Seq(Dots.single(Dot(a, 0)) -> Set(0)))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(b, 42))), Seq(Dots.single(Dot(b, 42)) -> Set(42)))

    // Read combined
    assertEquals(
      store.readAvailableDeltas(Dots.single(Dot(b, 42)).add(Dot(a, 0))).toSet,
      Set(Dots.single(Dot(b, 42)) -> Set(42), Dots.single(Dot(a, 0)) -> Set(0))
    )
  }

  test("readAvailable ignores missing deltas") {
    val store = MutableDeltaStore[Set[Int]]()

    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 0))), Seq.empty)

    store.writeIfNotPresent(Dots.single(Dot(a, 0)), Set(0))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 1))), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(b, 0))), Seq.empty)

    // Only one of the two dots missing
    assertEquals(
      store.readAvailableDeltas(Dots.single(Dot(a, 0)).add(Dot(b, 1))),
      Seq(Dots.single(Dot(a, 0)) -> Set(0))
    )

    store.writeIfNotPresent(Dots.single(Dot(b, 21)), Set(21))
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(a, 1))), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(b, 0))), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dots.single(Dot(b, 20))), Seq.empty)

    // Works with prefix
    val aRange     = new ArrayRanges(Array(0, 6), 2)
    val bRange     = new ArrayRanges(Array(0, 7), 2) // (b,42) is not included
    val cRange     = new ArrayRanges(Array(4711, 4712, 10_000, 20_000), 4)
    val prefixDots = Dots(Map(a -> aRange, b -> bRange, c -> cRange))
    store.writePrefix(prefixDots, Set.empty)

    assertEquals(store.readAvailableDeltas(Dots.single(Dot(b, 42))), Seq.empty)
  }

}
