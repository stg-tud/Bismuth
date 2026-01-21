package test.rdts.bespoke

import rdts.time.Dots
import rdts.base.{Bottom, LocalUid}
import rdts.base.Historized.MetaDelta
import rdts.datatypes.GrowOnlyCounter

class GrowOnlyCounterTest extends munit.FunSuite {

  given stringBottom: Bottom[String] with {
    override def empty: String = ""
  }

  test("single replica: delta marks deltas with smaller value as redundant") {
    import GrowOnlyCounter.given

    val localId: LocalUid = LocalUid.gen()
    var dots              = Dots.empty
    val dot1              = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)

    var counter = GrowOnlyCounter.zero

    val delta1 = counter.inc()(using localId)
    counter = counter `merge` delta1
    val delta2 = counter.inc()(using localId)
    counter = counter `merge` delta2
    val delta3 = counter.inc()(using localId)
    counter = counter `merge` delta3

    val buffer: List[MetaDelta[GrowOnlyCounter]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot2), delta2),
      MetaDelta(Dots.single(dot3), delta3)
    )

    val delta           = counter.inc()(using localId)
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, dots)
  }

  test(
    "single replica: delta marks deltas containing smaller counter value as redundant but not deltas containing larger counter values"
  ) {
    import GrowOnlyCounter.given

    val localId: LocalUid = LocalUid.gen()
    var dots              = Dots.empty
    val dot1              = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)

    var counter = GrowOnlyCounter.zero

    val delta1 = counter.inc()(using localId)
    counter = counter `merge` delta1
    val delta2 = counter.inc()(using localId)
    counter = counter `merge` delta2
    val delta3 = counter.inc()(using localId)
    counter = counter `merge` delta3

    val buffer: List[MetaDelta[GrowOnlyCounter]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot3), delta3)
    )

    val redundantDeltas = buffer.getRedundantDeltas(delta2)

    assertEquals(redundantDeltas, Dots.single(dot1))
  }

  test(
    "multiple replica: delta marks deltas with smaller value from same replica as redundant but not from other replicas"
  ) {
    import GrowOnlyCounter.given

    val localId1: LocalUid = LocalUid.gen()
    val localId2: LocalUid = LocalUid.gen()
    var dots               = Dots.empty
    val dot11              = dots.nextDot(using localId1)
    dots = dots.add(dot11)
    val dot21 = dots.nextDot(using localId2)
    dots = dots.add(dot21)
    val dot12 = dots.nextDot(using localId1)
    dots = dots.add(dot21)

    var counter = GrowOnlyCounter.zero

    val delta1 = counter.inc()(using localId1)
    counter = counter `merge` delta1
    val delta2 = counter.inc()(using localId2)
    counter = counter `merge` delta2
    val delta3 = counter.inc()(using localId1)
    counter = counter `merge` delta3

    val buffer: List[MetaDelta[GrowOnlyCounter]] = List(
      MetaDelta(Dots.single(dot11), delta1),
      MetaDelta(Dots.single(dot21), delta2),
      MetaDelta(Dots.single(dot12), delta3)
    )

    val delta           = counter.inc()(using localId2)
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, Dots.single(dot21))
  }

}
