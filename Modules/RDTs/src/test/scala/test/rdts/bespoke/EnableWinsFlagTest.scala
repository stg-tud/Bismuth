package test.rdts.bespoke

import rdts.time.Dots
import rdts.base.{Bottom, LocalUid}
import rdts.base.Historized.MetaDelta
import rdts.datatypes.EnableWinsFlag

class EnableWinsFlagTest extends munit.FunSuite {

  given stringBottom: Bottom[String] with {
    override def empty: String = ""
  }

  test("new unset overrides all previous deltas") {
    import EnableWinsFlag.given

    val localId: LocalUid = LocalUid.gen()
    var dots              = Dots.empty
    val dot1              = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)
    val dot4 = dots.nextDot(using localId)
    dots = dots.add(dot4)
    val dot5 = dots.nextDot(using localId)
    dots = dots.add(dot5)

    var ewFlag = EnableWinsFlag.empty
    val delta1 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta1
    val delta2 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta2
    val delta3 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta3
    val delta4 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta4
    val delta5 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta5

    val buffer: List[MetaDelta[EnableWinsFlag]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot2), delta2),
      MetaDelta(Dots.single(dot3), delta3),
      MetaDelta(Dots.single(dot4), delta4),
      MetaDelta(Dots.single(dot5), delta5),
    )

    val delta           = ewFlag.disable()
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, dots)

    assert(!delta2.isRedundant(delta1))

    assert(delta3.isRedundant(delta1))
    assert(delta3.isRedundant(delta2))

    assert(delta4.isRedundant(delta1))
    assert(delta4.isRedundant(delta2))
    assert(delta4.isRedundant(delta3))

    assert(!delta5.isRedundant(delta1))
    assert(!delta5.isRedundant(delta2))
    assert(!delta5.isRedundant(delta3))
    assert(!delta5.isRedundant(delta4))
  }

  test("new unset overrides all deltas containing a subset of the observed dots") {
    import EnableWinsFlag.given

    val localId: LocalUid = LocalUid.gen()
    var dots              = Dots.empty
    val dot1              = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)
    val dot4 = dots.nextDot(using localId)
    dots = dots.add(dot4)
    val dot5 = dots.nextDot(using localId)
    dots = dots.add(dot5)

    var ewFlag = EnableWinsFlag.empty
    val delta1 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta1
    val delta2 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta2
    val delta3 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta3
    val delta4 = ewFlag.disable()
    val delta5 = ewFlag.enable(using localId)()
    ewFlag = ewFlag `merge` delta5

    val buffer: List[MetaDelta[EnableWinsFlag]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot2), delta2),
      MetaDelta(Dots.single(dot3), delta3),
      MetaDelta(Dots.single(dot5), delta5),
    )

    val redundantDeltas = buffer.getRedundantDeltas(delta4)

    assertEquals(redundantDeltas, dots.subtract(Dots.single(dot4).union(Dots.single(dot5))))
  }

  test("subsumption previous from same replica") {
    import EnableWinsFlag.given

    val localUid1 = LocalUid.gen()
    var ewFlag    = EnableWinsFlag.empty
    var dots      = Dots.empty

    val delta1 = ewFlag.enable(using localUid1)()
    ewFlag = ewFlag `merge` delta1
    val dot1 = dots.nextDot(using localUid1)
    dots = dots.add(dot1)

    println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta2 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta2
    val dot2 = dots.nextDot(using localUid1)
    dots = dots.add(dot2)

    println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta3 = ewFlag.enable(using localUid1)()
    ewFlag = ewFlag `merge` delta3
    val dot3 = dots.nextDot(using localUid1)
    dots = dots.add(dot3)

    println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta4 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta4
    val dot4 = dots.nextDot(using localUid1)
    dots = dots.add(dot4)

    println(f"${ewFlag.read}, ${ewFlag.set}")

    println(delta1)
    println(delta2)
    println(delta3)
    println(delta4)

    println("delta2")
    println(f"${delta2 `subsumes` delta1} ${delta2.isRedundant(delta1)}")
    println("delta3")
    println(f"${delta3 `subsumes` delta1} ${delta3.isRedundant(delta1)}")
    println(f"${delta3 `subsumes` delta2} ${delta3.isRedundant(delta2)}")
    println("delta4")
    println(f"${delta4 `subsumes` delta1} ${delta4.isRedundant(delta1)}")
    println(f"${delta4 `subsumes` delta2} ${delta4.isRedundant(delta2)}")
    println(f"${delta4 `subsumes` delta3} ${delta4.isRedundant(delta3)}")
  }

}
