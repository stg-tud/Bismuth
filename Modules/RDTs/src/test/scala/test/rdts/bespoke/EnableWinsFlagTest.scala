package test.rdts.bespoke

import rdts.base.Redundancy.MetaDelta
import rdts.base.{Bottom, Redundancy, ReplicaId}
import rdts.datatypes.EnableWinsFlag
import rdts.time.Dots

class EnableWinsFlagTest extends munit.FunSuite {

  given stringBottom: Bottom[String] with {
    override def empty: String = ""
  }

  test("new unset overrides all previous deltas") {
    import EnableWinsFlag.given

    val localId: ReplicaId = ReplicaId.gen()
    var dots               = Dots.empty
    val dot1               = dots.nextDot(using localId)
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

    assert(!Redundancy[EnableWinsFlag].isRedundant(delta2, delta1))

    assert(Redundancy[EnableWinsFlag].isRedundant(delta3, delta1))
    assert(Redundancy[EnableWinsFlag].isRedundant(delta3, delta2))

    assert(Redundancy[EnableWinsFlag].isRedundant(delta4, delta1))
    assert(Redundancy[EnableWinsFlag].isRedundant(delta4, delta2))
    assert(Redundancy[EnableWinsFlag].isRedundant(delta4, delta3))

    assert(!Redundancy[EnableWinsFlag].isRedundant(delta5, delta1))
    assert(!Redundancy[EnableWinsFlag].isRedundant(delta5, delta2))
    assert(!Redundancy[EnableWinsFlag].isRedundant(delta5, delta3))
    assert(!Redundancy[EnableWinsFlag].isRedundant(delta5, delta4))
  }

  test("new unset overrides all deltas containing a subset of the observed dots") {
    import EnableWinsFlag.given

    val localId: ReplicaId = ReplicaId.gen()
    var dots               = Dots.empty
    val dot1               = dots.nextDot(using localId)
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

    val localUid1 = ReplicaId.gen()
    var ewFlag    = EnableWinsFlag.empty
    var dots      = Dots.empty

    val delta1 = ewFlag.enable(using localUid1)()
    ewFlag = ewFlag `merge` delta1
    val dot1 = dots.nextDot(using localUid1)
    dots = dots.add(dot1)

    // println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta2 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta2
    val dot2 = dots.nextDot(using localUid1)
    dots = dots.add(dot2)

    // println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta3 = ewFlag.enable(using localUid1)()
    ewFlag = ewFlag `merge` delta3
    val dot3 = dots.nextDot(using localUid1)
    dots = dots.add(dot3)

    // println(f"${ewFlag.read}, ${ewFlag.set}")

    val delta4 = ewFlag.disable()
    ewFlag = ewFlag `merge` delta4
    val dot4 = dots.nextDot(using localUid1)
    dots = dots.add(dot4)

    // println(f"${ewFlag.read}, ${ewFlag.set}")

    // println(delta1)
    // println(delta2)
    // println(delta3)
    // println(delta4)

    // println("delta2")
    // println(f"${delta2 `subsumes` delta1} ${Redundancy[EnableWinsFlag].isRedundant(delta2, delta1)}")
    // println("delta3")
    // println(f"${delta3 `subsumes` delta1} ${Redundancy[EnableWinsFlag].isRedundant(delta3, delta1)}")
    // println(f"${delta3 `subsumes` delta2} ${Redundancy[EnableWinsFlag].isRedundant(delta3, delta2)}")
    // println("delta4")
    // println(f"${delta4 `subsumes` delta1} ${Redundancy[EnableWinsFlag].isRedundant(delta4, delta1)}")
    // println(f"${delta4 `subsumes` delta2} ${Redundancy[EnableWinsFlag].isRedundant(delta4, delta2)}")
    // println(f"${delta4 `subsumes` delta3} ${Redundancy[EnableWinsFlag].isRedundant(delta4, delta3)}")
  }

  test("disable makes a previous enable redundant even though it does not subsume it") {
    import EnableWinsFlag.given

    val localId: ReplicaId = ReplicaId.gen()
    val enable             = EnableWinsFlag.empty.enable(using localId)()
    val disable            = enable.disable()

    // the disable does not contain the enable dot in `set`, so lattice subsumption fails
    assert(!(disable `subsumes` enable))
    // but Redundancy considers the enable redundant, as the disable covers its observed dots
    assert(Redundancy[EnableWinsFlag].isRedundant(disable, enable))
    // and merging the redundant enable back in does not change the state
    assertEquals((disable `merge` enable).read, disable.read)
    // the reverse does not hold
    assert(!Redundancy[EnableWinsFlag].isRedundant(enable, disable))
  }

}
