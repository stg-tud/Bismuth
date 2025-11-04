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
    var dots = Dots.empty
    val dot1 = dots.nextDot(using localId)
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

    val delta = ewFlag.disable()
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, dots)
  }

  test("new unset overrides all deltas containing a subset of the observed dots") {
    import EnableWinsFlag.given

    val localId: LocalUid = LocalUid.gen()
    var dots = Dots.empty
    val dot1 = dots.nextDot(using localId)
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

}
