package test.rdts.bespoke

import rdts.time.Dots
import rdts.base.{Uid, LocalUid}
import rdts.datatypes.MultiVersionRegister
import rdts.base.Historized.MetaDelta

class MultiVersionRegisterTest extends munit.FunSuite {

  test("basic usage") {

    val a = MultiVersionRegister.empty[String]

    val alice   = Uid.predefined("alice")
    val bob     = Uid.predefined("bob")
    val charlie = Uid.predefined("charlie")

    val b = a.write("hi")(using alice.convert)
    val c = a.write("ho")(using bob.convert)

    val m1 = b `merge` c

    assertEquals(m1.read, Set("hi", "ho"))

    val d = m1.write("lets go!")(using charlie.convert)

    assertEquals(d.read, Set("lets go!"))

    assertEquals(m1 `merge` d, d `merge` b)

  }

  test("new write does not overrides all previous deltas") {
    import MultiVersionRegister.given

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

    var mvRegister = MultiVersionRegister.empty[String]
    val delta1     = mvRegister.write("a")(using localId)
    mvRegister = mvRegister `merge` delta1
    val delta2 = mvRegister.write("b")(using localId)
    mvRegister = mvRegister `merge` delta2
    val delta3 = mvRegister.write("c")(using localId)
    mvRegister = mvRegister `merge` delta3
    val delta4 = mvRegister.write("d")(using localId)
    mvRegister = mvRegister `merge` delta4
    val delta5 = mvRegister.write("e")(using localId)
    mvRegister = mvRegister `merge` delta5

    val buffer: List[MetaDelta[MultiVersionRegister[String]]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot2), delta2, Dots.single(dot1)),
      MetaDelta(Dots.single(dot3), delta3, Dots.from(List(dot1, dot2))),
      MetaDelta(Dots.single(dot4), delta4, Dots.from(List(dot1, dot2, dot3))),
      MetaDelta(Dots.single(dot5), delta5, Dots.from(List(dot1, dot2, dot3, dot4))),
    )

    val delta           = mvRegister.write("f")(using localId)
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, Dots.empty)
  }

}
