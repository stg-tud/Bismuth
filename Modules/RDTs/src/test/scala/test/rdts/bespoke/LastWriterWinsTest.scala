package test.rdts.bespoke

import rdts.time.Dots
import rdts.base.{Bottom, LocalUid}
import rdts.base.Historized.MetaDelta
import rdts.datatypes.LastWriterWins

class LastWriterWinsTest extends munit.FunSuite {

  given stringBottom: Bottom[String] with {
    override def empty: String = ""
  }

  test("basic write") {
    import LastWriterWins.given

    val lww1 = LastWriterWins.now("Hello World")
    val lww2 = lww1.write("Hello Distributed World")

    assertNotEquals(lww1.timestamp, lww2.timestamp)

    val merged = lww1 `merge` lww2

    assertEquals(merged.read, "Hello Distributed World")
  }

  test("newer initial value overwrites an earlier initial value") {
    import LastWriterWins.given

    val lww1 = LastWriterWins.now("Hello World")
    val lww2 = LastWriterWins.now("Hello Distributed World")

    assertNotEquals(lww1.timestamp, lww2.timestamp)

    val merged = lww1 `merge` lww2

    assertEquals(merged.read, "Hello Distributed World")
  }

  test("newer Bottom does not overwrite an earlier initial value") {
    import LastWriterWins.given

    val lww1 = LastWriterWins.now("Hello Distributed World")
    val lww2 = Bottom[LastWriterWins[String]].empty

    assertNotEquals(lww1.timestamp, lww2.timestamp)

    val merged = lww1 `merge` lww2

    assertEquals(merged.read, "Hello Distributed World")
  }
  
  test("newer delta marks older deltas as redundant") {
    import LastWriterWins.given
    
    val localId: LocalUid = LocalUid.gen()
    var dots = Dots.empty
    val dot1 = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)
    
    val buffer: List[MetaDelta[LastWriterWins[String]]] = List(
      MetaDelta(Dots.single(dot1), LastWriterWins.now("Hello World")),
      MetaDelta(Dots.single(dot2), LastWriterWins.now("Hello Distributed World")),
      MetaDelta(Dots.single(dot3), LastWriterWins.now("Bye World"))
    )
    
    val delta = LastWriterWins.now("Bye Distributed World")
    val redundantDeltas = delta.getRedundantDeltas(buffer)
    
    assertEquals(redundantDeltas, dots)
  }

  test("delta marks older deltas as redundant but not newer") {
    import LastWriterWins.given

    val localId: LocalUid = LocalUid.gen()
    var dots = Dots.empty
    val dot1 = dots.nextDot(using localId)
    dots = dots.add(dot1)
    val dot2 = dots.nextDot(using localId)
    dots = dots.add(dot2)
    val dot3 = dots.nextDot(using localId)
    dots = dots.add(dot3)

    val delta1 = LastWriterWins.now("Hello World")
    val delta2 = LastWriterWins.now("Hello Distributed World")
    val delta3 = LastWriterWins.now("Bye World")
    
    val buffer: List[MetaDelta[LastWriterWins[String]]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot3), delta3)
    )
    
    val redundantDeltas: Dots = delta2.getRedundantDeltas(buffer)

    assertEquals(redundantDeltas, Dots.single(dot1))
  }

}
