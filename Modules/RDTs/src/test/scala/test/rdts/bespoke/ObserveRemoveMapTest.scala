package test.rdts.bespoke

import rdts.time.Dots
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.base.Historized.MetaDelta
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.time.Dot

class ObserveRemoveMapTest extends munit.FunSuite {

  given Lattice[Dot] = Lattice.assertEquals

  test("basic usage") {
    val obremmap = ObserveRemoveMap.empty[String, Dot]

    given replicaId: LocalUid = LocalUid.gen()

    val added = {
      val nextDot = obremmap.observed.nextDot(replicaId.uid)
      obremmap.update("Hi!", nextDot)
    }

    assert(added.contains("Hi!"))

    val remove = added.remove("Hi!")

    val merged = added `merge` remove

    assertEquals(merged.entries.toMap, Map.empty)
  }

  test("redundancy test 1") {
    given Bottom[String] = Bottom.provide("")

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

    var orMap = ObserveRemoveMap.empty[String, LastWriterWins[String]]
    def produceDelta(key: String, value: String): ObserveRemoveMap[String, LastWriterWins[String]] = {
      val nextDot = orMap.observed.nextDot(localId.uid)
      orMap.update(key, LastWriterWins.empty[String].write(value))(using localId)
    }

    val delta1 = produceDelta("a", "a")
    orMap = orMap `merge` delta1
    val delta2 = produceDelta("b", "b")
    orMap = orMap `merge` delta2
    val delta3 = produceDelta("c", "c")
    orMap = orMap `merge` delta3
    val delta4 = produceDelta("d", "d")
    orMap = orMap `merge` delta4
    val delta5 = produceDelta("e", "e")
    orMap = orMap `merge` delta5

    val buffer: List[MetaDelta[ObserveRemoveMap[String, LastWriterWins[String]]]] = List(
      MetaDelta(Dots.single(dot1), delta1),
      MetaDelta(Dots.single(dot2), delta2),
      MetaDelta(Dots.single(dot3), delta3),
      MetaDelta(Dots.single(dot4), delta4),
      MetaDelta(Dots.single(dot5), delta5),
    )

    val delta           = produceDelta("a", "ab")
    val redundantDeltas = buffer.getRedundantDeltas(delta)

    assertEquals(redundantDeltas, Dots.single(dot1))
  }

}
