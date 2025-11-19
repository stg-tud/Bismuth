package test.rdts.bespoke


import rdts.datatypes.{GrowOnlySet, LastWriterWins}
import test.rdts.given

class GrowOnlySetTest extends munit.FunSuite {

  test("growing only uniquely") {
    var goSet = GrowOnlySet.empty[String]

    val delta0 = goSet.add("a")
    goSet = goSet `merge` delta0
    val delta1 = goSet.add("b")
    goSet = goSet `merge` delta1
    val delta2 = goSet.add("c")
    goSet = goSet `merge` delta2
    val delta3 = goSet.add("d")
    goSet = goSet `merge` delta3
    val delta4 = goSet.add("a")
    goSet = goSet `merge` delta4

    assertEquals(goSet.set, Set("a", "b", "c", "d"))
  }

  test("composed") {
    var goSet = GrowOnlySet.empty[LastWriterWins[String]]

    val lww0 = LastWriterWins.now("a")
    val lww1 = LastWriterWins.now("b")
    val lww2 = LastWriterWins.now("c")

    val delta0 = goSet.add(lww0)
    goSet = goSet `merge` delta0
    val delta1 = goSet.add(lww1)
    goSet = goSet `merge` delta1
    val delta2 = goSet.add(lww2)
    goSet = goSet `merge` delta2

    assertEquals(goSet.set, Set(lww0, lww1, lww2))

    val delta3 = lww0.write("aa")
    val delta4 = goSet.add(delta3)
    goSet = goSet `merge` delta4

    assertNotEquals(goSet.set, Set(lww0, lww1, lww2))
    assertEquals(goSet.set, Set(lww0, lww1, lww2, delta3))
  }

}
