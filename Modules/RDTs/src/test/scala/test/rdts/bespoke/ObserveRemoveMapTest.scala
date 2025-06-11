package test.rdts.bespoke

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.ObserveRemoveMap
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
}
