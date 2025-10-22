package rdts.size

import com.github.plokhotnyuk.jsoniter_scala.core
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.ObserveRemoveMap
import replication.JsoniterCodecs.given

class SizeTest extends munit.FunSuite {

  test("size does not grow indefinetely") {
    given Lattice[Int] = math.max

    given LocalUid = LocalUid.gen()

    val empty = ObserveRemoveMap.empty[String, Int]

    val oneThousand = (1 to 1000).foldLeft(empty) { (state, _) =>
      val added = state `merge` state.update("k", 1)
      added `merge` added.remove("k")
    }

    val twoThousand = (1 to 1000).foldLeft(oneThousand) { (state, _) =>
      val added = state `merge` state.update("k", 1)
      added `merge` added.remove("k")
    }

    given JsonValueCodec[ObserveRemoveMap[String, Int]] = JsonCodecMaker.make

    val emptyjson       = core.writeToArray(empty)
    val oneThousandJson = core.writeToArray(oneThousand)
    val twoThousandJson = core.writeToArray(twoThousand)

    assertEquals(oneThousand.removed.size, 1000L)
    assertEquals(twoThousand.removed.size, 2000L)

    assertEquals(oneThousandJson.length, twoThousandJson.length)
  }

}
