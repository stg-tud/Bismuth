package test.rdts.bespoke

import org.scalacheck.{Arbitrary, Gen}
import rdts.base.LocalUid.asId
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.EnableWinsFlag
import rdts.dotted.HasDots.*
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.DataGenerator.*

class SyntaxTest extends munit.FunSuite {

  test("Manual Tests") {

    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given LocalUid                   = "me".asId

    assert(!flag.data.read)
    val enabled = flag.modn(_.enable())
    assert(enabled.data.read)
    val disabled = enabled.modn(_.disable())
    assert(!disabled.data.read)

  }

}
