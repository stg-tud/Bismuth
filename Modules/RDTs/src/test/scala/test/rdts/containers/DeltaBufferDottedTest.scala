package test.rdts.containers

import rdts.base.{Bottom, LocalUid}
import rdts.datatypes.EnableWinsFlag
import rdts.syntax.DeltaBuffer

class DeltaBufferDottedTest extends munit.FunSuite {

  test("basic interaction") {

    given LocalUid = LocalUid.gen()

    val dbe = DeltaBuffer[EnableWinsFlag](EnableWinsFlag.empty)

    assertEquals(dbe.state, Bottom.empty[EnableWinsFlag])
    assert(!dbe.state.read)
    assertEquals(dbe.deltaBuffer, List.empty)

    // two enables produce the exact same result, so do not cause delta buffer to grow
    val dis = dbe.mod(_.enable()).mod(_.enable())
    assert(dis.state.read)
    val en = dis.mod(_.disable())

    assert(!en.state.read)
    assertEquals(en.deltaBuffer.size, 3)

  }

}
