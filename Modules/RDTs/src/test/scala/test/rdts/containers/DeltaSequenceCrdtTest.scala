package test.rdts.containers

import rdts.datatypes.alternatives.rga.{DeltaSequence, Vertex}
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import test.rdts.given

class DeltaSequenceCrdtTest extends munit.FunSuite {

  test("basic interaction") {
    val ds                                         = DeltaBuffer(DeltaSequence.empty[String])
    val a                                          = "a"
    val vertex1                                    = Vertex.fresh()
    val vertex2                                    = Vertex.fresh()
    val added2: DeltaBuffer[DeltaSequence[String]] = ds
      .mod(_.addRightDelta(a, Vertex.start, vertex1, "Hello world!"))
      .mod(_.addRightDelta(a, Vertex.start, vertex2, "Hello world 2!"))
    assertEquals(added2.state.iterator.toList, List("Hello world 2!", "Hello world!"), "add right is correctly ordered")

    val combined = added2.mod(_.addRightDelta(a, vertex2, Vertex.fresh(), "Hello world after 2!"))

    assertEquals(combined.state.iterator.toList, List("Hello world 2!", "Hello world after 2!", "Hello world!"))

    val combined2 = added2.mod(_.addRightDelta(a, vertex2, Vertex.fresh(), "Hello world after 2 the second!"))

    assertEquals(
      combined2.state.iterator.toList,
      List("Hello world 2!", "Hello world after 2 the second!", "Hello world!")
    )

  }

}
