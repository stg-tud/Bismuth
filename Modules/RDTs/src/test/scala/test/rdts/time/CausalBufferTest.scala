package test.rdts.time

import rdts.base.Uid
import rdts.time.{Causal, CausalBuffer, Dot, Dots}

class CausalBufferTest extends munit.FunSuite {

  val a: Uid = Uid.predefined("a")
  val b: Uid = Uid.predefined("b")

  def d(uid: Uid, t: Long): Dots = Dots.single(Dot(uid, t))

  test("applies in causal order and drops duplicates") {
    val first  = Causal(Dots.empty, "a0")
    val second = Causal(d(a, 0), "a1")
    val third  = Causal(d(a, 0).union(d(a, 1)), "b0")

    val (b1, r1) = CausalBuffer[String]().receive(d(b, 0), third)
    assertEquals(r1, Nil)
    val (b2, r2) = b1.receive(d(a, 1), second)
    assertEquals(r2, Nil)
    assertEquals(b2.waiting, d(b, 0).union(d(a, 1)))
    val (b3, r3) = b2.receive(d(a, 0), first)
    assertEquals(r3, List("a0", "a1", "b0"))
    assertEquals(b3.pending, Nil)

    val (b4, r4) = b3.receive(d(a, 1), second)
    assertEquals(r4, Nil)
    assertEquals(b4, b3)
  }

  test("pending duplicates are ignored") {
    val delta    = Causal(d(a, 0), "x")
    val (b1, _)  = CausalBuffer[String]().receive(d(b, 0), delta)
    val (b2, r2) = b1.receive(d(b, 0), delta)
    assertEquals(r2, Nil)
    assertEquals(b2.pending.size, 1)
  }

  test("markApplied releases deltas waiting on local dots") {
    val (b1, r1) = CausalBuffer[String]().receive(d(b, 0), Causal(d(a, 0), "reply"))
    assertEquals(r1, Nil)
    val (b2, r2) = b1.markApplied(d(a, 0))
    assertEquals(r2, List("reply"))
    assertEquals(b2.applied, d(a, 0).union(d(b, 0)))
  }

  test("predecessors may include own dots (merged deltas)") {
    val merged   = d(a, 0).union(d(a, 1))
    val (_, ready) = CausalBuffer[String]().receive(merged, Causal(d(a, 0), "merged"))
    assertEquals(ready, List("merged"))
  }
}
