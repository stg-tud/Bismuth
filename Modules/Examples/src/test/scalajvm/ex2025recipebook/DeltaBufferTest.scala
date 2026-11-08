package ex2025recipebook

import rdts.base.Redundancy.MetaDelta
import rdts.base.Uid
import rdts.time.{Dot, Dots}

class DeltaBufferTest extends munit.FunSuite {

  private val uid                = Uid.predefined("alice")
  private def id(time: Int): Dots = Dots.single(Dot(uid, time))

  // uses the default subsumption-based Redundancy for Set[Int]
  private val d1 = MetaDelta(id(1), Set(1))
  private val d2 = MetaDelta(id(2), Set(1, 2))
  private val d3 = MetaDelta(id(3), Set(3))

  test("NonRedundant (MetaDelta overload) remembers redundant dots and drops stale redeliveries") {
    val buffer = DeltaBufferNonRedundant[Set[Int]]()
      .applyDelta(d1)
      .applyDelta(d2) // makes d1 redundant
      .applyDelta(d1) // stale redelivery of d1
      .applyDelta(d3) // unrelated, should prune d1 again

    assertEquals(buffer.getDeltas(Dots.empty).map(_.id), Set(id(2), id(3)))
  }

  test("NonRedundant (delta, id overload) remembers redundant dots and drops stale redeliveries") {
    val buffer = DeltaBufferNonRedundant[Set[Int]]()
      .applyDelta(d1.delta, d1.id)
      .applyDelta(d2.delta, d2.id)
      .applyDelta(d1.delta, d1.id)
      .applyDelta(d3.delta, d3.id)

    assertEquals(buffer.getDeltas(Dots.empty).map(_.id), Set(id(2), id(3)))
  }
}
