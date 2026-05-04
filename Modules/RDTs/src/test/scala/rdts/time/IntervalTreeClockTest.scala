package rdts.time

import rdts.time.IntervalTreeClockGenerators.genIntervalTreeClock
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class IntervalTreeClockTest extends ScalaCheckSuite {

  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMaxDiscardRatio(20)

  property("NormalForm[IntervalTreeClock]") {
    forAll(genIntervalTreeClock) { itc =>
      val normalizedItc = itc.normalized
      assertEquals(normalizedItc.idTree, itc.idTree.normalized)
      assertEquals(normalizedItc.eventTree, itc.eventTree.normalized)
    }
  }

  property("PartialOrdering[IntervalTreeClock]") {
    forAll(genIntervalTreeClock, genIntervalTreeClock) { (l, r) =>
      assertEquals(
        summon[PartialOrdering[IntervalTreeClock]].tryCompare(l, r),
        summon[PartialOrdering[EventTree]].tryCompare(l.eventTree, r.eventTree)
      )
    }
  }
}
