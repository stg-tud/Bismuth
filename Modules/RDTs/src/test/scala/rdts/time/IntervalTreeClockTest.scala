package rdts.time

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import rdts.time.IntervalTreeClockGenerators.genIntervalTreeClock
import org.scalacheck.Test.Parameters

class IntervalTreeClockTest extends ScalaCheckSuite {

  override def scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters.withMaxDiscardRatio(20)

  property("NormalForm[IntervalTreeClock]") {
    forAll(genIntervalTreeClock) { itc =>
      val normalizedItc = itc.normalize
      assertEquals(normalizedItc.idTree, itc.idTree.normalize)
      assertEquals(normalizedItc.eventTree, itc.eventTree.normalize)
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
