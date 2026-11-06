package test.rdts.time

import org.scalacheck.Gen
import rdts.time.IntervalTreeClock

object IntervalTreeClockGenerators {
  given genIntervalTreeClock: Gen[IntervalTreeClock] = for {
    id    <- IdTreeGenerators.genIdTree
    event <- EventTreeGenerators.genEventTree
  } yield IntervalTreeClock(id, event)
}
