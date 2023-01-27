package de.tu_darmstadt.stg.daimpl
package causality

import org.scalacheck.Gen

object IntervalTreeClockGenerators {
  given genIntervalTreeClock: Gen[IntervalTreeClock] = for {
    id <- IdTreeGenerators.genIdTree
    event <- EventTreeGenerators.genEventTree
  } yield IntervalTreeClock(id, event)
}
