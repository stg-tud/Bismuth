package com.github.ckuessner
package causality

import causality.IntervalTreeClockGenerators.genIntervalTreeClock

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IntervalTreeClockTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(maxDiscardedFactor = 20.0)

  "NormalForm[IntervalTreeClock]" should "work" in {
    forAll(genIntervalTreeClock) { itc =>
      val normalizedItc = itc.normalized
      normalizedItc.idTree shouldBe itc.idTree.normalized
      normalizedItc.eventTree shouldBe itc.eventTree.normalized
    }
  }

  "PartialOrdering[IntervalTreeClock]" should "work" in {
    forAll(genIntervalTreeClock, genIntervalTreeClock) { (l, r) =>
      summon[PartialOrdering[IntervalTreeClock]].tryCompare(l, r) shouldBe
        summon[PartialOrdering[EventTree]].tryCompare(l.eventTree, r.eventTree)
    }
  }
}
