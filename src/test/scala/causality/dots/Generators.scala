package de.tu_darmstadt.stg.daimpl
package causality.dots

import causality.dots.Defs.{Id, Time}
import causality.dots.impl.ArrayRanges

import org.scalacheck.Gen

object Generators {
  given dotGen: Gen[Dot] = for {
    time      <- Gen.posNum[Long]
    replicaId <- Gen.long
  } yield Dot(replicaId, time)

  given arrayRangesGen: Gen[ArrayRanges] = for {
    ranges <- Gen.listOf(timeRangeGen)
  } yield ArrayRanges(ranges)

  given timeRangeGen: Gen[(Time, Time)] = for {
    a <- Gen.posNum[Time]
    b <- Gen.posNum[Time]
    lower = Math.min(a, b)
    upper = Math.max(a, b)
  } yield (lower, upper)

  given Gen[DotSet] = for {
    ids    <- Gen.listOf(Gen.posNum[Time])
    ranges <- Gen.listOfN(ids.size, arrayRangesGen)
  } yield DotSet(ids.zip(ranges).toMap)

  given VectorClockGen: Gen[VectorClock] = for {
    ids    <- Gen.listOf(Gen.posNum[Time])
    clocks <- Gen.listOfN(ids.size, Gen.posNum[Time])
  } yield VectorClock(ids.zip(clocks).toMap)

  given VectorClockStampGen: Gen[(Id, VectorClock)] = for {
    id <- Gen.posNum[Id]
    vc <- VectorClockGen
  } yield (id, vc)
}
