package de.tu_darmstadt.stg.daimpl
package causality

import causality.EventTree.seed
import causality.IdTreeGenerators.genIdTree

import org.scalacheck.{Arbitrary, Gen}

object EventTreeGenerators {
  given genEventTree: Gen[EventTree] = {
    for {
      numModifications <- Gen.choose(0, 20)
      eventTree        <- modifyEventTree(seed, numModifications)
    } yield eventTree
  }

  private def modifyEventTree(eventTree: Gen[EventTree], numModifications: Int): Gen[EventTree] = {
    if (numModifications == 0) return eventTree
    for {
      id <- genIdTree.suchThat(id => !id.isAnonymous)
      modifiedEventTree <- for {
        ev <- modifyEventTree(eventTree, numModifications - 1)
      } yield ev.increment(id)
    } yield modifiedEventTree
  }
}
