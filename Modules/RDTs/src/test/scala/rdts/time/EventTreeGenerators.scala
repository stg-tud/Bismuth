package com.github.ckuessner
package causality

import causality.EventTree.{Branch, Leaf, seed}
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

  given genRandomEventTree: Gen[EventTree] = {
    for {
      maxDepth  <- Gen.choose(1, 10)
      eventTree <- genEventTreeBranch(maxDepth)
    } yield eventTree
  }

  private def genEventTreeBranch(maxDepth: Int): Gen[Branch] = for {
    maxDepthL      <- Gen.choose(0, maxDepth - 1)
    maxDepthR      <- Gen.choose(0, maxDepth - 1)
    branchValue    <- Gen.choose(0, 10)
    leftEventTree  <- genEventTreeBranchOrLeaf(maxDepthL)
    rightEventTree <- genEventTreeBranchOrLeaf(maxDepthR)
  } yield Branch(branchValue, leftEventTree, rightEventTree)

  val genEventTreeLeaf: Gen[Leaf] = for {
    value <- Gen.choose(0, 100)
  } yield Leaf(value)

  private def genEventTreeBranchOrLeaf(maxDepth: Int): Gen[EventTree] = {
    if (maxDepth == 0) genEventTreeLeaf
    else Gen.oneOf(genEventTreeLeaf, genEventTreeBranch(maxDepth))
  }
}
