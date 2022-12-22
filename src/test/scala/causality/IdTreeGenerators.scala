package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf, anonymous, seed}

import org.scalacheck.{Arbitrary, Gen}

import scala.language.implicitConversions

object IdTreeGenerators {
  val genIdTreeLeaf: Gen[IdTree]       = Gen.oneOf(Leaf(0), Leaf(1))
  val normalizedBaseCases: Gen[IdTree] = Gen.oneOf(Leaf(0), Leaf(1), Branch(1, 0), Branch(0, 1))
  val allBaseCases: Gen[IdTree] = Gen.oneOf(Leaf(1), Leaf(0), Branch(0, 0), Branch(0, 1), Branch(1, 0), Branch(1, 1))

  given genIdTreeShallow: Gen[IdTree] = for {
    maxDepth <- Gen.choose(1, 10)
    idTree <- Gen.oneOf(genIdTreeLeaf, genIdTreeBranch(maxDepth))
  } yield idTree

  given genIdTree: Gen[IdTree] = for {
    maxDepth <- Gen.choose(1, 50)
    idTree   <- genIdTreeBranch(maxDepth)
  } yield idTree

  given genIdTreeBySplitting: Gen[IdTree] = for {
    numSplits <- Gen.choose(0, 100)
    id        <- splitAndChoose(numSplits)
  } yield id

  private def splitAndChoose(numSplits: Int): Gen[IdTree] =
    if (numSplits == 0) {
      IdTree.seed
    } else {
      for {
        id <- splitAndChoose(numSplits - 1)
        (left, right) = id.split
        chosenId <- Gen.oneOf(left, right)
      } yield chosenId
    }

  given genTwoNonOverlappingIdTrees: Gen[(IdTree, IdTree)] = for {
    left  <- genIdTree
    right <- genIdTree.suchThat(right => !(left overlapsWith right))
  } yield (left, right)

  private def genIdTreeBranch(maxDepth: Int): Gen[IdTree] = for {
    maxDepthL <- Gen.choose(0, maxDepth - 1)
    maxDepthR <- Gen.choose(0, maxDepth - 1)
    leftId    <- genIdTreeBranchOrLeaf(maxDepthL)
    rightId   <- genIdTreeBranchOrLeaf(maxDepthR)
  } yield Branch(leftId, rightId)

  private def genIdTreeBranchOrLeaf(maxDepth: Int): Gen[IdTree] = {
    if (maxDepth == 0) genIdTreeLeaf
    else Gen.oneOf(genIdTreeLeaf, genIdTreeBranch(maxDepth))
  }
}
