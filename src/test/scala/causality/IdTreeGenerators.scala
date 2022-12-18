package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf}

import org.scalacheck.Gen

import scala.language.implicitConversions

object IdTreeGenerators {
  def normalizedBaseCases: Gen[IdTree] = Gen.oneOf(Leaf(0), Leaf(1), Branch(1, 0), Branch(0, 1))
  def allBaseCases: Gen[IdTree]        = Gen.oneOf(Leaf(1), Leaf(0), Branch(0, 1), Branch(1, 0), Branch(1, 1))

  given genIdTree: Gen[IdTree] = for {
    maxDepth <- Gen.choose(0, 10)
    idTree   <- genIdTreeBranch(maxDepth)
  } yield idTree

  private def genIdTreeBranch(maxDepth: Int): Gen[IdTree] = for {
    maxDepthL <- Gen.choose(0, Math.max(0, maxDepth - 1))
    maxDepthR <- Gen.choose(0, Math.max(0, maxDepth - 1))
    leftId    <- genIdTreeBranchOrLeaf(maxDepthL)
    rightId   <- genIdTreeBranchOrLeaf(maxDepthR)
  } yield Branch(leftId, rightId)

  private def genIdTreeLeaf = Gen.oneOf(Leaf(0), Leaf(1))

  private def genIdTreeBranchOrLeaf(maxDepth: Int): Gen[IdTree] = {
    if (maxDepth == 0) genIdTreeLeaf
    else Gen.oneOf(genIdTreeLeaf, genIdTreeBranch(maxDepth))
  }
}
