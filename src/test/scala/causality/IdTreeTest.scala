package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.{Branch, Leaf, given}

import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class IdTreeTest extends AnyFlatSpec {
  "The seed" should "split correctly" in {
    assert(IdTree.seed.split == (Branch(1, 0), Branch(0, 1)))
  }
}
