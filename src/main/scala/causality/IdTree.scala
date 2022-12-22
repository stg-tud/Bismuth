package de.tu_darmstadt.stg.daimpl
package causality

import causality.IdTree.splitNormalizedIdTree

import scala.annotation.{tailrec, targetName}

enum IdTree:
  case Leaf(value: 0 | 1)
  case Branch(left: IdTree, right: IdTree)

  /** Normalizes and splits this id into two non-overlapping ids such that both ids are child ids.
    *
    * @return
    *   A tuple of non-overlapping child ids.
    */
  def split: (IdTree, IdTree) = {
    splitNormalizedIdTree(this.normalized)
  }

  /** Add two non-overlapping id trees, resulting in one normalized IdTree.
    * @throws IllegalArgumentException
    *   if both IdTrees overlap
    * @param otherId
    *   The id to add to `this`
    * @return
    *   the normalized IdTree containing both ids
    */
  @throws[IllegalArgumentException]("If the IdTrees overlap")
  @targetName("add")
  def +(otherId: IdTree): IdTree = (this, otherId) match
    case (Leaf(0), otherId) =>
      otherId.normalized match {
        case Leaf(0)         => this // Prefer left part of tree
        case otherNormalized => otherNormalized
      }
    case (id, Leaf(0)) => id.normalized
    case (Branch(l1, r1), Branch(l2, r2)) => {
      val l = l1 + l2
      val r = r1 + r2

      // Ensure normalization
      (l, r) match {
        case (Leaf(lVal), Leaf(rVal)) if lVal == rVal => l
        case _                                        =>
          // Reuse parts of the tree
          if ((l eq l1) && (r eq r1)) return this
          if ((l eq l2) && (r eq r2)) return otherId

          Branch(l, r)
      }
    }
    case (l, r) => {
      (l.normalized, r.normalized) match {
        case (l @ Leaf(1), Leaf(0)) => l
        case (Leaf(0), r @ Leaf(1)) => r
        case _                      => throw IllegalArgumentException("Cannot add two overlapping IdTrees")
      }
    }

  def min: 0 | 1 = this match {
    case IdTree.Leaf(value) => value
    case IdTree.Branch(left, right) =>
      val lMin: 0 | 1 = left.min
      val rMin: 0 | 1 = right.min
      if lMin <= rMin then lMin else rMin
  }

  def max: 0 | 1 = this match {
    case IdTree.Leaf(value) => value
    case IdTree.Branch(left, right) =>
      val lMax: 0 | 1 = left.max
      val rMax: 0 | 1 = right.max
      if lMax >= rMax then lMax else rMax
  }

  def isAnonymous: Boolean = this.max == 0

  def overlapsWith(otherId: IdTree): Boolean = (this, otherId) match
    case (Leaf(0), _)                     => false
    case (_, Leaf(0))                     => false
    case (Leaf(1), Leaf(1))               => true
    case (Leaf(1), r)                     => r.max != 0
    case (l, Leaf(1))                     => l.max != 0
    case (Branch(l1, r1), Branch(l2, r2)) => (l1 overlapsWith l2) || (r1 overlapsWith r2)

object IdTree {
  val seed: IdTree      = Leaf(1)
  val anonymous: IdTree = Leaf(0)

  /** Splits the id without normalization.
    *
    * If the id is in normal form, the resulting ids can be incorrect. Example: Branch(Branch(1, 1), Branch(0,0)) will
    * be split into Branch(Branch(1,1),0) and Branch(0, Branch(0,0)) which is equivalent to Branch(1,0) and Leaf(0) (the
    * anonymous stamp).
    *
    * @param id
    *   The normalized id to be split
    * @return
    *   Assuming the ids are normalized, two non-overlapping child ids of the id
    */
  def splitNormalizedIdTree(id: IdTree): (IdTree, IdTree) =
    id match
      case Leaf(0) => (Leaf(0), Leaf(0))
      case Leaf(1) => (Branch(Leaf(1), Leaf(0)), Branch(Leaf(0), Leaf(1)))
      case Branch(Leaf(0), i) =>
        val (i1, i2) = splitNormalizedIdTree(i)
        (Branch(Leaf(0), i1), Branch(Leaf(0), i2))
      case Branch(i, Leaf(0)) =>
        val (i1, i2) = splitNormalizedIdTree(i)
        (Branch(i1, Leaf(0)), Branch(i2, Leaf(0)))
      case Branch(i1, i2) =>
        (Branch(i1, Leaf(0)), Branch(Leaf(0), i2))

  given Conversion[Int, Leaf] = {
    case 0 => Leaf(0)
    case 1 => Leaf(1)
  }

  given Conversion[(Leaf, Leaf), Branch] = Branch.apply

  given NormalForm[IdTree] with
    extension (tree: IdTree)
      def normalized: IdTree = tree match
        case Leaf(_)                                    => tree // Reuse reference if nothing has changed
        case Branch(l @ Leaf(i1), Leaf(i2)) if i1 == i2 => l
        case Branch(l, r) =>
          (l.normalized, r.normalized) match
            case (Leaf(lNormVal), Leaf(rNormVal)) if lNormVal == rNormVal => Leaf(lNormVal)
            case (lNorm, rNorm) if ((l eq lNorm) && (r eq rNorm)) => tree // Reuse reference if nothing has changed
            case (lNorm, rNorm)                                   => Branch(lNorm, rNorm)

  given PartialOrdering[IdTree] with {
    extension (left: IdTree) inline def <=(right: IdTree): Boolean = lteq(left, right)

    override def lteq(x: IdTree, y: IdTree): Boolean = (x, y) match {
      case (Leaf(l), Leaf(r))               => l <= r
      case (Leaf(l), r)                     => l <= r.min
      case (l, Leaf(r))                     => l.max <= r
      case (Branch(l1, l2), Branch(r1, r2)) => l1 <= r1 && l2 <= r2
    }

    override def tryCompare(x: IdTree, y: IdTree): Option[Int] = (x, y) match {
      case (Leaf(l), Leaf(r)) => Some(l.compare(r))
      case (Branch(l1, l2), Branch(r1, r2)) =>
        for {
          l1Comp <- tryCompare(l1, r1)
          l2Comp <- tryCompare(l2, r2)
          comp <-
            if l1Comp == l2Comp
            then Some(l1Comp)
            else if l1Comp == 0 then Some(l2Comp)
            else if l2Comp == 0 then Some(l1Comp)
            else None
        } yield comp
      case (l @ Leaf(_), Branch(r1, r2))   => tryCompare(Branch(l, l), Branch(r1, r2))
      case (l @ Branch(_, _), r @ Leaf(_)) => tryCompare(l, Branch(r, r))
    }
  }
}
