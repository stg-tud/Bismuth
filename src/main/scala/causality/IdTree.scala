package de.tu_darmstadt.stg.daimpl
package causality

enum IdTree:
  case Leaf(value: 0 | 1)
  case Branch(left: IdTree, right: IdTree)

  def split: (IdTree, IdTree) = this match
    case Leaf(0) => (Leaf(0), Leaf(0))
    case Leaf(1) => (Branch(Leaf(1), Leaf(0)), Branch(Leaf(0), Leaf(1)))
    case Branch(Leaf(0), i) =>
      val (i1, i2) = i.split
      (Branch(Leaf(0), i1), Branch(Leaf(0), i2))
    case Branch(i, Leaf(0)) =>
      val (i1, i2) = i.split
      (Branch(i1, Leaf(0)), Branch(i2, Leaf(0)))
    case Branch(i1, i2) =>
      (Branch(i1, Leaf(0)), Branch(Leaf(0), i2))

  def +(otherId: IdTree): Option[IdTree] = (this, otherId) match
    case (Leaf(0), otherId) => Some(otherId)
    case (id, Leaf(0)) => Some(id)
    case (Branch(l1, r1), Branch(l2, r2)) =>
      for {
        l <- l1 + l2
        r <- r1 + r2
      } yield Branch(l, r).normalize
    // Cannot join two IdTrees that aren't disjoint
    case (Leaf(1), _) => None
    case (_, Leaf(1)) => None

object IdTree {
  val seed: IdTree = Leaf(1)
  val anonymous: IdTree = Leaf(0)

  given Conversion[Int, Leaf] = {
    case 0 => Leaf(0)
    case 1 => Leaf(1)
  }

  given Conversion[(Leaf, Leaf), Branch] = Branch.apply

  given NormalForm[IdTree] with
    extension (tree: IdTree)
      def normalize: IdTree = tree match
        case l@Leaf(_) => l
        case Branch(l@Leaf(i1), Leaf(i2)) if i1 == i2 => l
        case Branch(l, r) => (l.normalize, r.normalize) match
          case (Leaf(lNormVal), Leaf(rNormVal)) if lNormVal == rNormVal => Leaf(lNormVal)
          case (lNorm, rNorm) => Branch(lNorm, rNorm)
}
