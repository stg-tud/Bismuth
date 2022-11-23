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

  def +(otherId: IdTree): IdTree = (this, otherId) match
    case (Leaf(0), otherId) => otherId
    case (id, Leaf(0)) => id
    case (Branch(l1, r1), Branch(l2, r2)) =>
      val l: IdTree = (l1 + l2).normalize
      val r: IdTree = (r1 + r2).normalize
      Branch(l, r)
// TODO: Maybe handle this differently.
// The missing cases are invalid, when using + for join
//case (Leaf(1), _) => Leaf(1)
//case (_, Leaf(1)) => Leaf(1)

object IdTree {
  val seed: IdTree = Leaf(1)

  given Conversion[Int, Leaf] = {
    case 0 => Leaf(0)
    case 1 => Leaf(1)
  }

  given Conversion[(Leaf, Leaf), Branch] = Branch.apply

  given NormalForm[IdTree] with
    extension (tree: IdTree)
      def normalize: IdTree = tree match
        case Branch(l@Leaf(i1), Leaf(i2)) if i1 == i2 => l
        case id@_ => id

  val anonymous: IdTree = Leaf(0)
}
