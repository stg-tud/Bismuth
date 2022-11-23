package de.tu_darmstadt.stg.daimpl
package causality

import EventTree.{Leaf, MAX_DEPTH, given}
import causality.IdTree

import scala.language.implicitConversions

enum EventTree:
  case Leaf(value: Int)

  case Branch(value: Int, left: EventTree, right: EventTree)

  def min: Int = this match
    case Leaf(i) => i
    case Branch(n, left, right) =>
      Math.min(Math.min(n, left.min), right.min)

  def max: Int = this match
    case Leaf(i) => i
    case Branch(n, left, right) => n + left.max + right.max

  def lift(m: Int): EventTree = this match
    case Leaf(n) => Leaf(n + m)
    case Branch(n, e1, e2) => Branch(n + m, e1, e2)

  def sink(m: Int): EventTree = this match
    case Leaf(n) => Leaf(n - m)
    case Branch(n, e1, e2) => Branch(n - m, e1, e2)

  def join(other: EventTree): EventTree = (this, other) match
    case (Leaf(n1), Leaf(n2)) => Leaf(Math.max(n1, n2))
    case (Leaf(n1), rb@Branch(_, _, _)) => Branch(n1, 0, 0) join rb
    case (lb@Branch(_, _, _), Leaf(n2)) => lb join Branch(n2, 0, 0)
    case (lb@Branch(n1, _, _), rb@Branch(n2, _, _)) if n1 > n2 => rb join lb
    case (Branch(n1, l1, r1), Branch(n2, l2, r2)) =>
      Branch(
        n1,
        l1 join (l2 lift (n2 - n1)),
        r1 join (r2 lift (n2 - n1))
      ).normalize

  def increment(id: IdTree): EventTree =
    val filledEventTree = fill(id)

    if this != filledEventTree then
      filledEventTree
    else
      grow(id)._1

  private def fill(id: IdTree): EventTree = (id, this) match
    case (IdTree.Leaf(0), e) => e
    case (IdTree.Leaf(1), e) => e.max
    case (_, e@EventTree.Leaf(_)) => e
    case (IdTree.Branch(IdTree.Leaf(1), ir), EventTree.Branch(n, el, er)) =>
      val erFilled = er.fill(ir)
      Branch(
        n,
        Math.max(el.max, erFilled.min),
        erFilled
      ).normalize
    case (IdTree.Branch(il, IdTree.Leaf(1)), EventTree.Branch(n, el, er)) =>
      val elFilled = el.fill(il)
      Branch(
        n,
        elFilled,
        Math.max(er.max, elFilled.min)
      ).normalize
    case (IdTree.Branch(il, ir), EventTree.Branch(n, el, er)) =>
      Branch(
        n,
        el.fill(il),
        er.fill(ir)
      ).normalize

  private def grow(id: IdTree): (EventTree, Int) = (id, this) match
    case (IdTree.Leaf(1), EventTree.Leaf(n)) => (Leaf(n + 1), 0)
    case (i, EventTree.Leaf(n)) =>
      val (eGrown, cost) = EventTree.Branch(n, 0, 0).grow(i)
      (eGrown, cost + MAX_DEPTH)
    case (IdTree.Branch(IdTree.Leaf(0), ir), EventTree.Branch(n, el, er)) =>
      val (erGrown, cost) = er.grow(ir)
      (Branch(n, el, erGrown), cost + 1)
    case (IdTree.Branch(il, IdTree.Leaf(0)), EventTree.Branch(n, el, er)) =>
      val (elGrown, cost) = el.grow(il)
      (Branch(n, elGrown, er), cost + 1)
    case (IdTree.Branch(il, ir), EventTree.Branch(n, el, er)) =>
      val (elGrown, elGrowCost) = el.grow(il)
      val (erGrown, erGrowCost) = er.grow(ir)
      if elGrowCost < erGrowCost then
        (Branch(n, elGrown, er), elGrowCost + 1)
      else
        (Branch(n, el, erGrown), erGrowCost + 1)
    // The following two cases should never be reached, since should be called before grow
    //case (IdTree.Leaf(0), EventTree.Branch(_, _, _)) => ???
    //case (IdTree.Leaf(1), EventTree.Branch(_, _, _)) => ???


object EventTree {
  private val MAX_DEPTH = 4096

  val seed: EventTree = Leaf(0)

  given Conversion[Int, Leaf] = Leaf.apply

  given PartialOrdering[EventTree] with {
    extension (left: EventTree)
      inline def <=(right: EventTree): Boolean = lteq(left, right)
      inline def <=(right: Int): Boolean = lteq(left, Leaf(right))

    override def lteq(x: EventTree, y: EventTree): Boolean = (x, y) match {
      case (Leaf(n1), Leaf(n2)) => n1 <= n2
      case (Leaf(n1), Branch(n2, _, _)) => n1 <= n2
      case (Branch(n1, l1, r1), Leaf(n2)) => n1 <= n2 && l1.lift(n1) <= n2 && r1.lift(n1) <= n2
      case (Branch(n1, l1, r1), Branch(n2, l2, r2)) => n1 <= n2 && l1.lift(n1) <= l2.lift(n2) && r1.lift(n1) <= r2.lift(n2)
    }

    // TODO: optimize this
    override def tryCompare(x: EventTree, y: EventTree): Option[Int] =
      if x <= y then
        if y <= x then Some(1)
        else Some(-1)
      else if y <= x then Some(1)
      else None
  }

  given NormalForm[EventTree] with
    extension (tree: EventTree)
      def normalize: EventTree = tree match
        case leaf@Leaf(_) => leaf
        case Branch(n, l, r) => (l.normalize, r.normalize) match
          case (Leaf(m1), Leaf(m2)) if m1 == m2 => // TODO: What if m1 != m2?
            Leaf(n + m1)
          case (e1, e2) =>
            // TODO: Can be optimized, since e1 and e2 are normalized and for normalized tree t, t.min = n for t = (n, left, right)
            val m = Math.min(e1.min, e2.min)
            Branch(n + m, e1.sink(m), e2.sink(m))
}
