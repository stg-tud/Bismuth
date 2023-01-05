package de.tu_darmstadt.stg.daimpl
package causality

import causality.EventTree.{Branch, Leaf, MAX_DEPTH, given}
import causality.IdTree

import scala.annotation.targetName
import scala.language.implicitConversions

sealed trait EventTree:
  def min: Int = this match
    case Leaf(i)                => i
    case Branch(n, left, right) =>
      // In a normalized EventTree, the min is simply n
      n + Math.min(left.min, right.min)

  def max: Int = this match
    case Leaf(i) => i
    case Branch(n, left, right) =>
      n + Math.max(left.max, right.max)

  def lift(m: Int): EventTree = this match
    case _ if m == 0       => this
    case Leaf(n)           => Leaf(n + m)
    case Branch(n, e1, e2) => Branch(n + m, e1, e2)

  def sink(m: Int): EventTree = this match
    case _ if m == 0                 => this
    case Leaf(n) if n >= m           => Leaf(n - m)
    case Branch(n, e1, e2) if n >= m => Branch(n - m, e1, e2)
    case _                           => throw IllegalArgumentException(f"Cannot sink $this by $m")

  def join(other: EventTree): EventTree = (this, other) match
    case (Leaf(n1), Leaf(n2))                                      => Leaf(Math.max(n1, n2))
    case (Leaf(n1), rb @ Branch(_, _, _))                          => Branch(n1, 0, 0) join rb
    case (lb @ Branch(_, _, _), Leaf(n2))                          => lb join Branch(n2, 0, 0)
    case (lb @ Branch(n1, _, _), rb @ Branch(n2, _, _)) if n1 > n2 => rb join lb
    case (Branch(n1, l1, r1), Branch(n2, l2, r2)) =>
      Branch(
        n1,
        l1 join (l2 lift (n2 - n1)),
        r1 join (r2 lift (n2 - n1))
      ).normalized

  @throws[IllegalArgumentException]("when the id is anonymous")
  def increment(id: IdTree): EventTree = {
    if (id.isAnonymous) {
      throw IllegalArgumentException("Cannot increment an EventTree by the anonymous IdTree")
    }

    val normalizedId        = id.normalized
    val normalizedEventTree = this.normalized

    val filledEventTree = normalizedEventTree.fill(normalizedId)

    if (!(normalizedEventTree eq filledEventTree)) {
      filledEventTree
    } else {
      normalizedEventTree.grow(normalizedId)._1
    }
  }

  private def fill(id: IdTree): EventTree = (id, this) match
    case (IdTree.Leaf(0), e)        => e
    case (IdTree.Leaf(1), e)        => e.max
    case (_, e @ EventTree.Leaf(_)) => e
    case (IdTree.Branch(IdTree.Leaf(1), ir), EventTree.Branch(n, el, er)) =>
      val erFilled = er.fill(ir)
      Branch(
        n,
        Math.max(el.max, erFilled.min),
        erFilled
      ).normalized
    case (IdTree.Branch(il, IdTree.Leaf(1)), EventTree.Branch(n, el, er)) =>
      val elFilled = el.fill(il)
      Branch(
        n,
        elFilled,
        Math.max(er.max, elFilled.min)
      ).normalized
    case (IdTree.Branch(il, ir), EventTree.Branch(n, el, er)) =>
      Branch(
        n,
        el.fill(il),
        er.fill(ir)
      ).normalized

  protected def grow(id: IdTree): (EventTree, Int) = (id, this) match
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
      if elGrowCost < erGrowCost then (Branch(n, elGrown, er), elGrowCost + 1)
      else (Branch(n, el, erGrown), erGrowCost + 1)
    // The following case should never be reached.
    case _ =>
      throw IllegalStateException("Either fill wasn't called before grow, or the id is anonymous/not normalized")

object EventTree {
  case class Leaf(value: Int) extends EventTree:
    require(value >= 0)

  case class Branch(value: Int, left: EventTree, right: EventTree) extends EventTree:
    require(value >= 0)

  private val MAX_DEPTH = 4096

  val seed: EventTree = Leaf(0)

  given Conversion[Int, Leaf] = Leaf.apply

  given PartialOrdering[EventTree] with {
    extension (left: EventTree)
      @targetName("lteq")
      inline def <=(right: EventTree): Boolean = lteq(left, right)
      @targetName("lteq")
      inline def <=(right: Int): Boolean = lteq(left, Leaf(right))

    override def lteq(x: EventTree, y: EventTree): Boolean = (x, y) match {
      case (Leaf(n1), Leaf(n2))           => n1 <= n2
      case (Leaf(n1), Branch(n2, _, _))   => n1 <= n2
      case (Branch(n1, l1, r1), Leaf(n2)) => n1 <= n2 && l1.lift(n1) <= n2 && r1.lift(n1) <= n2
      case (Branch(n1, l1, r1), Branch(n2, l2, r2)) =>
        n1 <= n2 && l1.lift(n1) <= l2.lift(n2) && r1.lift(n1) <= r2.lift(n2)
    }

    // TODO: optimize this
    override def tryCompare(x: EventTree, y: EventTree): Option[Int] =
      if x <= y then
        if y <= x then Some(0)
        else Some(-1)
      else if y <= x then Some(1)
      else None
  }

  given NormalForm[EventTree] with
    extension (tree: EventTree)
      def normalized: EventTree = tree match {
        case Leaf(_) => tree // Already normalized
        case Branch(n, l, r) =>
          val (lNorm, rNorm) = (l.normalized, r.normalized)
          val min = (lNorm, rNorm) match {
            case (Leaf(m1), Leaf(m2)) if m1 == m2     => return Leaf(n + m1)
            case (Leaf(m1), Leaf(m2))                 => Math.min(m1, m2)
            case (Leaf(m1), Branch(m2, _, _))         => Math.min(m1, m2)
            case (Branch(m1, _, _), Leaf(m2))         => Math.min(m1, m2)
            case (Branch(m1, _, _), Branch(m2, _, _)) => Math.min(m1, m2)
          }
          if (min == 0) {                       // Already normalized
            if ((lNorm eq l) && (rNorm eq r)) { // Reuse reference if tree is already normalized
              tree
            } else {
              Branch(n, lNorm, rNorm)
            }
          } else {
            Branch(n + min, lNorm.sink(min), rNorm.sink(min))
          }
      }
}
