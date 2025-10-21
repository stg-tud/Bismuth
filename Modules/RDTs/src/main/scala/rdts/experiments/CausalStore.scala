package rdts.experiments

import rdts.base.{Bottom, DecoratedLattice, Lattice, LocalUid}
import rdts.experiments.CausalStore.CausalDelta
import rdts.time.{Dot, Dots}

import scala.annotation.tailrec

case class CausalStore[A](pending: Set[CausalDelta[A]], context: Dots, state: Option[A]) {
  def add(delta: A)(using LocalUid): CausalStore[A] =
    val nextDot = context.nextDot
    CausalStore(Set(CausalDelta(nextDot, context, delta)), Dots.empty, None)

  @tailrec
  final def compact(using lattice: Lattice[Option[A]]): CausalStore[A] = {
    val (applicable, rem) = pending.partition(p => context.contains(p.predecessors))
    if applicable.isEmpty then this
    else
      CausalStore(
        rem,
        context.union(Dots.from(applicable.map(_.dot))),
        applicable.map(a => Some(a.delta)).foldLeft(state)(Lattice.merge)
      ).compact
  }
}

object CausalStore {

  case class CausalDelta[A](dot: Dot, predecessors: Dots, delta: A)

  given lattice[A: Lattice]: DecoratedLattice[CausalStore[A]](Lattice.derived) with {

    override def compact(merged: CausalStore[A]): CausalStore[A] = merged.compact

  }

  given bottom[A: Bottom]: Bottom[CausalStore[A]] = Bottom.derived

}
