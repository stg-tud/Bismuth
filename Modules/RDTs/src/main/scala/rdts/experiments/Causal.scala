package rdts.experiments

import rdts.base.{Bottom, DecoratedLattice, Lattice}
import rdts.time.Dots

case class CausalDelta[A](contained: Dots, predecessors: Dots, delta: A) derives Lattice, Bottom

case class CausalStore[A](pending: Set[CausalDelta[A]], context: Dots, state: A)

object CausalStore {
  given lattice[A: Lattice]: DecoratedLattice[CausalStore[A]](Lattice.derived) with {

    override def compact(merged: CausalStore[A]): CausalStore[A] = {
      val (applicable, rem) = merged.pending.partition(p => merged.context.contains(p.predecessors))
      CausalStore(
        rem,
        applicable.map(_.contained).foldLeft(merged.context)(Lattice.merge),
        applicable.map(_.delta).foldLeft(merged.state)(Lattice.merge)
      )
    }

  }

  given bottom[A: Bottom]: Bottom[CausalStore[A]] = Bottom.derived

}
