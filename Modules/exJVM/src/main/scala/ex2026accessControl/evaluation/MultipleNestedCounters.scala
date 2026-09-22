package ex2026accessControl.evaluation

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{GrowOnlyCounter, PosNegCounter}
import rdts.filters.Filter

case class MultipleNestedCounters(
    a: NestedCounters,
    b: NestedCounters,
    c: NestedCounters
)

case class NestedCounters(
    a: CounterTripel,
    b: CounterTripel,
    c: CounterTripel
)

case class CounterTripel(
    a: PosNegCounter,
    b: PosNegCounter,
    c: PosNegCounter
)

object MultipleNestedCounters {
  given Lattice[MultipleNestedCounters] = Lattice.derived
  given Lattice[NestedCounters]         = Lattice.derived
  given Lattice[CounterTripel]          = Lattice.derived

  given Bottom[MultipleNestedCounters] = Bottom.deriveStructural
  given Bottom[NestedCounters]         = Bottom.deriveStructural
  given Bottom[CounterTripel]          = Bottom.deriveStructural
  given Bottom[PosNegCounter]          = Bottom.deriveStructural
  given Bottom[GrowOnlyCounter]        = Bottom.provide(GrowOnlyCounter(Map.empty))

  given Filter[MultipleNestedCounters] = Filter.derived
  given Filter[NestedCounters]         = Filter.derived
  given Filter[CounterTripel]          = Filter.derived
  given Filter[PosNegCounter]          = Filter.derived
  given Filter[GrowOnlyCounter]        = Filter.ofTerminalValue

  val permissions: Seq[String] = for {
    a <- Seq("a", "b", "c")
    b <- Seq("a", "b", "c")
    c <- Seq("a", "b", "c")
    d <- Seq("pos", "neg")
  } yield s"$a.$b.$c.$d"
}
