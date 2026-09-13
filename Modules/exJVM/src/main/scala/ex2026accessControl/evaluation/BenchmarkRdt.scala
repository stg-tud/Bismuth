package ex2026accessControl.evaluation

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{LastWriterWins, PosNegCounter}
import rdts.filters.Filter

case class BenchmarkRdt(
    title: LastWriterWins[Int],
    nestedA: MultipleFields,
    nestedB: MultipleFields,
    moreNesting: MoreNesting
)

case class MultipleFields(counterA: PosNegCounter, counterB: PosNegCounter, lastWriterWins: LastWriterWins[Int])

case class MoreNesting(something: MultipleFields, other: PosNegCounter)

case class EvenMoreNesting(moreNesting: MoreNesting)

object BenchmarkRdt {
  given Lattice[BenchmarkRdt]   = Lattice.derived
  given Lattice[MultipleFields] = Lattice.derived
  given Lattice[MoreNesting]    = Lattice.derived

  given Bottom[Int]            = Bottom.provide(0)
  given Bottom[BenchmarkRdt]   = Bottom.derived
  given Bottom[MultipleFields] = Bottom.derived
  given Bottom[MoreNesting]    = Bottom.derived

  given Filter[BenchmarkRdt] = Filter.derived
  given Filter[LastWriterWins[Int]] = Filter.terminalLwwFilter
  given Filter[PosNegCounter] = Filter.ofTerminalValue
  given Filter[MultipleFields] = Filter.derived
  given Filter[MoreNesting] = Filter.derived
}
