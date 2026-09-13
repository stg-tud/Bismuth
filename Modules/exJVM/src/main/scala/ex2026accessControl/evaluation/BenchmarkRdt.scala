package ex2026accessControl.evaluation

import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{LastWriterWins, PosNegCounter}
import rdts.filters.Filter

case class BenchmarkRdt(
    a: LastWriterWins[Int],
    b: MultipleFields,
    c: MoreNesting,
    d: EvenMoreNesting
)

case class MultipleFields(x: PosNegCounter, y: PosNegCounter, z: LastWriterWins[Int])

case class MoreNesting(alpha: MultipleFields, beta: PosNegCounter)

case class EvenMoreNesting(one: MoreNesting, two: LastWriterWins[Int])

object BenchmarkRdt {
  given Lattice[BenchmarkRdt]    = Lattice.derived
  given Lattice[MultipleFields]  = Lattice.derived
  given Lattice[MoreNesting]     = Lattice.derived
  given Lattice[EvenMoreNesting] = Lattice.derived

  given Bottom[Int]             = Bottom.provide(0)
  given Bottom[BenchmarkRdt]    = Bottom.derived
  given Bottom[MultipleFields]  = Bottom.derived
  given Bottom[MoreNesting]     = Bottom.derived
  given Bottom[EvenMoreNesting] = Bottom.derived

  given Filter[BenchmarkRdt]        = Filter.derived
  given Filter[LastWriterWins[Int]] = Filter.terminalLwwFilter
  given Filter[PosNegCounter]       = Filter.ofTerminalValue
  given Filter[MultipleFields]      = Filter.derived
  given Filter[MoreNesting]         = Filter.derived
  given Filter[EvenMoreNesting]     = Filter.derived

  private val multiFieldsPerms       = List("x", "y", "z")
  private val moreNestingPerms       = List("alpha", for { f <- multiFieldsPerms } yield s"alpha.$f", "beta")
  private val evenMoreNestingPerms   = List("one", for { f <- moreNestingPerms } yield s"one.$f", "two")
  val benchmarkRdtPerms: Seq[String] =
    Seq("a", "b")
    ++ (for { f <- multiFieldsPerms } yield s"b.$f")
    ++ Seq("c")
    ++ (for { f <- moreNestingPerms } yield s"c.$f")
    ++ Seq("d")
    ++ (for { f <- evenMoreNestingPerms } yield s"d.$f")
}
