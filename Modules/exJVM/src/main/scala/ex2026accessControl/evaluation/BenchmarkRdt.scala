package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, Decompose, Lattice}
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

  given Decompose[BenchmarkRdt]    = Decompose.derived
  given Decompose[MultipleFields]  = Decompose.derived
  given Decompose[MoreNesting]     = Decompose.derived
  given Decompose[EvenMoreNesting] = Decompose.derived

  given jsonCodec: JsonValueCodec[BenchmarkRdt] =
      given JsonValueCodec[Int] = JsonCodecMaker.make
      import channels.JsoniterCodecs.given
      JsonCodecMaker.make[BenchmarkRdt]

  val empty: BenchmarkRdt = Bottom[BenchmarkRdt].empty

  private val multiFieldsPerms     = List("x", "y", "z")
  private val moreNestingPerms     = List("alpha") ++ multiFieldsPerms.map(f => s"alpha.$f") ++ List("beta")
  private val evenMoreNestingPerms = List("one") ++ moreNestingPerms.map(f => s"one.$f") ++ List("two")
  val benchmarkRdtPerms: Seq[String] =
    Seq("a", "b")
    ++ multiFieldsPerms.map(f => s"b.$f")
    ++ Seq("c")
    ++ moreNestingPerms.map(f => s"c.$f")
    ++ Seq("d")
    ++ evenMoreNestingPerms.map(f => s"d.$f")
}
