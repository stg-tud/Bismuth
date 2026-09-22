package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import ex2026accessControl.evaluation.BenchmarkRdt.given
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.{GrowOnlyCounter, PosNegCounter}
import rdts.filters.Filter

import scala.util.Random

case class BenchmarkRdt(
    a: NestedCounters = Bottom.empty,
    b: NestedCounters = Bottom.empty,
    c: NestedCounters = Bottom.empty
)

case class NestedCounters(
    a: CounterTripel = Bottom.empty,
    b: CounterTripel = Bottom.empty,
    c: CounterTripel = Bottom.empty
)

case class CounterTripel(
    a: PosNegCounter = Bottom.empty,
    b: PosNegCounter = Bottom.empty,
    c: PosNegCounter = Bottom.empty
)

object BenchmarkRdt {
  given Lattice[BenchmarkRdt]   = Lattice.derived
  given Lattice[NestedCounters] = Lattice.derived
  given Lattice[CounterTripel]  = Lattice.derived

  given Bottom[BenchmarkRdt]    = Bottom.deriveStructural
  given Bottom[NestedCounters]  = Bottom.deriveStructural
  given Bottom[CounterTripel]   = Bottom.deriveStructural
  given Bottom[PosNegCounter]   = Bottom.deriveStructural
  given Bottom[GrowOnlyCounter] = Bottom.provide(GrowOnlyCounter(Map.empty))

  given Decompose[BenchmarkRdt]   = Decompose.derived
  given Decompose[NestedCounters] = Decompose.derived
  given Decompose[CounterTripel]  = Decompose.derived

  given Filter[BenchmarkRdt]    = Filter.derived
  given Filter[NestedCounters]  = Filter.derived
  given Filter[CounterTripel]   = Filter.derived
  given Filter[PosNegCounter]   = Filter.derived
  given Filter[GrowOnlyCounter] = Filter.ofTerminalValue

  given jsonCodec: JsonValueCodec[BenchmarkRdt] =
      import channels.JsoniterCodecs.given
      JsonCodecMaker.make[BenchmarkRdt]

  val empty: BenchmarkRdt = Bottom[BenchmarkRdt].empty

  val leafPaths: Seq[String] = for {
    a <- Seq("a", "b", "c")
    b <- Seq("a", "b", "c")
    c <- Seq("a", "b", "c")
    d <- Seq("pos", "neg")
  } yield s"$a.$b.$c.$d"

  def applyBenchmarkRdtMutator(
      choice: String,
      state: BenchmarkRdt,
  )(using random: Random, author: LocalUid): BenchmarkRdt = {
    val choiceSplit = choice.split('.')

    inline def one(in: BenchmarkRdt): BenchmarkRdt = choiceSplit(0) match {
      case "a" => BenchmarkRdt(a = two(in.a))
      case "b" => BenchmarkRdt(b = two(in.b))
      case "c" => BenchmarkRdt(c = two(in.c))
    }

    inline def two(in: NestedCounters): NestedCounters = choiceSplit(1) match {
      case "a" => NestedCounters(a = three(in.a))
      case "b" => NestedCounters(b = three(in.b))
      case "c" => NestedCounters(c = three(in.c))
    }

    inline def three(in: CounterTripel): CounterTripel = choiceSplit(2) match {
      case "a" => CounterTripel(a = four(in.a))
      case "b" => CounterTripel(b = four(in.b))
      case "c" => CounterTripel(c = four(in.c))
    }

    inline def four(in: PosNegCounter): PosNegCounter = choiceSplit(3) match {
      case "pos" => PosNegCounter(in.pos.inc(), GrowOnlyCounter.bottom.empty)
      case "neg" => PosNegCounter(GrowOnlyCounter.bottom.empty, in.pos.inc())
    }

    one(state)
  }
}
