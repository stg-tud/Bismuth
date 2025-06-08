package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class GSetBench {
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = scala.compiletime.uninitialized

  var set: NamedDeltaBuffer[Set[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    set = (0 until size).foldLeft(NamedDeltaBuffer("a".asId, Set.empty[Int])) {
      case (s, e) => s.mod(_ => Set(e))
    }
  }

  @Benchmark
  def elements(): Set[Int] = set.state

  @Benchmark
  def insert(): NamedDeltaBuffer[Set[Int]] = set.mod(_ => Set(-1))
}
