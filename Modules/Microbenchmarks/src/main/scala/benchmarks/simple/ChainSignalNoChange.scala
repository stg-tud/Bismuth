package benchmarks.simple

import benchmarks.{EngineParam, Size, Workload}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ChainSignalNoChange {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var source: Var[Int]    = scala.compiletime.uninitialized
  var result: Signal[Int] = scala.compiletime.uninitialized

  @Setup
  def setup(size: Size, engineParam: EngineParam, work: Workload): Unit = {
    engine = engineParam.engine
    source = Var(0)
    result = source
    for _ <- Range(0, size.size) do {
      result = result.map { v =>
        val r = v + 1; work.consume(); r
      }
    }
  }

  @Benchmark
  def run(): Unit = source.set(0)
}
