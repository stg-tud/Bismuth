package benchmarks.reactor

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(5)
@Threads(1)
@State(Scope.Thread)
class BaselineEventPropagation {
  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var event: Evt[Int]     = scala.compiletime.uninitialized
  var signal: Signal[Int] = scala.compiletime.uninitialized

  @Setup
  def setup(engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    event = Evt[Int]()
    signal = event.hold(0)
  }

  @Benchmark
  def run(): Unit = event.fire(signal.now + 1)
}
