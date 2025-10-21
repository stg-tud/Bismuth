package benchmarks.reactor

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*
import reactives.extra.reactor.{Reactor, S, Stage}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(5)
@Threads(1)
@State(Scope.Thread)
class BaselineUntilBenchmark {
  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine

  import stableEngine.*

  var reactor: Reactor[Int] = scala.compiletime.uninitialized
  var trigger: Evt[Unit]    = scala.compiletime.uninitialized

  @Setup
  def setup(engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    trigger = Evt[Unit]()
    reactor = Reactor.loop(0) {
      S.until(
        trigger,
        body =
          S.end: Stage[Int],
        interruptHandler =
          S.end: Stage[Int]
      )
    }
  }

  @Benchmark
  def run(): Unit = trigger.fire()
}
