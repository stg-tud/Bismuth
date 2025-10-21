package benchmarks.dynamic

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Range

/** creates a chain per thread, and connects the result dynamically with the chain of another
  * sources -> chain … chain -> result <- dynamically chain of other source
  */
@State(Scope.Benchmark)
class StackState {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var sources: Array[Var[Int]]     = scala.compiletime.uninitialized
  var results: Array[Signal[Int]]  = scala.compiletime.uninitialized
  var dynamics: Array[Signal[Int]] = scala.compiletime.uninitialized
  var isManual: Boolean            = false

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, eParam: EngineParam, work: Workload, size: Size, step: Step): Unit = {
    engine = eParam.engine
    val threads = params.getThreads
    if reactives.SelectedScheduler.candidate.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged then {
      isManual = true
    }
    sources = Range(0, threads).map(_ => Var(0)).toArray
    results = sources.map { source =>
      var cur: Signal[Int] = source
      for _ <- Range(0, size.size) do cur = cur.map(1.+)
      cur.map { x => work.consume(); x }
    }

    dynamics = results.zipWithIndex.map {
      case (r, i) =>
        Signal.dynamic {
          val v   = r.value
          val idx = i + (if step.test(v) then 2 else 1)
          results(idx % threads).value
        }
    }
  }
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(8)
class Stacks {

  @Benchmark
  def run(state: StackState, step: Step, params: ThreadParams): Int = {
    import state.stableEngine.*
    if state.isManual then
      state.synchronized {
        val index = params.getThreadIndex % params.getThreadCount
        state.sources(index).set(step.run())
        state.dynamics(index).readValueOnce
      }
    else {
      val index = params.getThreadIndex % params.getThreadCount
      state.sources(index).set(step.run())
      state.dynamics(index).readValueOnce
    }
  }

}
