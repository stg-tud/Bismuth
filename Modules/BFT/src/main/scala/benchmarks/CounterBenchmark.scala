package benchmarks

import datatypes.Counter
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class CounterBenchmark {

  var counter1: Counter = Counter()
  var counter2: Counter = Counter()

  @Setup(Level.Iteration)
  def setup(): Unit = {
    counter1 = Counter()
    counter2 = Counter()
  }

  @Benchmark
  def testIncrement(): Counter = {
    val delta = counter1.inc
    counter1 = counter1.merge(delta)
    delta
  }

  @Benchmark
  def testDecrement(): Counter = {
    val delta = counter1.dec
    counter1 = counter1.merge(delta)
    delta
  }

  @Benchmark
  def testMerge(): Counter = {
    val d1 = counter1.add(10)
    val d2 = counter2.add(-5)

    counter1 = counter1.merge(d1)
    counter2 = counter2.merge(d1)

    counter1 = counter1.merge(d2)
    counter2 = counter2.merge(d2)

    d1
  }

  @Benchmark
  def testValueComputation(): Int =
    counter1.value
}
