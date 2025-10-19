package benchmarks.incremental
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import reactives.default.*
import reactives.extra.incremental.*

import java.util.concurrent.TimeUnit

/** @author gerizuna
  * @since 10.10.19
  */

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@State(Scope.Thread)
class SizeBenchmarkWithInsert {

  @Param(Array("1", "5", "10", "50", "100", "500", "1000", "5000", "10000"))
  var arg: Int = scala.compiletime.uninitialized

  var addEvent: Evt[Int]     = scala.compiletime.uninitialized
  var sizeOfSeq: Signal[Int] = scala.compiletime.uninitialized

  var reactSeq: IncSeq[Int]       = scala.compiletime.uninitialized
  var sizeOfReactSeq: Signal[Int] = scala.compiletime.uninitialized

  @Setup(Level.Invocation)
  def prepare(): Unit = {
    addEvent = Evt[Int]()
    val seq = addEvent.fold((1 to arg).toList) { (s, x) =>
      s :+ x
    }
    sizeOfSeq = Signal {
      seq.value.size
    }

    reactSeq = IncSeq.empty[Int]
    sizeOfReactSeq = reactSeq.size
    seq.now.foreach(x => reactSeq.add(x))
  }

  @Benchmark
  def testSeq(blackHole: Blackhole): Unit = {
    addEvent.fire(arg)
    blackHole.consume(addEvent)
    blackHole.consume(sizeOfSeq)
  }

  @Benchmark
  def testReactSeq(blackHole: Blackhole): Unit = {
    reactSeq.add(arg)
    blackHole.consume(reactSeq)
    blackHole.consume(sizeOfReactSeq)
  }

}
