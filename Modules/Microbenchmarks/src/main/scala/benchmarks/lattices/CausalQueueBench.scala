package benchmarks.lattices

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid
import rdts.datatypes.ReplicatedList

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class CausalQueueBenchWithRGA {

  @Param(Array("100", "1000", "10000", "100000"))
  var size: Int = scala.compiletime.uninitialized

  @Param(Array("10000"))
  var operations: Int = scala.compiletime.uninitialized

  var lca: ReplicatedList[Int] = scala.compiletime.uninitialized

  given LocalUid = LocalUid.predefined("a")

  @Setup
  def setup(): Unit = {
    lca = (1 to size).foldLeft(ReplicatedList.empty[Int]) { (q, e) => q.prepend(e) }
  }

  def make(base: ReplicatedList[Int], ops: Int) = {
    val s     = ops / 2
    val added = (1 to s).foldLeft(base) { (acc, v) => acc.prepend(v) }
    (1 to s).foldLeft(added) { (acc, _) => acc.delete(0) }
  }

  @Benchmark
  def create(): ReplicatedList[Int] = make(lca, operations)

  @Benchmark
  def createAndMerge(): ReplicatedList[Int] = {
    val left  = make(lca, operations)
    val right = make(lca, operations)

    val res = left `merge` right
    res
  }

}
