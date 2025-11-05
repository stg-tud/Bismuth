package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.ReplicatedSet

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AWSetBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = scala.compiletime.uninitialized

  var set: NamedDeltaBuffer[ReplicatedSet[Int]] = scala.compiletime.uninitialized

  def createBySize(size: Int): NamedDeltaBuffer[ReplicatedSet[Int]] =
    (0 until size).foldLeft(NamedDeltaBuffer("a".asId, ReplicatedSet.empty[Int])) {
      case (s, e) => s.mod(_.add(using s.replicaID)(e))
    }

  @Setup
  def setup(): Unit =
    set = createBySize(size)

  @Benchmark
  def elements(): Set[Int] = set.state.elements

  @Benchmark
  def add(): NamedDeltaBuffer[ReplicatedSet[Int]] = set.mod(_.add(using set.replicaID)(-1))

  @Benchmark
  def addAll(): NamedDeltaBuffer[ReplicatedSet[Int]] =
      val ndb = NamedDeltaBuffer("a".asId, ReplicatedSet.empty[Int])
      ndb.mod(_.addAll(using ndb.replicaID)(0 until size))

  @Benchmark
  def remove(): NamedDeltaBuffer[ReplicatedSet[Int]] = set.mod(_.remove(0))

  @Benchmark
  def removeBy(): NamedDeltaBuffer[ReplicatedSet[Int]] = set.mod(_.removeBy((e: Int) => e == 0))

  @Benchmark
  def removeAll(): NamedDeltaBuffer[ReplicatedSet[Int]] = set.mod(_.removeAll(set.state.elements))

  @Benchmark
  def clear(): NamedDeltaBuffer[ReplicatedSet[Int]] = set.mod(_.clear())

  @Benchmark
  def construct(): NamedDeltaBuffer[ReplicatedSet[Int]] = createBySize(size)
}
