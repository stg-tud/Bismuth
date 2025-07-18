package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.ReplicatedList

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ReplicatedListBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var listSize: Int = scala.compiletime.uninitialized

  type SUT = NamedDeltaBuffer[ReplicatedList[Int]]

  var rga: SUT        = scala.compiletime.uninitialized
  var rgaCleared: SUT = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    rga = NamedDeltaBuffer("a".asId, ReplicatedList.empty[Int]).mod(_.appendAll(using "".asId)(0 until listSize))
    rgaCleared = rga.mod(_.clear())
  }

  @Benchmark
  def readFirst(): Option[Int] = rga.state.read(0)

  @Benchmark
  def readLast(): Option[Int] = rga.state.read(listSize - 1)

  @Benchmark
  def size(): Int = rga.state.sizeIncludingDeadElements

  @Benchmark
  def toList: List[Int] = rga.state.toList

  @Benchmark
  def prepend(): SUT = rga.mod(_.prepend(using rga.replicaID)(-1))

  @Benchmark
  def append(): SUT = rga.mod(_.append(using rga.replicaID)(listSize))

  @Benchmark
  def prependTen(): SUT = rga.mod(_.prependAll(using rga.replicaID)(-10 to -1))

  @Benchmark
  def appendTen(): SUT = rga.mod(_.appendAll(using rga.replicaID)(listSize until listSize + 10))

  @Benchmark
  def updateFirst(): SUT = rga.mod(_.update(0, -1))

  @Benchmark
  def updateLast(): SUT = rga.mod(_.update(listSize - 1, -1))

  @Benchmark
  def deleteFirst(): SUT = rga.mod(_.delete(0))

  @Benchmark
  def deleteLast(): SUT = rga.mod(_.delete(listSize - 1))

  @Benchmark
  def clear(): SUT = rga.mod(_.clear())

  @Benchmark
  def purgeTombstones(): SUT = rgaCleared.mod(_.purgeTombstones())
}
