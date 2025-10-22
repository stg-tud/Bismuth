package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.RemoveWinsArray

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RemoveWinsArrayBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var listSize: Int = scala.compiletime.uninitialized

  type SUT = NamedDeltaBuffer[RemoveWinsArray[Int]]

  var rga: SUT = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    rga = NamedDeltaBuffer("a".asId, RemoveWinsArray.empty[Int]).mod(_.appendAll(0 until listSize)(using "".asId))
  }

  @Benchmark
  def readFirst(): Option[Int] = rga.state.read(0)

  @Benchmark
  def readLast(): Option[Int] = rga.state.read(listSize - 1)

  @Benchmark
  def size(): Int = rga.state.size

  @Benchmark
  def toList: List[Int] = rga.state.toList

  @Benchmark
  def prepend(): SUT = rga.mod(_.prepend(-1)(using rga.replicaID))

  @Benchmark
  def append(): SUT = rga.mod(_.append(listSize)(using rga.replicaID))

  @Benchmark
  def prependTen(): SUT = rga.mod(_.prependAll(-10 to -1)(using rga.replicaID))

  @Benchmark
  def appendTen(): SUT = rga.mod(_.appendAll(listSize until listSize + 10)(using rga.replicaID))

  @Benchmark
  def deleteFirst(): SUT = rga.mod(_.remove(0))

  @Benchmark
  def deleteLast(): SUT = rga.mod(_.remove(listSize - 1))

}
