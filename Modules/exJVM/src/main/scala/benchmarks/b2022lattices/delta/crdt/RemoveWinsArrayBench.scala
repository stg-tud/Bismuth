package benchmarks.b2022lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.RemoveWinsArray

import java.util.concurrent.TimeUnit
import rdts.base.Lattice

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

  var sut: SUT = scala.compiletime.uninitialized

  given intLattice: Lattice[Int] = math.max

  @Setup
  def setup(): Unit = {
    sut = NamedDeltaBuffer("a".asId, RemoveWinsArray.empty[Int]).mod(_.appendAll(0 until listSize)(using "".asId))
  }

  @Benchmark
  def readFirst(): Option[Int] = sut.state.read(0)

  @Benchmark
  def readLast(): Option[Int] = sut.state.read(listSize - 1)

  @Benchmark
  def size(): Int = sut.state.size

  @Benchmark
  def toList: List[Int] = sut.state.toList

  @Benchmark
  def prepend(): SUT = sut.mod(_.prepend(-1)(using sut.replicaID))

  @Benchmark
  def append(): SUT = sut.mod(_.append(listSize)(using sut.replicaID))

  @Benchmark
  def prependTen(): SUT = sut.mod(_.prependAll(-10 to -1)(using sut.replicaID))

  @Benchmark
  def appendTen(): SUT = sut.mod(_.appendAll(listSize until listSize + 10)(using sut.replicaID))

  @Benchmark
  def deleteFirst(): SUT = sut.mod(_.remove(0))

  @Benchmark
  def deleteLast(): SUT = sut.mod(_.remove(listSize - 1))

}
