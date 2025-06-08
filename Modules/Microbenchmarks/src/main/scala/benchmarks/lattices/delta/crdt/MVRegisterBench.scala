package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.MultiVersionRegister

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class MVRegisterBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var numWrites: Int = scala.compiletime.uninitialized

  given Lattice[Int]                                   = math.max
  var reg: NamedDeltaBuffer[MultiVersionRegister[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(NamedDeltaBuffer("-1".asId, MultiVersionRegister.empty[Int])) {
      case (r, i) =>
        given rid: LocalUid = i.toString.asId
        val delta           = MultiVersionRegister.empty[Int].write(i)
        r.applyDelta(rid.uid, delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.state.read

  @Benchmark
  def write(): NamedDeltaBuffer[MultiVersionRegister[Int]] = reg.mod(_.write(using reg.replicaID)(-1))

  @Benchmark
  def clear(): NamedDeltaBuffer[MultiVersionRegister[Int]] = reg.mod(_.clear())
}
