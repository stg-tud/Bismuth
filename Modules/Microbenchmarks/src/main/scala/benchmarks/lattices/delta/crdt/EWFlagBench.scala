package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.EnableWinsFlag

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: NamedDeltaBuffer[EnableWinsFlag]  = scala.compiletime.uninitialized
  var flagDisabled: NamedDeltaBuffer[EnableWinsFlag] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    flagEnabled = NamedDeltaBuffer("a".asId, EnableWinsFlag.empty).mod(_.enable(using "a".asId)())
    flagDisabled = NamedDeltaBuffer("b".asId, EnableWinsFlag.empty).mod(_.disable())
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.state.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.state.read

  @Benchmark
  def enableEnabled(): NamedDeltaBuffer[EnableWinsFlag] = flagEnabled.mod(_.enable(using flagEnabled.replicaID)())

  @Benchmark
  def enableDisabled(): NamedDeltaBuffer[EnableWinsFlag] = flagDisabled.mod(_.enable(using flagDisabled.replicaID)())

  @Benchmark
  def disableEnabled(): NamedDeltaBuffer[EnableWinsFlag] = flagEnabled.mod(_.disable())

  @Benchmark
  def disableDisabled(): NamedDeltaBuffer[EnableWinsFlag] = flagDisabled.mod(_.disable())
}
