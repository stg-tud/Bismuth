package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.{Decompose, LocalUid}
import rdts.datatypes.{EnableWinsFlag, ObserveRemoveMap}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ORMapBench {

  given decompose[K, V]: Decompose[ObserveRemoveMap[K, V]] = Decompose.atomic

  @Param(Array("1", "10", "100", "1000"))
  var numEntries: Int = scala.compiletime.uninitialized

  type SUT = NamedDeltaBuffer[ObserveRemoveMap[Int, EnableWinsFlag]]

  var map: SUT = scala.compiletime.uninitialized

  given LocalUid = LocalUid.predefined("a")

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(NamedDeltaBuffer[ObserveRemoveMap[Int, EnableWinsFlag]](
      summon,
      ObserveRemoveMap.empty
    )) {
      case (m, i) => m.mod(_.transform(i)(_.map(_.enable(using m.replicaID)())))
    }
  }

  def queryExisting(): Boolean = map.state.get(0).exists(_.read)

  @Benchmark
  def queryMissing(): Boolean = map.state.get(-1).exists(_.read)

  @Benchmark
  def containsExisting(): Boolean = map.state.contains(0)

  @Benchmark
  def containsMissing(): Boolean = map.state.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.state.queryAllEntries.map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mod(_.transform(0)(_.map(_.disable())))

  @Benchmark
  def mutateMissing(): SUT = map.mod(_.transform(-1)(_.map(_.enable())))

  @Benchmark
  def removeExisting(): SUT = map.mod(_.remove(0))

  @Benchmark
  def removeMissing(): SUT = map.mod(_.remove(-1))

  @Benchmark
  def removeAll(): SUT = map.mod(_.removeAll(0 until numEntries))

  @Benchmark
  def removeByValue(): SUT = map.mod(_.removeByValue(ewf => ewf.read))

  @Benchmark
  def clear(): SUT = map.mod(_.clear())
}
