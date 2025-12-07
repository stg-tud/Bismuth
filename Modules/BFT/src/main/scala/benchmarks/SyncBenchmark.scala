package benchmarks

import datatypes.ORSet
import org.openjdk.jmh.annotations.*

import java.io.*
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer


/*@AuxCounters(AuxCounters.Type.EVENTS)
@State(Scope.Thread)
*/
class SyncMetrics {
  var roundtripsAll: Int = 0
  var run                = 0
}

@Fork(1)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class SyncBenchmark {

  @Param(Array("100", "500", "1000"/*, "5000","10000"*/))
  var size: Int = 0
  @Param(Array("0.01", "0.05", "0.1", "0.2", "0.5", "0.8", "0.9", "1"))
  var diff: Float = 0
  @Param(Array("1", "10", "100", "1000"))
  var deltaSize: Int = 1

  var r1 = ORSet[String]()
  var r2 = ORSet[String]()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val gen = ReplicaGenerator.generate(size, diff, r1, r2, deltaSize)

    r1 = gen._1
    r2 = gen._2

  }

  @Benchmark
  def sync(): Unit = {

    var res = SyncStrategies.syncRIBLT(r1, r2, 1, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 5, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 10, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 20, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 50, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 100, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncRIBLT(r1, r2, 1000, size, diff, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 1, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 5, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 10, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 20, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 50, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 100, deltaSize)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 1000, deltaSize)
    MyCollector.add(res)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    val allValues = MyCollector.getAll

    val writer = new FileWriter("src/main/scala/benchmarks/benchmark.csv", true)
    benchmarks.Measurement.writeCSVRows(writer, allValues)
  }

  private object MyCollector {
    private val buf = ListBuffer[benchmarks.Measurement]()

    def add(measurement: benchmarks.Measurement): Unit = synchronized { buf += measurement }: Unit

    def getAll: Seq[benchmarks.Measurement] = buf.toList

    def clear(): Unit = this.synchronized {
      buf.clear()
    }
  }
}

/*
[info] Benchmark                         (diff)  (size)   Mode  Cnt     Score   Error   Units
[info] SyncBenchmark.sync                   0.6    1000  thrpt          0,019          ops/ms
[info] SyncBenchmark.sync:roundtripsAll     0.6    1000  thrpt       1196,000               #
[info] SyncBenchmark.sync:run               0.6    1000  thrpt          2,000               #
[info] SyncBenchmark.sync                   0.8    1000  thrpt          0,022          ops/ms
[info] SyncBenchmark.sync:roundtripsAll     0.8    1000  thrpt        617,000               #
[info] SyncBenchmark.sync:run               0.8    1000  thrpt          2,000               #
 */
