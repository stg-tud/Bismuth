package benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import dag.Event
import datatypes.{Counter, ORSet}
import org.openjdk.jmh.annotations.*
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}
import riblt.RIBLT
import java.io.*
import scala.util.Random
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

given c2: JsonValueCodec[Event[Int]] = JsonCodecMaker.make

@AuxCounters(AuxCounters.Type.EVENTS)
@State(Scope.Thread)
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

  @Param(Array("100", "500", "1000", "5000", "10000"))
  var size: Int = 0
  @Param(Array("0.01", "0.05", "0.1", "0.2", "0.5", "0.8", "0.9", "1"))
  var diff: Float = 0
  @Param(Array("1", "5", "10", "20"))
  var codedSymbolPerRoundTrip: Int = 1

  var r1 = Counter()
  var r2 = Counter()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val gen = ReplicaGenerator.generate(size, diff, r1, r2, 0)

    r1 = gen._1
    r2 = gen._2

  }

  @Benchmark
  def sync(syncMetrics: SyncMetrics): Unit = {

    val res = SyncStrategies.syncRIBLT(r1, r2, codedSymbolPerRoundTrip)
    MyCollector.add("RIBLT", size, diff, res._1, res._2, codedSymbolPerRoundTrip)

    if codedSymbolPerRoundTrip == 1 then
      val res2 = SyncStrategies.syncPingPong(r1, r2)
      MyCollector.add("Traditional", size, diff, res2._1, res2._2, -1)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    val allValues = MyCollector.getAll

    val writer  = new FileWriter("src/main/scala/benchmarks/benchmark.csv", true)
    benchmarks.Measurement.writeCSVRows(writer, allValues)
  }

  private object MyCollector {
    private val buf = ListBuffer[benchmarks.Measurement]()

    def add(method: String, size: Int, diff: Float, roundTrip: Int, bandwidth: Int, codedSymbolPerRoundTrip: Int): Unit =
      synchronized {
        buf += benchmarks.Measurement(method, size, diff, roundTrip, bandwidth, codedSymbolPerRoundTrip)
      }: Unit

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
