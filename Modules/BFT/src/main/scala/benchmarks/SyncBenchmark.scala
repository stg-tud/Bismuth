package benchmarks

import datatypes.ORSet
import org.openjdk.jmh.annotations.*

import java.io.*
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.util.Random


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

  @Param(Array("10000"))
  var size: Int = 0
  @Param(Array(/*"0.01", "0.05", "0.1", "0.2", "0.5", */"0.8"/*, "0.9", "1"*/))
  var diff: Float = 0
  //@Param(Array("1", "10", "100", "1000"))
  var deltaSizeInKiloBytes: Int = 10

  var r1 = ORSet[String]()
  var r2 = ORSet[String]()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val gen = ReplicaGenerator.generate(size, diff, r1, r2, deltaSizeInKiloBytes)

    r1 = gen._1
    r2 = gen._2

  }

  @Benchmark
  def sync(): Unit = {

    for i <- Range(1, 31) do
      println(s"RIBLT $i")
      var res = SyncStrategies.syncRIBLT(r1, r2, i, size, diff, deltaSizeInKiloBytes)
      MyCollector.add(res)

    var l = List.empty[Int]
    val rand = new Random()
    for i <- Range(0, 100) do
      val n = rand.nextInt(1000) + 30
      l = l :+ n

    for i <- l.sorted do
      println(s"RIBLT $i")
      var res = SyncStrategies.syncRIBLT(r1, r2, i, size, diff, deltaSizeInKiloBytes)
      MyCollector.add(res)

    for i <- Range(1000, 4000, 500) do
      println(s"RIBLT $i")
      var res = SyncStrategies.syncRIBLT(r1, r2, i, size, diff, deltaSizeInKiloBytes)
      MyCollector.add(res)


    for i <- Range(1, 300) do
      println(s"RSync $i")
      var res = SyncStrategies.rsync(r1, r2, size, diff, i, deltaSizeInKiloBytes)
      MyCollector.add(res)



    // RIBLT
    /*println("rib1")
    var res = SyncStrategies.syncRIBLT(r1, r2, 1, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    println("rib2")
    res = SyncStrategies.syncRIBLT(r1, r2, 5, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt1 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 10, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt2 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 20, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt3 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 50, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt4 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 72, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt5 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 96, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt6 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 158, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt7 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 364, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt8 = res.roundTrips

    res = SyncStrategies.syncRIBLT(r1, r2, 1000, size, diff, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt9 = res.roundTrips

    println("check1")

    // Recursive Sync
    val d = diff * size
    res = SyncStrategies.syncPingPong(r1, r2, size, diff, 1, deltaSizeInKiloBytes)
    MyCollector.add(res)
    val rt = res.roundTrips

    println("check2")

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt1, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    println("check3")


    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt2, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt3, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt4, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt5, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt6, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt7, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt8, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)

    res = SyncStrategies.syncPingPong(r1, r2, size, diff, calculateNeededRSyncDepth(rt9, rt), deltaSizeInKiloBytes)
    MyCollector.add(res)*/
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    val allValues = MyCollector.getAll

    val writer = new FileWriter("src/main/scala/benchmarks/benchmark.csv", true)
    benchmarks.Measurement.writeCSVRows(writer, allValues)
  }

  def calculateNeededRSyncDepth(ribltRT: Int, RSyncRT: Int): Int =
    val tmp = RSyncRT / ribltRT
    if tmp == 0 || tmp == 1 then
      2
    else
      tmp

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
