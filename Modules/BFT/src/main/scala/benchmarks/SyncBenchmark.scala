package benchmarks

import datatypes.ORSet
import org.openjdk.jmh.annotations.*
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}
import riblt.RIBLT

import java.io.*
import scala.util.Random
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

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

  @Param(Array("1000", "10000", "100000"))
  var size: Int = 0
  @Param(Array("0.6", "0.8", "0.9"))
  var diff: Float = 0

  var replica1 = ORSet[String]()
  var replica2 = ORSet[String]()
  var riblt1   = RIBLT[String]()
  var riblt2   = RIBLT[String]()

  @Setup(Level.Invocation)
  def setup(): Unit = {

    replica1 = ORSet[String]()
    replica2 = ORSet[String]()
    riblt1 = RIBLT[String]()
    riblt2 = RIBLT[String]()

    for i <- 0 to size do
        val r = Random().nextDouble()
        if r <= diff then {
          var tmp = ORSet[String]()
          val rr  = Random().nextDouble()
          if rr <= 0.5 then
              tmp = replica1.add(i.toString)
          else
              tmp = replica2.add(i.toString)

          replica1 = replica1.merge(tmp)
          replica2 = replica2.merge(tmp)
        } else
            val rr = Random().nextDouble()
            if rr <= 0.5 then
                replica1 = replica1.merge(replica1.add(i.toString))
            else
                replica2 = replica2.merge(replica2.add(i.toString))

  }

  @Benchmark
  def sync(syncMetrics: SyncMetrics): Unit = {
    var r = 0

    for id <- replica1.hashDAG.getIDs do
        riblt1.addSymbol(id)

    for id <- replica2.hashDAG.getIDs do
        riblt2.addSymbol(id)

    while !riblt1.isDecoded do {
      val cs = riblt2.produceNextCodedSymbol
      riblt1.addCodedSymbol(cs)
      r += 1
      //println(r)
    }

    //println("DONE")

    syncMetrics.roundtripsAll += r
    syncMetrics.run += 1
    MyCollector.add(r)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    val allValues = MyCollector.getAll()

    val writer = new FileWriter("src/main/scala/benchmarks/benchmark.csv", true)
    val printer = new PrintWriter(writer)

    printer.println(s"size = $size, diff = $diff")
    printer.println(allValues)//.foldLeft("")((z, i) => z.concat(s"${i.toString}, ")))
    printer.close()
  }

  private object MyCollector {
    private val buf = ListBuffer[Int]()

    def add(v: Int): Unit = synchronized { buf += v }: Unit

    def getAll(): Seq[Int] = buf.toList

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
