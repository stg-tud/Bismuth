package benchmarks

import datatypes.ORSet
import org.openjdk.jmh.annotations.*
import riblt.RIBLT.{given_Hashable_String, given_Xorable_String}
import riblt.RIBLT
import scala.util.Random
import java.util.concurrent.TimeUnit

@AuxCounters(AuxCounters.Type.EVENTS)
@State(Scope.Thread)
class Metrics {
  var roundTrips: Int = 0
  var bytesSent: Int = 0
}


@Fork(2)
@Warmup(iterations = 2)
@Measurement(iterations = 2)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class SyncBenchmark {

  //@Param(Array("1000"))
  //var size: Int = 0
  //@Param(Array("0.6", "0.8"))
  //var diff: Float = 0

  var replica1 = ORSet[String]()
  var replica2 = ORSet[String]()
  val riblt1   = RIBLT[String]()
  val riblt2   = RIBLT[String]()

  @Setup(Level.Trial)
  def setup(): Unit = {

    for i <- 0 to 10000 do
      val r = Random().nextDouble()
      if r <= 0.85 then {
        replica1 = replica1.merge(replica1.add(i.toString))
        replica2 = replica2.merge(replica2.add(i.toString))
      } else
        val rr = Random().nextDouble()
        if rr <= 0.5 then
          replica1 = replica1.merge(replica1.add(i.toString))
        else
          replica2 = replica2.merge(replica2.add(i.toString))

    
  }


  @Benchmark
  def sync(metrics: Metrics): Unit = {
    for id <- replica1.hashDAG.getIDs do
      riblt1.addSymbol(id)

    for id <- replica2.hashDAG.getIDs do
      riblt2.addSymbol(id)
      
    while !riblt1.isDecoded do {
      val cs = riblt2.produceNextCodedSymbol
      riblt1.addCodedSymbol(cs)
      metrics.roundTrips += 1
    }
  }
}
