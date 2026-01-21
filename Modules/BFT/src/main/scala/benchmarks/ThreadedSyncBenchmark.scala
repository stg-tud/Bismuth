package benchmarks

import datatypes.ORSet
import network.Network
import org.openjdk.jmh.annotations.*
import riblt.RIBLTSyncWithThreads
import riblt.SessionType.{receiver, sender}

import scala.util.Random
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ThreadedSyncBenchmark {

  var replica1 = ORSet[String]()
  var replica2 = ORSet[String]()
  var riblt1   = RIBLTSyncWithThreads(replica1, "replica1")
  var riblt2   = RIBLTSyncWithThreads(replica2, "replica2")
  val random   = new scala.util.Random(42)
  Network.startChannel("replica1")
  Network.startChannel("replica2")

  @Setup(Level.Trial)
  def setup(): Unit = {

    for i <- 0 to 10 do
        val r = Random().nextDouble()
        if r <= 0.8 then {
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
  def sync(): Unit = {
    val t1 = riblt1.startSession("replica2", sender)
    val t2 = riblt2.startSession("replica1", receiver)

    t1.join()
    t2.join()
  }

}
