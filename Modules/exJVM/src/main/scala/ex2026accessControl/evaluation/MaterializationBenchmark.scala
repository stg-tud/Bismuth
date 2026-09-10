package ex2026accessControl.evaluation

import ex2026accessControl.travelplanner.TravelPlan
import org.openjdk.jmh.annotations.*
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore}

import java.util.concurrent.TimeUnit
import scala.util.Random

/** Holds a randomly generated [[ArdtEventGraph]] of TravelPlan edits, built once per JMH trial (i.e. before
  * warmup/measurement iterations start, so its construction is never included in the measured time). The graph
  * is generated deterministically from [[seed]], so every fork/trial with the same `@Param` values operates on
  * the exact same trace.
  */
@State(Scope.Benchmark)
class MaterializationBenchmarkState {

  // The total number of TravelPlan edits performed, distributed among replicas at random. This controls the
  // size of the generated event graph.
  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int                = 10
  val minEntriesPerMapPerReplica: Int = 5
  val maxEntriesPerMapPerReplica: Int = 50
  val concurrencyProbability: Double  = 0.2
  val seed: Long                      = 42L

  var eventGraph: ArdtEventGraph[TravelPlan]       = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[TravelPlan] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    val generated        = TraceGeneration.generateEventGraph(
      numReplicas,
      numEvents,
      minEntriesPerMapPerReplica,
      maxEntriesPerMapPerReplica,
      concurrencyProbability
    )
    eventGraph = generated.eventGraph
    deltaValueStore = generated.deltaValueStore
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class MaterializationBenchmark {

  /** Full state materialization, including access control enforcement (capability/write-permission filtering and
    * revocation/causality checks), as used in production.
    */
  @Benchmark
  def materializeWithAuthorization(state: MaterializationBenchmarkState): TravelPlan =
    Authorization.materialize(state.eventGraph, state.deltaValueStore)

  /** Materializes the very same trace by merging every delta value in causal-order-independent fashion, without
    * any access control checks. The difference to [[materializeWithAuthorization]] is the overhead added by
    * access control enforcement.
    */
  @Benchmark
  def materializeWithoutAuthorization(state: MaterializationBenchmarkState): TravelPlan =
    UnauthorizedMaterialize.materialize(state.eventGraph, state.deltaValueStore)
}

object Runner {
  def main(args: Array[String]): Unit = {
    val state = new MaterializationBenchmarkState()
    state.numEvents = 100_000
    state.setup()
    val bench = new MaterializationBenchmark()
    println("Done with setup")
    val res2 = {
      val timeStart = System.nanoTime()
      val result    = bench.materializeWithoutAuthorization(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
      result
    }
    val res1 = {
      val timeStart = System.nanoTime()
      val result    = bench.materializeWithAuthorization(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
      result
    }

    require(res1 == res2)
  }
}
