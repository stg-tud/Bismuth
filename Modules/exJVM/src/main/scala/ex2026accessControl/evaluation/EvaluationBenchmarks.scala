package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.Hash
import crypto.channels.PrivateIdentity
import ex2026accessControl.evaluation.EvaluationBenchmark.noopOnStateChange
import ex2026accessControl.travelplanner.TravelPlan
import org.openjdk.jmh.annotations.*
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore, Replica}

import java.util.concurrent.TimeUnit
import scala.util.Random

/** Holds a randomly generated [[ArdtEventGraph]] of TravelPlan edits, built once per JMH trial (i.e. before
  * warmup/measurement iterations start, so its construction is never included in the measured time), together
  * with the pre-encoded events and delta-commitment classification needed to feed them into a [[Replica]] via
  * `receiveEvent`/`receiveDelta`. The graph is generated deterministically from [[seed]], so every fork/trial
  * with the same `@Param` values operates on the exact same trace.
  */
@State(Scope.Benchmark)
class TravelPlanTraceBenchmarkState {

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
  var genesisHash: Hash                            = scala.compiletime.uninitialized
  var rootIdentity: PrivateIdentity                = scala.compiletime.uninitialized
  var trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    scala.compiletime.uninitialized

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
    genesisHash = generated.eventGraph.genesis
    rootIdentity = generated.replicaIds(0)
    trace = generated.eventGraph.allEventsInCausalOrder.map { (hash, event) =>
      val deltaCommitment = event.payload match {
        case DeltaCommitment(commitment) => Some(commitment)
        case _                           => None
      }
      (hash = hash, encodedEvent = writeToArray(event), deltaCommitment = deltaCommitment)
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class EvaluationBenchmark {

  /** Full state materialization, including access control enforcement (capability/write-permission filtering and
    * revocation/causality checks), as used in production.
    */
  @Benchmark
  def materializeWithAuthorization(state: TravelPlanTraceBenchmarkState): TravelPlan =
    Authorization.materialize(state.eventGraph, state.deltaValueStore)

  /** Materializes the very same trace by merging every delta value in causal-order-independent fashion, without
    * any access control checks. The difference to [[materializeWithAuthorization]] is the overhead added by
    * access control enforcement.
    */
  @Benchmark
  def materializeWithoutAuthorization(state: TravelPlanTraceBenchmarkState): TravelPlan =
    UnauthorizedMaterialize.materialize(state.eventGraph, state.deltaValueStore)

  /** Ingests the entire trace into a freshly constructed [[Replica]] via `receiveEvent`/`receiveDelta`, mirroring
    * how a replica processes events and delta payloads received from its peers. A fresh replica is required per
    * invocation since `Replica` is stateful: replaying the same trace into an already-populated replica would
    * make every subsequent invocation a cheap no-op. Overrides the class-level warmup/measurement durations
    * since a single invocation is far cheaper than one round of materialization.
    */
  @Benchmark
  def receiveEventsAndDeltas(state: TravelPlanTraceBenchmarkState): Set[Hash] = {
    val replica = new Replica[TravelPlan](
      state.genesisHash,
      state.rootIdentity,
      r => NoOpAntiEntropy(r),
      noopOnStateChange
    )

    state.trace.foreach { (hash, encodedEvent, deltaCommitment) =>
      replica.receiveEvent(encodedEvent)
      deltaCommitment.foreach { commitment =>
        state.deltaValueStore.get(commitment).foreach(revealed => replica.receiveDelta(hash, revealed))
      }
    }

    replica.heads
  }
}

object EvaluationBenchmark {
  def noopOnStateChange[T](x: => T): Unit = ()
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    val state = new TravelPlanTraceBenchmarkState()
    state.numEvents = 100_000
    state.setup()
    val bench = new EvaluationBenchmark()
    println("Done with setup")

    val res1 = {
      val timeStart = System.nanoTime()
      val result    = bench.materializeWithAuthorization(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
      result
    }

    val res2 = {
      val timeStart = System.nanoTime()
      val result    = bench.materializeWithoutAuthorization(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
      result
    }
    require(res1 == res2)

    val timeStart = System.nanoTime()
    bench.receiveEventsAndDeltas(state)
    println((System.nanoTime() - timeStart) / 1_000_000_000.0)
  }
}
