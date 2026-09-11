package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.Hash
import crypto.channels.PrivateIdentity
import ex2026accessControl.travelplanner.TravelPlan
import org.openjdk.jmh.annotations.*
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{DeltaValueStore, Replica}

import java.util.concurrent.TimeUnit
import scala.util.Random

/** Holds a randomly generated trace of TravelPlan edits, built once per JMH trial (i.e. before warmup/measurement
  * iterations start), together with the pre-encoded events and delta-commitment classification needed to feed
  * them into a [[Replica]] via `receiveEvent`/`receiveDelta`. The graph is generated deterministically from
  * [[seed]], so every fork/trial with the same `@Param` values operates on the exact same trace as
  * [[MaterializationBenchmarkState]].
  */
@State(Scope.Benchmark)
class ReceiveBenchmarkState {

  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int                = 10
  val minEntriesPerMapPerReplica: Int = 5
  val maxEntriesPerMapPerReplica: Int = 50
  val concurrencyProbability: Double  = 0.2
  val seed: Long                      = 42L

  var genesisHash: Hash                             = scala.compiletime.uninitialized
  var rootIdentity: PrivateIdentity                 = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[TravelPlan]  = scala.compiletime.uninitialized
  var trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    val generated = TraceGeneration.generateEventGraph(
      numReplicas,
      numEvents,
      minEntriesPerMapPerReplica,
      maxEntriesPerMapPerReplica,
      concurrencyProbability
    )

    genesisHash = generated.eventGraph.genesis
    rootIdentity = generated.replicaIds(0)
    deltaValueStore = generated.deltaValueStore
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
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class ReceiveBenchmark {

  /** Ingests the entire trace into a freshly constructed [[Replica]] via `receiveEvent`/`receiveDelta`, mirroring
    * how a replica processes events and delta payloads received from its peers. A fresh replica is required per
    * invocation since `Replica` is stateful: replaying the same trace into an already-populated replica would
    * make every subsequent invocation a cheap no-op.
    */
  @Benchmark
  def receiveEventsAndDeltas(state: ReceiveBenchmarkState): Set[Hash] = {
    val replica = new Replica[TravelPlan](
      state.genesisHash,
      state.rootIdentity,
      r => new NoOpAntiEntropy(r),
      _ => ()
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
