package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.Hash
import crypto.channels.PrivateIdentity
import ex2026accessControl.evaluation.BenchmarkHelper.BenchmarkRdtMutatorChoice
import ex2026accessControl.evaluation.EvaluationBenchmarks.noopOnStateChange
import org.openjdk.jmh.annotations.*
import rdts.base.{LocalUid, Uid}
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore, Replica}

import java.util.concurrent.TimeUnit
import scala.util.Random

/** Holds a randomly generated [[ArdtEventGraph]] of BenchmarkRdt edits, built once per JMH trial (i.e. before
  * warmup/measurement iterations start, so its construction is never included in the measured time). Besides
  * [[deltaValueStore]] and the pre-encoded [[trace]] needed to feed the graph into a [[Replica]] via
  * `receiveEvent`/`receiveDelta`, this also holds everything [[EvaluationBenchmarks.createEvents]] needs to
  * author one further, realistic event on top of the graph: [[rdtState]] (the fully merged value resulting
  * from every event in the graph, to compute the new event's delta from), and a single preselected
  * [[selectedIdentity]]/[[selectedMutatorChoice]]/[[authorizationHash]] combination (rather than picking one
  * at random on every invocation), so that every invocation authors the exact same event. The graph is
  * generated deterministically from [[seed]], so every fork/trial with the same `@Param` values operates on
  * the exact same graph and picks the exact same combination.
  */
@State(Scope.Benchmark)
class BenchmarkRdtBenchmarkState {

  // The total number of BenchmarkRdt edits making up the pre-built event graph, distributed among replicas at
  // random. This controls the size of the graph that createEvents authors one further event on top of, as
  // well as the size read by the other benchmarks below.
  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int               = 10
  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L

  var eventGraph: ArdtEventGraph[BenchmarkRdt]       = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[BenchmarkRdt] = scala.compiletime.uninitialized
  var genesisHash: Hash                              = scala.compiletime.uninitialized
  var rootIdentity: PrivateIdentity                  = scala.compiletime.uninitialized
  var trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    scala.compiletime.uninitialized

  var rdtState: BenchmarkRdt = scala.compiletime.uninitialized

  // The single replica and mutation createEvents authors its one new event as, preselected once per trial
  // rather than picked at random on every invocation.
  var selectedIdentity: PrivateIdentity                = scala.compiletime.uninitialized
  var selectedMutatorChoice: BenchmarkRdtMutatorChoice = scala.compiletime.uninitialized
  var authorizationHash: Hash                          = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    val generated        = TraceGeneration.generateBenchmarkRdtEventGraph(
      numReplicas,
      numEvents,
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

    rdtState = generated.state

    val replicaIndex = random.nextInt(numReplicas)
    selectedIdentity = generated.replicaIds(replicaIndex)
    selectedMutatorChoice = BenchmarkHelper.randomMutatorChoice(generated.permittedMutators(replicaIndex))
    authorizationHash = generated.capabilityEvent(selectedIdentity.getPublic)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class EvaluationBenchmarks {

  @Benchmark
  def createEvents(state: BenchmarkRdtBenchmarkState): ArdtEventGraph[BenchmarkRdt] = {
    given random: Random = Random(state.seed)
    given LocalUid       = LocalUid(Uid(state.selectedIdentity.getPublic.id))

    val delta = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.eventGraph.heads
    var graph   = state.eventGraph

    delta.decomposed.foreach { decomposedDelta =>
      val (event, revealed) =
        EventGraphBuilder.buildDeltaEvent(decomposedDelta, state.selectedIdentity, parents, state.authorizationHash)
      graph = EventGraphBuilder.receiveOrThrow(graph, event)
    }

    graph
  }

  /** Full state materialization, including access control enforcement (capability/write-permission filtering and
    * revocation/causality checks), as used in production.
    */
  @Benchmark
  def materializeWithAuthorization(state: BenchmarkRdtBenchmarkState): BenchmarkRdt =
    Authorization.materialize(state.eventGraph, state.deltaValueStore)

  @Benchmark
  def receiveEventsAndDeltas(state: BenchmarkRdtBenchmarkState): Set[Hash] = {
    val replica = new Replica[BenchmarkRdt](
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

object EvaluationBenchmarks {
  def noopOnStateChange[T](x: => T): Unit = ()
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    val state = new BenchmarkRdtBenchmarkState()
    state.numEvents = 100_000
    state.setup()
    val bench = new EvaluationBenchmarks()
    println("Done with setup")

    {
      val timeStart = System.nanoTime()
      bench.createEvents(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
    }

    {
      val timeStart = System.nanoTime()
      val result    = bench.materializeWithAuthorization(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
    }

    val timeStart = System.nanoTime()
    bench.receiveEventsAndDeltas(state)
    println((System.nanoTime() - timeStart) / 1_000_000_000.0)
  }
}
