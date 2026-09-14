package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.{Hash, PublicIdentity}
import crypto.channels.PrivateIdentity
import ex2026accessControl.evaluation.EvaluationBenchmark.noopOnStateChange
import org.openjdk.jmh.annotations.*
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore, Replica}

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.util.Random

/** Holds a randomly generated [[ArdtEventGraph]] of BenchmarkRdt edits, built once per JMH trial (i.e. before
  * warmup/measurement iterations start, so its construction is never included in the measured time), together
  * with the pre-encoded events and delta-commitment classification needed to feed them into a [[Replica]] via
  * `receiveEvent`/`receiveDelta`. The graph is generated deterministically from [[seed]], so every fork/trial
  * with the same `@Param` values operates on the exact same trace.
  */
@State(Scope.Benchmark)
class BenchmarkRdtTraceBenchmarkState {

  // The total number of BenchmarkRdt edits performed, distributed among replicas at random. This controls the
  // size of the generated event graph.
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
  }
}

/** Holds a [[BenchmarkRdtTracePlan]] (every random decision of a BenchmarkRdt edit trace, made once per JMH
  * trial) together with the fixed genesis/capability-delegation prefix needed to replay it, as done by
  * [[EvaluationBenchmark.createEvents]]. The counterpart to [[BenchmarkRdtTraceBenchmarkState]], which instead
  * pre-builds the whole trace (not just the plan for it) so that the other benchmarks can measure reading it.
  *
  * [[rdtState]], [[eventGraph]], [[deltaValueStore]] and [[eventIndex]] are reset to a fresh, empty replay
  * before every invocation (`@Setup(Level.Invocation)`), so that `createEvents` can freely mutate them in
  * place while replaying [[plan]] without that mutation leaking into the next invocation.
  */
@State(Scope.Benchmark)
class BenchmarkRdtCreationState {

  // The total number of BenchmarkRdt edits to plan (and later replay) per invocation, distributed among
  // replicas at random.
  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int               = 10
  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L

  var plan: BenchmarkRdtTracePlan = scala.compiletime.uninitialized

  // The fixed genesis + capability-delegation prefix, built once and reused unmodified as the starting point
  // of every invocation's replay, since it holds no randomized "decisions" of its own.
  private var initialEventGraph: ArdtEventGraph[BenchmarkRdt] = scala.compiletime.uninitialized
  private var initialEventIndex: Map[Int, Hash]               = scala.compiletime.uninitialized
  var capabilityEvent: Map[PublicIdentity, Hash]              = scala.compiletime.uninitialized

  // Replay state, updated in place by createEvents and reset before every invocation.
  var rdtState: BenchmarkRdt                         = scala.compiletime.uninitialized
  var eventGraph: ArdtEventGraph[BenchmarkRdt]        = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[BenchmarkRdt]  = scala.compiletime.uninitialized
  var eventIndex: mutable.Map[Int, Hash]              = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    plan = TraceGeneration.planBenchmarkRdtTrace(numReplicas, numEvents, concurrencyProbability)

    val rootIdentity = plan.replicaIds(0)
    val genesisEvent = Authorization.createGenesis(rootIdentity)
    var graph        = ArdtEventGraph[BenchmarkRdt](genesisEvent)
    val capability   = mutable.Map(rootIdentity.getPublic -> genesisEvent.hash)
    val index        = mutable.Map(0 -> genesisEvent.hash)
    plan.replicaIds.drop(1).zipWithIndex.foreach { case (identity, i) =>
      val delegation = EventGraphBuilder.buildCapabilityEvent(
        holder = identity.getPublic,
        read = PermissionTree.allow,
        write = plan.writePermissions(identity.getPublic),
        author = rootIdentity,
        parents = graph.heads,
        authorization = genesisEvent.hash
      )
      graph = EventGraphBuilder.receiveOrThrow(graph, delegation)
      capability(identity.getPublic) = delegation.hash
      index(i + 1) = delegation.hash
    }

    initialEventGraph = graph
    initialEventIndex = index.toMap
    capabilityEvent = capability.toMap
  }

  @Setup(Level.Invocation)
  def resetReplayState(): Unit = {
    rdtState = BenchmarkRdt.empty
    eventGraph = initialEventGraph
    deltaValueStore = DeltaValueStore[BenchmarkRdt]()
    eventIndex = mutable.Map.from(initialEventIndex)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class EvaluationBenchmark {

  /** Replays a pre-planned [[BenchmarkRdtTracePlan]] (see [[BenchmarkRdtCreationState]]): none of its random
    * decisions (which replica authors a mutation, which field it touches, which earlier events it is
    * concurrent with) are made here, since they were already resolved when the plan was built. What remains,
    * and what this measures, is purely the mechanical cost of authoring events: merging each mutation's delta
    * into the running [[BenchmarkRdt]] state, decomposing/signing/committing it, and inserting the resulting
    * event(s) into a running [[ArdtEventGraph]] and [[DeltaValueStore]]. The counterpart to
    * [[materializeWithAuthorization]]/[[materializeWithoutAuthorization]], which instead measure reading an
    * already-built trace of the same size.
    */
  @Benchmark
  def createEvents(state: BenchmarkRdtCreationState): ArdtEventGraph[BenchmarkRdt] = {
    given random: Random = Random(state.seed)

    state.plan.mutationSteps.foreach { step =>
      val identity = state.plan.replicaIds(step.authorIndex)
      val author   = identity.getPublic

      given LocalUid = LocalUid(Uid(author.id))
      val delta = BenchmarkHelper.applyBenchmarkRdtMutator(step.mutatorChoice, state.rdtState)
      state.rdtState = state.rdtState.merge(delta)

      val parents = step.parentIndices.map(state.eventIndex)

      delta.decomposed.foreach { decomposedDelta =>
        val (event, revealed) =
          EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, state.capabilityEvent(author))
        state.eventGraph = EventGraphBuilder.receiveOrThrow(state.eventGraph, event)
        state.eventIndex(state.eventIndex.size) = event.hash
        state.deltaValueStore.put(revealed)
      }
    }

    state.eventGraph
  }

  /** Full state materialization, including access control enforcement (capability/write-permission filtering and
    * revocation/causality checks), as used in production.
    */
  @Benchmark
  def materializeWithAuthorization(state: BenchmarkRdtTraceBenchmarkState): BenchmarkRdt =
    Authorization.materialize(state.eventGraph, state.deltaValueStore)

  /** Materializes the very same trace by merging every delta value in causal-order-independent fashion, without
    * any access control checks. The difference to [[materializeWithAuthorization]] is the overhead added by
    * access control enforcement.
    */
  @Benchmark
  def materializeWithoutAuthorization(state: BenchmarkRdtTraceBenchmarkState): BenchmarkRdt =
    UnauthorizedMaterialize.materialize(state.eventGraph, state.deltaValueStore)

  /** Ingests the entire trace into a freshly constructed [[Replica]] via `receiveEvent`/`receiveDelta`, mirroring
    * how a replica processes events and delta payloads received from its peers. A fresh replica is required per
    * invocation since `Replica` is stateful: replaying the same trace into an already-populated replica would
    * make every subsequent invocation a cheap no-op. Overrides the class-level warmup/measurement durations
    * since a single invocation is far cheaper than one round of materialization.
    */
  @Benchmark
  def receiveEventsAndDeltas(state: BenchmarkRdtTraceBenchmarkState): Set[Hash] = {
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

object EvaluationBenchmark {
  def noopOnStateChange[T](x: => T): Unit = ()
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    val state = new BenchmarkRdtTraceBenchmarkState()
    state.numEvents = 100_000
    state.setup()
    val bench = new EvaluationBenchmark()
    println("Done with setup")

    val creationState = new BenchmarkRdtCreationState()
    creationState.numEvents = 100_000
    creationState.setup()
    creationState.resetReplayState()
    {
      val timeStart = System.nanoTime()
      bench.createEvents(creationState)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
    }

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
