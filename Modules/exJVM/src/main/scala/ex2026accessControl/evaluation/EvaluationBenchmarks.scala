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

    val replicaIndex = Random(42).nextInt(numReplicas)
    selectedIdentity = generated.replicaIds(replicaIndex)
    selectedMutatorChoice =
      BenchmarkHelper.randomMutatorChoice(generated.permittedMutators(replicaIndex))(using Random(42))
    authorizationHash = generated.capabilityEvent(selectedIdentity.getPublic)
  }
}

/** Holds a [[HashDag]] of [[SignedHashDagEntry]]s translated (via [[TraceGeneration.translateToSignedHashDag]])
  * from the exact same [[ArdtEventGraph]] trace that [[BenchmarkRdtBenchmarkState]] builds (built with the same
  * `numReplicas`/`concurrencyProbability`/`seed`, which deterministically reproduces the same graph rather than
  * sharing state across the two), instead of generating an independent random graph — the counterpart of
  * [[BenchmarkRdtBenchmarkState]] for the ACL-free [[HashDag]] representation (no capabilities, no access
  * control enforcement) used as a baseline to compare against it. See [[BenchmarkRdtBenchmarkState]] for what
  * `trace`/`rdtState`/`selectedIdentity`/`selectedMutatorChoice` are for.
  */
@State(Scope.Benchmark)
class SignedHashDagBenchmarkRdtBenchmarkState {

  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int               = 10
  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L

  var hashDag: HashDag[SignedHashDagEntry] = scala.compiletime.uninitialized
  var trace: Array[Array[Byte]]            = scala.compiletime.uninitialized
  var rdtState: BenchmarkRdt               = scala.compiletime.uninitialized

  var selectedIdentity: PrivateIdentity                = scala.compiletime.uninitialized
  var selectedMutatorChoice: BenchmarkRdtMutatorChoice = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    val generated         = TraceGeneration.generateBenchmarkRdtEventGraph(numReplicas, numEvents, concurrencyProbability)
    val translated         = TraceGeneration.translateToSignedHashDag(generated)

    hashDag = translated.hashDag
    trace = translated.trace
    rdtState = translated.state

    val replicaIndex = random.nextInt(numReplicas)
    selectedIdentity = generated.replicaIds(replicaIndex)
    selectedMutatorChoice = BenchmarkHelper.randomMutatorChoice(generated.permittedMutators(replicaIndex))
  }
}

/** [[SignedHashDagBenchmarkRdtBenchmarkState]], using [[UnsignedHashDagEntry]] instead, i.e. without the
  * signing/verification overhead paid by every [[SignedHashDagEntry]].
  */
@State(Scope.Benchmark)
class UnsignedHashDagBenchmarkRdtBenchmarkState {

  @Param(Array("20000", "40000", "60000", "80000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val numReplicas: Int               = 10
  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L

  var hashDag: HashDag[UnsignedHashDagEntry] = scala.compiletime.uninitialized
  var trace: Array[Array[Byte]]              = scala.compiletime.uninitialized
  var rdtState: BenchmarkRdt                 = scala.compiletime.uninitialized

  var selectedIdentity: PrivateIdentity                = scala.compiletime.uninitialized
  var selectedMutatorChoice: BenchmarkRdtMutatorChoice = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    val generated         = TraceGeneration.generateBenchmarkRdtEventGraph(numReplicas, numEvents, concurrencyProbability)
    val translated         = TraceGeneration.translateToUnsignedHashDag(generated)

    hashDag = translated.hashDag
    trace = translated.trace
    rdtState = translated.state

    val replicaIndex = random.nextInt(numReplicas)
    selectedIdentity = generated.replicaIds(replicaIndex)
    selectedMutatorChoice = BenchmarkHelper.randomMutatorChoice(generated.permittedMutators(replicaIndex))
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

  /** [[createEvents]], authoring the new event as a [[SignedHashDagEntry]] on top of a [[HashDag]] instead. */
  @Benchmark
  def createEventsSignedHashDag(state: SignedHashDagBenchmarkRdtBenchmarkState): HashDag[SignedHashDagEntry] = {
    given random: Random = Random(state.seed)
    given LocalUid       = LocalUid(Uid(state.selectedIdentity.getPublic.id))

    val delta = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.hashDag.heads
    var dag     = state.hashDag

    delta.decomposed.foreach { decomposedDelta =>
      val entry = HashDagEntry.createSignedEntry(decomposedDelta, state.selectedIdentity, parents)
      dag = HashDag.receiveOrThrow(dag, writeToArray(entry))
    }

    dag
  }

  /** [[createEventsSignedHashDag]], authoring an [[UnsignedHashDagEntry]] instead. */
  @Benchmark
  def createEventsUnsignedHashDag(state: UnsignedHashDagBenchmarkRdtBenchmarkState): HashDag[UnsignedHashDagEntry] = {
    given random: Random = Random(state.seed)
    given LocalUid       = LocalUid(Uid(state.selectedIdentity.getPublic.id))

    val delta = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.hashDag.heads
    var dag     = state.hashDag

    delta.decomposed.foreach { decomposedDelta =>
      val entry = HashDagEntry.createUnsignedEntry(decomposedDelta, state.selectedIdentity, parents)
      dag = HashDag.receiveOrThrow(dag, writeToArray(entry))
    }

    dag
  }

  /** Full state materialization from a pre-built [[HashDag]] of [[SignedHashDagEntry]]s, without any access
    * control enforcement (unlike [[materializeWithAuthorization]]) — only decoding and merging every entry's
    * payload.
    */
  @Benchmark
  def materializeSignedHashDag(state: SignedHashDagBenchmarkRdtBenchmarkState): BenchmarkRdt =
    HashDag.materialize[BenchmarkRdt](state.hashDag)

  /** [[materializeSignedHashDag]], for a [[HashDag]] of [[UnsignedHashDagEntry]]s instead. */
  @Benchmark
  def materializeUnsignedHashDag(state: UnsignedHashDagBenchmarkRdtBenchmarkState): BenchmarkRdt =
    HashDag.materialize[BenchmarkRdt](state.hashDag)

  /** [[receiveEventsAndDeltas]], replaying the pre-encoded trace of [[SignedHashDagEntry]]s into a fresh
    * [[HashDag]] instead of into a [[Replica]]; since a [[HashDag]] entry carries its payload directly, there is
    * no separate delta to receive.
    */
  @Benchmark
  def receiveEventsSignedHashDag(state: SignedHashDagBenchmarkRdtBenchmarkState): Set[Hash] = {
    var dag = HashDag[SignedHashDagEntry](state.hashDag.genesis, Set(state.hashDag.genesis), Map.empty)
    state.trace.foreach { encodedEntry => dag = HashDag.receiveOrThrow(dag, encodedEntry) }
    dag.heads
  }

  /** [[receiveEventsSignedHashDag]], for a trace of [[UnsignedHashDagEntry]]s instead. */
  @Benchmark
  def receiveEventsUnsignedHashDag(state: UnsignedHashDagBenchmarkRdtBenchmarkState): Set[Hash] = {
    var dag = HashDag[UnsignedHashDagEntry](state.hashDag.genesis, Set(state.hashDag.genesis), Map.empty)
    state.trace.foreach { encodedEntry => dag = HashDag.receiveOrThrow(dag, encodedEntry) }
    dag.heads
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
