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

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Thread)
class EvaluationBenchmarks {

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createSingleEvent(state: ArdtEventGraphBenchmarkState): ArdtEventGraph[BenchmarkRdt] = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid
    val delta            = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.eventGraph.heads
    var graph   = state.eventGraph

    delta.decomposed.foreach { decomposedDelta =>
      val (event, revealed) =
        EventGraphBuilder.buildDeltaEvent(decomposedDelta, state.selectedIdentity, parents, state.authorizationHash)
      graph = EventGraphBuilder.receiveOrThrow(graph, event)
    }

    graph
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createSingleEventSignedHashDag(state: SignedHashDagBenchmarkState): HashDag[SignedHashDagEntry] = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid

    val delta = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.hashDag.heads
    var dag     = state.hashDag

    val entry = HashDagEntry.createSignedEntry(delta, state.selectedIdentity, parents)
    dag = HashDag.receiveOrThrow(dag, writeToArray(entry))

    dag
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createSingleEventUnsignedHashDag(
      state: UnsignedHashDagBenchmarkState
  ): HashDag[UnsignedHashDagEntry] = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid

    val delta = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)

    val parents = state.hashDag.heads
    var dag     = state.hashDag

    val entry = HashDagEntry.createUnsignedEntry(delta, state.selectedIdentity, parents)
    dag = HashDag.receiveOrThrow(dag, writeToArray(entry))

    dag
  }

  @Benchmark
  def receiveEventsAndDeltas(state: ArdtEventGraphBenchmarkState): Set[Hash] = {
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

  @Benchmark
  def receiveEventsSignedHashDag(state: SignedHashDagBenchmarkState): Set[Hash] = {
    var dag = HashDag[SignedHashDagEntry](state.hashDag.genesis, Set(state.hashDag.genesis), Map.empty)
    state.hashDagTrace.foreach { encodedEntry => dag = HashDag.receiveOrThrow(dag, encodedEntry) }
    dag.heads
  }

  @Benchmark
  def receiveEventsUnsignedHashDag(state: UnsignedHashDagBenchmarkState): Set[Hash] = {
    var dag = HashDag[UnsignedHashDagEntry](state.hashDag.genesis, Set(state.hashDag.genesis), Map.empty)
    state.hashDagTrace.foreach { encodedEntry => dag = HashDag.receiveOrThrow(dag, encodedEntry) }
    dag.heads
  }

  /** Full state materialization, including access control enforcement */
  @Benchmark
  def materializeWithAuthorization(state: ArdtEventGraphBenchmarkState): BenchmarkRdt =
    Authorization.materialize(state.eventGraph, state.deltaValueStore)

  /** Full state materialization from a pre-built [[HashDag]] of [[SignedHashDagEntry]]s, without any access
    * control enforcement.
    */
  @Benchmark
  def materializeSignedHashDag(state: SignedHashDagBenchmarkState): BenchmarkRdt =
    HashDag.materialize[BenchmarkRdt](state.hashDag)

  @Benchmark
  def materializeUnsignedHashDag(state: UnsignedHashDagBenchmarkState): BenchmarkRdt =
    HashDag.materialize[BenchmarkRdt](state.hashDag)
}

@State(Scope.Benchmark)
class ArdtEventGraphBenchmarkState {

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
  var selectedLocalUid: LocalUid                       = scala.compiletime.uninitialized
  var authorizationHash: Hash                          = scala.compiletime.uninitialized

  // Exposed so that state classes extending this one (e.g. those translating the generated graph into a
  // HashDag) can reuse it in their own @Setup, instead of generating an independent random graph.
  protected var generated: GeneratedBenchmarkRdtEventGraph = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    generated = TraceGeneration.generateBenchmarkRdtEventGraph(
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
    selectedLocalUid = LocalUid(Uid(selectedIdentity.getPublic.id))
    selectedMutatorChoice =
      BenchmarkHelper.randomMutatorChoice(generated.permittedMutators(replicaIndex))(using Random(42))
    authorizationHash = generated.capabilityEvent(selectedIdentity.getPublic)
  }
}

@State(Scope.Benchmark)
class SignedHashDagBenchmarkState extends ArdtEventGraphBenchmarkState {

  var hashDag: HashDag[SignedHashDagEntry] = scala.compiletime.uninitialized
  var hashDagTrace: Array[Array[Byte]]     = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    val translated = TraceGeneration.translateToSignedHashDag(generated)

    hashDag = translated.hashDag
    hashDagTrace = translated.trace
  }
}

/** [[SignedHashDagBenchmarkState]], using [[UnsignedHashDagEntry]] instead, i.e. without the
  * signing/verification overhead paid by every [[SignedHashDagEntry]].
  */
@State(Scope.Benchmark)
class UnsignedHashDagBenchmarkState extends ArdtEventGraphBenchmarkState {

  var hashDag: HashDag[UnsignedHashDagEntry] = scala.compiletime.uninitialized
  var hashDagTrace: Array[Array[Byte]]       = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    val translated = TraceGeneration.translateToUnsignedHashDag(generated)

    hashDag = translated.hashDag
    hashDagTrace = translated.trace
  }
}

object EvaluationBenchmarks {
  def noopOnStateChange[T](x: => T): Unit = ()
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    {
      val state = new ArdtEventGraphBenchmarkState()
      state.numEvents = 100_000
      state.setup()
      val bench = new EvaluationBenchmarks()
      println("Done with setup")
      val timeStart = System.nanoTime()
      bench.receiveEventsAndDeltas(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
    }

    {
      val state = new SignedHashDagBenchmarkState()
      state.numEvents = 100_000
      state.setup()
      val bench = new EvaluationBenchmarks()
      println("Done with setup")
      val timeStart = System.nanoTime()
      bench.receiveEventsSignedHashDag(state)
      println((System.nanoTime() - timeStart) / 1_000_000_000.0)
    }
  }
}
