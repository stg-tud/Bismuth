package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import ex2026accessControl.evaluation.BenchmarkHelper.BenchmarkRdtMutatorChoice
import ex2026accessControl.evaluation.EvaluationBenchmarks.noopOnStateChange
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{LocalUid, Uid}
import replication.authz.*
import replication.authz.ArdtEvent.Payload.DeltaCommitment

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
  def createSingleEvent(state: ArdtEventGraphBenchmarkState, blackhole: Blackhole): Unit = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid

    val delta            = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)
    val parents          = state.eventGraph.heads

    delta.decomposed.foreach { decomposedDelta =>
      blackhole.consume(
        EventGraphBuilder.buildDeltaEvent(decomposedDelta, state.selectedIdentity, parents, state.authorizationHash)
      )
    }
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createSingleEventSignedHashDag(state: SignedHashDagBenchmarkState, blackhole: Blackhole): Unit = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid

    val delta   = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)
    val parents = state.hashDag.heads

    blackhole.consume(
      HashDagEntry.createSignedEntry(delta, state.selectedIdentity, parents)
    )
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createSingleEventUnsignedHashDag(state: UnsignedHashDagBenchmarkState, blackhole: Blackhole): Unit = {
    given random: Random = Random(state.seed)
    given LocalUid       = state.selectedLocalUid

    val delta   = BenchmarkHelper.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)
    val parents = state.hashDag.heads

    blackhole.consume(
      HashDagEntry.createUnsignedEntry(delta, state.selectedIdentity, parents)
    )
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
    var rdtState = BenchmarkRdt.empty
    var dag      = HashDag[SignedHashDagEntry](state.hashDag.genesis, Set.empty, Map.empty)
    state.hashDagTrace.foreach { encodedEntry =>
      val oldHashDag = dag
      dag = HashDag.receiveOrThrow(dag, encodedEntry)
      val newEntry = dag.heads.diff(oldHashDag.heads).head
      val delta    = readFromArray[BenchmarkRdt](dag.events(newEntry).payload)
      rdtState = rdtState.merge(delta)
    }
    dag.heads
  }

  @Benchmark
  def receiveEventsUnsignedHashDag(state: UnsignedHashDagBenchmarkState): Set[Hash] = {
    var rdtState = BenchmarkRdt.empty
    var dag      = HashDag[UnsignedHashDagEntry](state.hashDag.genesis, Set.empty, Map.empty)
    state.hashDagTrace.foreach { encodedEntry =>
      val oldHashDag = dag
      dag = HashDag.receiveOrThrow(dag, encodedEntry)
      val newEntry = dag.heads.diff(oldHashDag.heads).head
      val delta    = readFromArray[BenchmarkRdt](dag.events(newEntry).payload)
      rdtState = rdtState.merge(delta)
    }
    dag.heads
  }

  /** Sending side of anti-entropy, including access control enforcement: every requested event is looked up
    * and encoded, and every delta value belonging to one of them is filtered against the destination's read
    * permissions before being encoded and handed to the [[SummingConnectionManager]].
    */
  @Benchmark
  def sendEventsWithDelta(state: SendEventsWithDeltaBenchmarkState): Long = {
    state.connectionManager.reset()
    state.antiEntropy.sendEventsWithDelta(state.destination, state.eventHashes)
    state.connectionManager.sentBytes
  }

  /** [[sendEventsWithDelta]] without any access control: every requested entry, payload included, is looked up,
    * encoded and handed to the [[SummingConnectionManager]] unfiltered.
    */
  @Benchmark
  def sendEntriesSignedHashDag(state: SendEntriesSignedHashDagBenchmarkState): Long = {
    state.connectionManager.reset()
    HashDag.sendEntries(state.hashDag, state.connectionManager, state.destination, state.entryHashes)
    state.connectionManager.sentBytes
  }

  @Benchmark
  def sendEntriesUnsignedHashDag(state: SendEntriesUnsignedHashDagBenchmarkState): Long = {
    state.connectionManager.reset()
    HashDag.sendEntries(state.hashDag, state.connectionManager, state.destination, state.entryHashes)
    state.connectionManager.sentBytes
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
  @Param(Array("10000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  @Param(Array("10", "100"))
  var numReplicas: Int               = scala.compiletime.uninitialized
  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L
  val numCapabilitiesPerReplica: Int = 4

  var eventGraph: ArdtEventGraph[BenchmarkRdt]       = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[BenchmarkRdt] = scala.compiletime.uninitialized
  var genesisHash: Hash                              = scala.compiletime.uninitialized
  var rootIdentity: PrivateIdentity                  = scala.compiletime.uninitialized
  var trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    scala.compiletime.uninitialized

  var rdtState: BenchmarkRdt = scala.compiletime.uninitialized

  // The single (non-root) replica and mutation used by createEvents
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
      concurrencyProbability,
      numCapabilitiesPerReplica
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

    selectedIdentity = generated.replicaIds(1 + Random(42).nextInt(numReplicas - 1))
    selectedLocalUid = LocalUid(Uid(selectedIdentity.getPublic.id))
    selectedMutatorChoice =
      BenchmarkHelper.randomMutatorChoice(BenchmarkRdtMutatorChoice.values)(using Random(42))
    authorizationHash = generated.capabilityEvent(selectedIdentity.getPublic)(selectedMutatorChoice)
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

@State(Scope.Benchmark)
class SendEventsWithDeltaBenchmarkState extends ArdtEventGraphBenchmarkState {

  var connectionManager: SummingConnectionManager = scala.compiletime.uninitialized
  var antiEntropy: AntiEntropy                    = scala.compiletime.uninitialized
  var destination: PublicIdentity                 = scala.compiletime.uninitialized
  var eventHashes: Array[Hash]                    = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()

    val replica = new Replica[BenchmarkRdt](
      genesisHash,
      rootIdentity,
      r => NoOpAntiEntropy(r),
      noopOnStateChange
    )
    trace.foreach { (hash, encodedEvent, deltaCommitment) =>
      replica.receiveEvent(encodedEvent)
      deltaCommitment.foreach { commitment =>
        deltaValueStore.get(commitment).foreach(revealed => replica.receiveDelta(hash, revealed))
      }
    }

    destination = generated.replicaIds(1).getPublic
    connectionManager = SummingConnectionManager(Set(destination))
    // The control plane is never consulted by sendEventsWithDelta, and is thus left unimplemented.
    antiEntropy = AntiEntropy(replica, _ => connectionManager, _ => ???)
    eventHashes = trace.map(_.hash)
  }
}

/** [[SignedHashDagBenchmarkState]] holding, in addition, what the access control free counterpart of
  * [[AntiEntropy.sendEventsWithDelta]] needs: the hashes of every translated entry, in the same (causal) order
  * as the trace the [[ArdtEventGraph]] based benchmark sends, and a [[SummingConnectionManager]] to send them to.
  */
@State(Scope.Benchmark)
class SendEntriesSignedHashDagBenchmarkState extends SignedHashDagBenchmarkState {

  var connectionManager: SummingConnectionManager = scala.compiletime.uninitialized
  var destination: PublicIdentity                 = scala.compiletime.uninitialized
  var entryHashes: Array[Hash]                    = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    destination = rootIdentity.getPublic
    connectionManager = SummingConnectionManager(Set(destination))
    entryHashes = hashDagTrace.map(Hash.compute)
  }
}

/** [[SendEntriesSignedHashDagBenchmarkState]], using [[UnsignedHashDagEntry]] instead. */
@State(Scope.Benchmark)
class SendEntriesUnsignedHashDagBenchmarkState extends UnsignedHashDagBenchmarkState {

  var connectionManager: SummingConnectionManager = scala.compiletime.uninitialized
  var destination: PublicIdentity                 = scala.compiletime.uninitialized
  var entryHashes: Array[Hash]                    = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    destination = rootIdentity.getPublic
    connectionManager = SummingConnectionManager(Set(destination))
    entryHashes = hashDagTrace.map(Hash.compute)
  }
}

object EvaluationBenchmarks {
  def noopOnStateChange[T](x: => T): Unit = ()
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    val state = new SendEventsWithDeltaBenchmarkState()
    state.numEvents = 100_000
    state.numReplicas = 10
    state.setup()
    val bench = new EvaluationBenchmarks()
    println("Done with setup")
    val timeStart = System.nanoTime()
    bench.sendEventsWithDelta(state)
    println((System.nanoTime() - timeStart) / 1_000_000_000.0)
  }
}
