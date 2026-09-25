package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.channels.PrivateIdentity
import crypto.{Commitment, Hash, PublicIdentity}
import ex2026accessControl.evaluation.EvaluationBenchmarks.{encodeTrace, noopOnStateChange, receiveTrace, replayTrace}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{LocalUid, Uid}
import replication.JsoniterCodecsJvm.ardtEventCodec
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
  def createUpdate(state: CreateUpdateBenchmarkState): Unit =
    state.replica.mutateState(
      BenchmarkRdt.applyBenchmarkRdtMutator(state.selectedMutatorChoice, _)(using state.selectedLocalUid),
      state.authorizationHash
    )

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createUpdateSignedHashDag(state: SignedHashDagBenchmarkState, blackhole: Blackhole): Unit = {
    given LocalUid = state.selectedLocalUid

    val delta   = BenchmarkRdt.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)
    val parents = state.hashDag.heads

    blackhole.consume(
      HashDagEntry.createSignedEntry(delta, state.selectedIdentity, parents)
    )
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def createUpdateUnsignedHashDag(state: UnsignedHashDagBenchmarkState, blackhole: Blackhole): Unit = {
    given LocalUid = state.selectedLocalUid

    val delta   = BenchmarkRdt.applyBenchmarkRdtMutator(state.selectedMutatorChoice, state.rdtState)
    val parents = state.hashDag.heads

    blackhole.consume(
      HashDagEntry.createUnsignedEntry(delta, state.selectedIdentity, parents)
    )
  }

  @Benchmark
  def receiveEventsAndDeltas(state: ArdtEventGraphBenchmarkState): Set[Hash] = receiveTrace(state)

  /** Receiving only a revocation, into a replica that has already received every other event of the trace */
  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def receiveRevocation(state: RevocationBenchmarkState): Either[Set[Hash], Option[Hash]] =
    state.replica.receiveEvent(state.encodedRevocation)

  @Benchmark
  def receiveEventsSignedHashDag(state: SignedHashDagBenchmarkState): Set[Hash] = {
    val replica = new HashDagReplica[SignedHashDagEntry[BenchmarkRdt], BenchmarkRdt](state.hashDag.genesis, ???)

    state.hashDagTrace.foreach { encodedEntry => replica.receiveEntry(encodedEntry) }

    replica.heads
  }

  @Benchmark
  def receiveEventsUnsignedHashDag(state: UnsignedHashDagBenchmarkState): Set[Hash] = {
    val replica = new HashDagReplica[UnsignedHashDagEntry[BenchmarkRdt], BenchmarkRdt](state.hashDag.genesis, ???)

    state.hashDagTrace.foreach { encodedEntry => replica.receiveEntry(encodedEntry) }

    replica.heads
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
  def sendEntriesSignedHashDag(state: SignedHashDagBenchmarkStateWithReplica): Long = {
    state.connectionManager.reset()
    state.replica.sendEntries(state.destination, state.entryHashes)
    state.connectionManager.sentBytes
  }

  @Benchmark
  def sendEntriesUnsignedHashDag(state: UnsignedHashDagBenchmarkStateWithReplica): Long = {
    state.connectionManager.reset()
    state.replica.sendEntries(state.destination, state.entryHashes)
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
    HashDag.materialize(state.hashDag)

  @Benchmark
  def materializeUnsignedHashDag(state: UnsignedHashDagBenchmarkState): BenchmarkRdt =
    HashDag.materialize(state.hashDag)
}

/** Holds an [[ArdtEventGraph]] built by [[TraceGeneration.generateDelegationHierarchyEventGraph]]: a fixed set of
  * 13 replicas in a two-level delegation hierarchy, writing in two phases.
  */
@State(Scope.Benchmark)
class ArdtEventGraphBenchmarkState {

  // The total number of BenchmarkRdt edits making up the pre-built event graph, half of them before and half of
  // them after the second-level delegations. This controls the size of the graph that createSingleEvent authors
  // one further event on top of, as well as the size read by the other benchmarks.
  @Param(Array("10000", "100000"))
  var numEvents: Int = scala.compiletime.uninitialized

  val concurrencyProbability: Double = 0.2
  val seed: Long                     = 42L

  var eventGraph: ArdtEventGraph[BenchmarkRdt]       = scala.compiletime.uninitialized
  var deltaValueStore: DeltaValueStore[BenchmarkRdt] = scala.compiletime.uninitialized
  var genesisHash: Hash                              = scala.compiletime.uninitialized
  var rootIdentity: PrivateIdentity                  = scala.compiletime.uninitialized
  var trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    scala.compiletime.uninitialized

  var rdtState: BenchmarkRdt = scala.compiletime.uninitialized

  // The single (non-root) replica and mutation used by createUpdate
  var selectedIdentity: PrivateIdentity = scala.compiletime.uninitialized
  var selectedMutatorChoice: String     = scala.compiletime.uninitialized
  var selectedLocalUid: LocalUid        = scala.compiletime.uninitialized
  var authorizationHash: Hash           = scala.compiletime.uninitialized

  // Exposed so that state classes extending this one (e.g. those translating the generated graph into a
  // HashDag) can reuse it in their own @Setup, instead of generating an independent random graph.
  protected var generated: GeneratedBenchmarkRdtEventGraph = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    given random: Random = Random(seed)
    generated = TraceGeneration.generateDelegationHierarchyEventGraph(
      numEventsPhase1 = numEvents / 2,
      numEventsPhase2 = numEvents - numEvents / 2,
      concurrencyProbability
    )
    useGenerated(generated)

    val selectionRandom = Random(seed)
    selectedIdentity = generated.replicaIds(1 + selectionRandom.nextInt(generated.replicaIds.length - 1))
    selectedLocalUid = LocalUid(Uid(selectedIdentity.getPublic.id))
    // A non-root replica may only write to its own subtree
    val permittedMutations = generated.capabilityEvent(selectedIdentity.getPublic).keys.toIndexedSeq.sorted
    selectedMutatorChoice = permittedMutations(selectionRandom.nextInt(permittedMutations.size))
    authorizationHash = generated.capabilityEvent(selectedIdentity.getPublic)(selectedMutatorChoice)
  }

  /** Replaces the graph-derived state with that of `replacement`, e.g. a modified version of [[generated]] */
  protected def useGenerated(replacement: GeneratedBenchmarkRdtEventGraph): Unit = {
    generated = replacement
    eventGraph = replacement.eventGraph
    deltaValueStore = replacement.deltaValueStore
    genesisHash = replacement.eventGraph.genesis
    rootIdentity = replacement.replicaIds(0)
    trace = encodeTrace(replacement.eventGraph)
    rdtState = replacement.state
  }
}

@State(Scope.Benchmark)
class CreateUpdateBenchmarkState extends ArdtEventGraphBenchmarkState {
  var replica: BenchmarkReplica[BenchmarkRdt] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    val tmpReplica = new BenchmarkReplica[BenchmarkRdt](
      genesisHash,
      selectedIdentity,
      r => NoOpAntiEntropy(r),
      noopOnStateChange
    )
    replayTrace(tmpReplica, trace, deltaValueStore)
    val preUpdateSnapshot = tmpReplica.snapshot()

    // Has hook to reset state after update
    replica = new BenchmarkReplica[BenchmarkRdt](
      genesisHash,
      selectedIdentity,
      r => NoOpAntiEntropy(r),
      noopOnStateChange
    ) {
      override protected def disseminate(eventsWithDeltas: Iterable[(
          Hash,
          ArdtEvent,
          Array[Byte],
          BenchmarkRdt,
          Commitment.RevealedValue
      )]): Unit = {
        this.eventGraph = preUpdateSnapshot.eventGraph
        this.materializedState = preUpdateSnapshot.materializedState
        eventsWithDeltas.foreach(evTpl =>
          this.deltaValueStore.remove(evTpl._2.payload.asInstanceOf[DeltaCommitment].commitment)
        )
      }
    }
    replica.restore(preUpdateSnapshot)
  }
}

/** [[ArdtEventGraphBenchmarkState]] whose trace ends in a revocation, authored by the root replica, of the
  * capability granting write access to either `a.*` or `a.a.*`.
  */
@State(Scope.Benchmark)
class RevocationBenchmarkState extends ArdtEventGraphBenchmarkState {

  // <revoked subtree>-<parents of the revocation>
  @Param(Array("a-concurrent", "a-heads", "a.a-concurrent", "a.a-heads"))
  var revocation: String = scala.compiletime.uninitialized

  var replica: BenchmarkReplica[BenchmarkRdt] = scala.compiletime.uninitialized
  var encodedRevocation: Array[Byte]          = scala.compiletime.uninitialized

  // The parts of the replica's state that receiving the revocation changes, as they were before
  private var invalidatesDeltas: Boolean                               = scala.compiletime.uninitialized
  private var replicaSnapshot: BenchmarkReplica.Snapshot[BenchmarkRdt] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()
    val (subtree, parents) = revocation.splitAt(revocation.lastIndexOf('-'))
    invalidatesDeltas = parents == "-concurrent"
    useGenerated(
      parents match {
        case "-concurrent" => TraceGeneration.revokeConcurrently(generated, subtree)
        case "-heads"      => TraceGeneration.revokeAtHeads(generated, subtree)
      }
    )

    // The revocation is the last event of the trace
    encodedRevocation = trace.last.encodedEvent
    replica = new BenchmarkReplica[BenchmarkRdt](genesisHash, rootIdentity, r => NoOpAntiEntropy(r), noopOnStateChange)
    replayTrace(replica, trace.init, deltaValueStore)
    replicaSnapshot = replica.snapshot()
  }

  /** Removes the revocation from the replica again. The event graph is immutable, so putting back the one from
    * before the revocation also restores its heads. Only a concurrent revocation invalidates deltas, making the
    * replica re-materialize its state. Receiving a revocation never touches the delta value store.
    */
  @Setup(Level.Invocation)
  def resetReplica(): Unit = {
    replica.currentEventGraph = replicaSnapshot.eventGraph
    if invalidatesDeltas then replica.restore(replicaSnapshot)
  }
}

@State(Scope.Benchmark)
class SignedHashDagBenchmarkState extends ArdtEventGraphBenchmarkState {

  var hashDag: HashDag[BenchmarkRdt, SignedHashDagEntry[BenchmarkRdt]] = scala.compiletime.uninitialized
  var hashDagTrace: Array[Array[Byte]]                                 = scala.compiletime.uninitialized

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

  var hashDag: HashDag[BenchmarkRdt, UnsignedHashDagEntry[BenchmarkRdt]] = scala.compiletime.uninitialized
  var hashDagTrace: Array[Array[Byte]]                                   = scala.compiletime.uninitialized

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
    replayTrace(replica, trace, deltaValueStore)

    destination = generated.replicaIds(1).getPublic
    connectionManager = SummingConnectionManager(Set(destination))
    // The control plane is never consulted by sendEventsWithDelta, and is thus left unimplemented.
    antiEntropy = AntiEntropy(replica, _ => connectionManager, _ => ???)
    eventHashes = trace.map(_.hash)
  }
}

@State(Scope.Benchmark)
class SignedHashDagBenchmarkStateWithReplica extends SignedHashDagBenchmarkState {

  var connectionManager: SummingConnectionManager                              = scala.compiletime.uninitialized
  var replica: HashDagReplica[SignedHashDagEntry[BenchmarkRdt], BenchmarkRdt]  = scala.compiletime.uninitialized
  var destination: PublicIdentity                                              = scala.compiletime.uninitialized
  var entryHashes: Array[Hash]                                                 = scala.compiletime.uninitialized
  var snapshotHashDag: HashDag[BenchmarkRdt, SignedHashDagEntry[BenchmarkRdt]] = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()

    destination = rootIdentity.getPublic
    connectionManager = SummingConnectionManager(Set(destination))

    replica = new HashDagReplica[SignedHashDagEntry[BenchmarkRdt], BenchmarkRdt](hashDag.genesis, connectionManager)
    hashDagTrace.foreach { encodedEntry => replica.receiveEntry(encodedEntry) }

    entryHashes = hashDagTrace.map(Hash.compute)
    snapshotHashDag = replica.hashDag
  }
}

@State(Scope.Benchmark)
class UnsignedHashDagBenchmarkStateWithReplica extends UnsignedHashDagBenchmarkState {

  var connectionManager: SummingConnectionManager                               = scala.compiletime.uninitialized
  var replica: HashDagReplica[UnsignedHashDagEntry[BenchmarkRdt], BenchmarkRdt] = scala.compiletime.uninitialized
  var destination: PublicIdentity                                               = scala.compiletime.uninitialized
  var entryHashes: Array[Hash]                                                  = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  override def setup(): Unit = {
    super.setup()

    destination = rootIdentity.getPublic
    connectionManager = SummingConnectionManager(Set(destination))

    replica = new HashDagReplica[UnsignedHashDagEntry[BenchmarkRdt], BenchmarkRdt](hashDag.genesis, connectionManager)
    hashDagTrace.foreach { encodedEntry => replica.receiveEntry(encodedEntry) }

    entryHashes = hashDagTrace.map(Hash.compute)
  }
}

object EvaluationBenchmarks {
  def noopOnStateChange[T](x: => T): Unit = ()

  /** Has a fresh replica of the root identity receive every event of `state`'s trace, along with its delta */
  def receiveTrace(state: ArdtEventGraphBenchmarkState): Set[Hash] = {
    val replica = new Replica[BenchmarkRdt](
      state.genesisHash,
      state.rootIdentity,
      r => NoOpAntiEntropy(r),
      noopOnStateChange
    )
    replayTrace(replica, state.trace, state.deltaValueStore)
    replica.heads
  }

  /** Has `replica` receive every event of `trace` in order, each directly followed by its delta (if any) */
  def replayTrace(
      replica: Replica[BenchmarkRdt],
      trace: Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])],
      deltaValueStore: DeltaValueStore[BenchmarkRdt]
  ): Unit =
    trace.foreach { (hash, encodedEvent, deltaCommitment) =>
      replica.receiveEvent(encodedEvent)
      deltaCommitment.foreach { commitment =>
        deltaValueStore.getRevealedValue(commitment).foreach(revealed => replica.receiveDelta(hash, revealed))
      }
    }

  /** Encodes every event of `eventGraph` in causal order, along with its delta commitment (if any) */
  def encodeTrace(eventGraph: ArdtEventGraph[BenchmarkRdt])
      : Array[(hash: Hash, encodedEvent: Array[Byte], deltaCommitment: Option[Hash])] =
    eventGraph.allEventsInCausalOrder.map { (hash, event) =>
      val deltaCommitment = event.payload match {
        case DeltaCommitment(commitment) => Some(commitment)
        case _                           => None
      }
      (hash = hash, encodedEvent = writeToArray(event), deltaCommitment = deltaCommitment)
    }
}

object EvaluationRunner {
  def main(args: Array[String]): Unit = {
    val state = new CreateUpdateBenchmarkState()
    state.numEvents = 100_000
    // state.revocation = "a-concurrent"
    state.setup()
    val bench = new EvaluationBenchmarks()
    println("Done with setup")
    val timeStart = System.nanoTime()
    0.until(1_000_000) foreach { _ =>
      bench.createUpdate(state)
    }
    println((System.nanoTime() - timeStart) / 1_000_000_000.0)
  }
}
