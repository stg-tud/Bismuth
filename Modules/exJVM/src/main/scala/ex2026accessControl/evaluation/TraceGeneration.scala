package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import ex2026accessControl.evaluation.BenchmarkHelper.BenchmarkRdtMutatorChoice
import ex2026accessControl.travelplanner.TravelPlan
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object TraceGeneration {

  def countDecomposed(trace: Array[TravelPlan]): Int = trace.map(delta => delta.decomposed.size).sum

  def countDecomposed(trace: Array[Array[TravelPlan]]): Int = trace.map(countDecomposed).sum

  def pickRandomPermissions(
      possiblePermissions: Seq[String],
      replicaIds: Array[PublicIdentity]
  )(using random: Random): Map[PublicIdentity, PermissionTree] = {
    // Pick one to three random permissions
    def pickRandomPermissions: PermissionTree = {
      var resultingPerm = PermissionTree.empty
      // Pick one to three distinct permissions
      var numPerms         = math.min(random.between(1, 4), possiblePermissions.size)
      var remainingChoices = possiblePermissions
      while numPerms > 0 do
          numPerms = numPerms - 1
          val choice = random.between(0, remainingChoices.size)
          resultingPerm = resultingPerm.merge(PermissionTree.fromPath(remainingChoices(choice)))
          remainingChoices = remainingChoices.patch(choice, Nil, 1) // Remove chosen permission

      resultingPerm
    }

    replicaIds.map(id => id -> pickRandomPermissions).toMap
  }

  /** Walks backwards from `start` along the event graph's ancestry a few steps (picking a random parent at each
    * step whenever an event has more than one), stopping early upon reaching an event without parents (i.e. the
    * genesis event). Used to find an event that is a few steps older than a given head, to use as the sole
    * parent of an event that should be concurrent to recently written events.
    */
  @tailrec
  private def walkBackAFewSteps[T](
      eventGraph: ArdtEventGraph[T],
      start: Hash,
      steps: Int
  )(using random: Random): Hash =
    if steps <= 0 then start
    else
        val parents = eventGraph.events(start)._1.parents
        if parents.isEmpty then start
        else walkBackAFewSteps(eventGraph, parents.iterator.drop(random.nextInt(parents.size)).next(), steps - 1)

  /** Builds a random [[ArdtEventGraph]] of TravelPlan edits, together with the [[DeltaValueStore]] holding the
    * revealed values of all of its delta events, so that [[Authorization.materialize]] can be run on the result.
    *
    * @param numReplicas number of participating replicas
    * @param numEvents total number of TravelPlan mutations performed across all replicas; for each one, a
    *   replica is chosen uniformly at random to author it.
    * @param minEntriesPerMapPerReplica lower bound on entries kept in bucketList/expenses (entriesPerMap)
    * @param maxEntriesPerMapPerReplica upper bound on entries kept in bucketList/expenses (entriesPerMap)
    * @param concurrencyProbability probability, per mutation, that its event is made concurrent to a randomly
    *   chosen current head of the graph, by walking back a few steps along that head's ancestry and using the
    *   resulting, slightly older event as its sole parent, instead of building on top of the current heads. A
    *   value of 0 produces a graph where every event is causally ordered after all previously created events
    *   (no concurrency), while a value of 1 makes (almost) every event concurrent to recently written events.
    */
  def generateEventGraph(
      numReplicas: Int,
      numEvents: Int,
      minEntriesPerMapPerReplica: Int,
      maxEntriesPerMapPerReplica: Int,
      concurrencyProbability: Double,
  )(using random: Random): GeneratedEventGraph[TravelPlan] = {
    require(numReplicas >= 1)
    require(numEvents >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val replicaIds   = BenchmarkHelper.generateReplicaIds(numReplicas)
    val rootIdentity = replicaIds(0)

    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    var eventGraph      = ArdtEventGraph[TravelPlan](genesisEvent)
    val deltaValueStore = DeltaValueStore[TravelPlan]()

    val writePermissions =
      pickRandomPermissions(Seq("title", "bucketList", "expenses"), replicaIds.drop(1).map(_.getPublic))
      + (rootIdentity.getPublic -> PermissionTree.allow)

    // The capability event that authorizes each replica's writes. The root replica writes under the
    // genesis capability directly; every other replica is delegated a capability restricted to its
    // randomly assigned write permissions (but full read permissions, as required for benchmarking).
    val capabilityEvent = mutable.Map(rootIdentity.getPublic -> genesisEvent.hash)
    replicaIds.drop(1).foreach { identity =>
      val delegation = EventGraphBuilder.buildCapabilityEvent(
        holder = identity.getPublic,
        read = PermissionTree.allow,
        write = writePermissions(identity.getPublic),
        author = rootIdentity,
        parents = eventGraph.heads,
        authorization = genesisEvent.hash
      )
      eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, delegation)
      capabilityEvent(identity.getPublic) = delegation.hash
    }

    val permittedMutators =
      replicaIds.map(identity => BenchmarkHelper.permittedMutators(writePermissions(identity.getPublic)))

    // The TravelPlan state used to pick the next random mutation, shared by all replicas alike (rather than
    // each replica tracking its own local view), independently of the causal structure of the event graph.
    var sharedState = TravelPlan.empty

    for _ <- 0 until numEvents do
        // Pick a single, uniformly random replica to author this event, instead of distributing events
        // evenly among replicas.
        val replicaIndex = random.nextInt(numReplicas)
        val identity     = replicaIds(replicaIndex)
        val author       = identity.getPublic

        given LocalUid = LocalUid(Uid(author.id))
        val delta      = BenchmarkHelper.randomTravelPlanDelta(
          permittedMutators(replicaIndex),
          minEntriesPerMapPerReplica,
          maxEntriesPerMapPerReplica,
          sharedState
        )
        sharedState = sharedState.merge(delta)

        val isConcurrentWrite = random.nextDouble() < concurrencyProbability
        val parents           =
          if isConcurrentWrite then
              // Make this event concurrent to recently written events by building on top of an ancestor a
              // few steps behind a randomly chosen current head, instead of on top of the current heads.
              val heads      = eventGraph.heads
              val chosenHead = heads.iterator.drop(random.nextInt(heads.size)).next()
              val backSteps  = 1 + random.nextInt(3)
              Set(walkBackAFewSteps(eventGraph, chosenHead, backSteps))
          else eventGraph.heads

        // Mirrors Replica.createUpdate: every decomposed part of the delta is authorized by the same
        // capability and built on top of the same parents, making them concurrent siblings of each other.
        val decomposedEvents = delta.decomposed.map { decomposedDelta =>
          EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, capabilityEvent(author))
        }.toArray

        decomposedEvents.foreach { case (event, revealed) =>
          eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, event)
          deltaValueStore.put(revealed)
        }

    GeneratedEventGraph(eventGraph, deltaValueStore, replicaIds)
  }

  /** Alternative to [[generateEventGraph]] that builds a random [[ArdtEventGraph]] of edits to [[BenchmarkRdt]], an
    * example RDT combining [[rdts.datatypes.LastWriterWins]] and [[rdts.datatypes.PosNegCounter]] fields nested a
    * few levels deep, instead of the application-shaped [[TravelPlan]]. Write permissions for non-root replicas are
    * assembled from one to three randomly chosen paths out of [[BenchmarkRdt.benchmarkRdtPerms]].
    *
    * Beyond what [[GeneratedEventGraph]] holds, this also exposes the capability event authorizing each
    * replica's writes, the mutators each replica is permitted to use, and the fully merged [[BenchmarkRdt]]
    * state, so that a benchmark can go on to author one further, realistic event on top of the generated graph
    * without redoing any of this setup.
    *
    * @param numReplicas number of participating replicas
    * @param numEvents total number of BenchmarkRdt mutations performed across all replicas; for each one, a
    *   replica is chosen uniformly at random to author it.
    * @param concurrencyProbability see [[generateEventGraph]]
    */
  def generateBenchmarkRdtEventGraph(
      numReplicas: Int,
      numEvents: Int,
      concurrencyProbability: Double,
  )(using random: Random): GeneratedBenchmarkRdtEventGraph = {
    require(numReplicas >= 1)
    require(numEvents >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val replicaIds   = BenchmarkHelper.generateReplicaIds(numReplicas)
    val rootIdentity = replicaIds(0)

    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    var eventGraph      = ArdtEventGraph[BenchmarkRdt](genesisEvent)
    val deltaValueStore = DeltaValueStore[BenchmarkRdt]()

    val writePermissions =
      pickRandomPermissions(BenchmarkRdt.benchmarkRdtPerms, replicaIds.drop(1).map(_.getPublic))
      + (rootIdentity.getPublic -> PermissionTree.allow)

    val capabilityEvent = mutable.Map(rootIdentity.getPublic -> genesisEvent.hash)
    replicaIds.drop(1).foreach { identity =>
      val delegation = EventGraphBuilder.buildCapabilityEvent(
        holder = identity.getPublic,
        read = PermissionTree.allow,
        write = writePermissions(identity.getPublic),
        author = rootIdentity,
        parents = eventGraph.heads,
        authorization = genesisEvent.hash
      )
      eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, delegation)
      capabilityEvent(identity.getPublic) = delegation.hash
    }

    val permittedMutators =
      replicaIds.map(identity => BenchmarkHelper.permittedBenchmarkRdtMutators(writePermissions(identity.getPublic)))

    var sharedState = BenchmarkRdt.empty

    for _ <- 0 until numEvents do
        val replicaIndex = random.nextInt(numReplicas)
        val identity     = replicaIds(replicaIndex)
        val author       = identity.getPublic

        given LocalUid    = LocalUid(Uid(author.id))
        val mutatorChoice = BenchmarkHelper.randomMutatorChoice(permittedMutators(replicaIndex))
        val delta         = BenchmarkHelper.applyBenchmarkRdtMutator(mutatorChoice, sharedState)
        sharedState = sharedState.merge(delta)

        val isConcurrentWrite = random.nextDouble() < concurrencyProbability
        val parents           =
          if isConcurrentWrite then
              val heads      = eventGraph.heads
              val chosenHead = heads.iterator.drop(random.nextInt(heads.size)).next()
              val backSteps  = 1 + random.nextInt(3)
              Set(walkBackAFewSteps(eventGraph, chosenHead, backSteps))
          else eventGraph.heads

        val decomposedEvents = delta.decomposed.map { decomposedDelta =>
          EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, capabilityEvent(author))
        }.toArray

        decomposedEvents.foreach { case (event, revealed) =>
          eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, event)
          deltaValueStore.put(revealed)
        }

    GeneratedBenchmarkRdtEventGraph(
      eventGraph,
      deltaValueStore,
      replicaIds,
      capabilityEvent.toMap,
      permittedMutators,
      sharedState
    )
  }

  /** Translates an already-built [[GeneratedBenchmarkRdtEventGraph]] (an [[ArdtEventGraph]] of [[BenchmarkRdt]]
    * edits, together with its delta value store) into a [[HashDag]] of the very same edits, in the same causal
    * order and authored by the same replicas, instead of generating an independent random graph — so that the
    * [[HashDag]]-based benchmarks exercise the exact same trace of edits as the [[ArdtEventGraph]]-based ones.
    * Since [[HashDag]] has no notion of access control, every capability/delegation event of the source graph is
    * dropped, and each delta event's parents are reconnected to its nearest ancestor delta event (or the dag's
    * genesis) instead.
    *
    * @param buildEntry builds one dag entry (signed or unsigned) authored by `identity`, on top of `parents`
    */
  def translateToHashDag[T <: HashDagEntry: JsonValueCodec](
      generated: GeneratedBenchmarkRdtEventGraph,
      buildEntry: (payload: BenchmarkRdt, identity: PrivateIdentity, parents: Set[Hash]) => T
  ): GeneratedHashDagEventGraph[T] = {
    val identityByPublic = generated.replicaIds.map(identity => identity.getPublic -> identity).toMap
    val rootIdentity      = generated.replicaIds(0)

    val trace = mutable.ArrayBuffer.empty[Array[Byte]]

    val genesisEntry   = buildEntry(BenchmarkRdt.empty, rootIdentity, Set.empty)
    val genesisEncoded = writeToArray(genesisEntry)
    var hashDag         = HashDag.receiveOrThrow(
      HashDag[T](genesisEntry.hash, Set(genesisEntry.hash), Map.empty),
      genesisEncoded
    )
    trace += genesisEncoded

    // Maps every ArdtEvent hash of the source graph to the set of HashDag entry hashes that should stand in
    // for it as a parent: a singleton of the translated entry, for delta events; for every other event
    // (dropped, since it carries no RDT payload), the resolved parents of that event instead, so that a delta
    // event originally parented on a capability/delegation event ends up parented on that event's nearest
    // ancestor delta event(s)/the genesis once translated.
    val resolved = mutable.Map(generated.eventGraph.genesis -> Set(genesisEntry.hash))

    generated.eventGraph.allEventsInCausalOrder.foreach { (oldHash, event) =>
      if oldHash != generated.eventGraph.genesis then
          event.payload match {
            case DeltaCommitment(commitment) =>
              val delta      = readFromArray[BenchmarkRdt](generated.deltaValueStore.get(commitment).get.value)
              val newParents = event.parents.flatMap(resolved)
              val entry      = buildEntry(delta, identityByPublic(event.author), newParents)
              val encoded    = writeToArray(entry)
              hashDag = HashDag.receiveOrThrow(hashDag, encoded)
              trace += encoded
              resolved(oldHash) = Set(entry.hash)
            case _ =>
              resolved(oldHash) = event.parents.flatMap(resolved)
          }
    }

    GeneratedHashDagEventGraph(hashDag, trace.toArray, generated.replicaIds, generated.state)
  }

  /** [[translateToHashDag]], authoring [[SignedHashDagEntry]] entries. */
  def translateToSignedHashDag(generated: GeneratedBenchmarkRdtEventGraph): GeneratedHashDagEventGraph[SignedHashDagEntry] =
    translateToHashDag(
      generated,
      (payload, identity, parents) => HashDagEntry.createSignedEntry(payload, identity, parents)
    )

  /** [[translateToHashDag]], authoring [[UnsignedHashDagEntry]] entries. */
  def translateToUnsignedHashDag(generated: GeneratedBenchmarkRdtEventGraph)
      : GeneratedHashDagEventGraph[UnsignedHashDagEntry] =
    translateToHashDag(
      generated,
      (payload, identity, parents) => HashDagEntry.createUnsignedEntry(payload, identity, parents)
    )
}

case class GeneratedEventGraph[T](
    eventGraph: ArdtEventGraph[T],
    deltaValueStore: DeltaValueStore[T],
    replicaIds: Array[PrivateIdentity]
)

/** Result of [[TraceGeneration.generateBenchmarkRdtEventGraph]]: the generated graph and delta store, together
  * with everything needed to go on authoring further, realistic [[BenchmarkRdt]] events on top of it without
  * redoing any of the genesis/capability-delegation setup: the capability event authorizing each replica's
  * writes, the mutators each replica is permitted to use, and `state`, the fully merged [[BenchmarkRdt]] value
  * resulting from every generated event.
  */
case class GeneratedBenchmarkRdtEventGraph(
    eventGraph: ArdtEventGraph[BenchmarkRdt],
    deltaValueStore: DeltaValueStore[BenchmarkRdt],
    replicaIds: Array[PrivateIdentity],
    capabilityEvent: Map[PublicIdentity, Hash],
    permittedMutators: Array[Array[BenchmarkRdtMutatorChoice]],
    state: BenchmarkRdt
)

/** Result of [[TraceGeneration.translateToSignedHashDag]]/[[TraceGeneration.translateToUnsignedHashDag]]: the
  * translated dag, together with `trace`, every entry's encoded bytes in causal (insertion) order (needed to
  * replay the dag into a fresh [[HashDag]] via repeated [[HashDag.receiveOrThrow]] calls, as [[HashDag]] itself,
  * unlike [[ArdtEventGraph]], does not track insertion order), and `state`, the fully merged [[BenchmarkRdt]]
  * value resulting from every translated entry (the same value as the source [[GeneratedBenchmarkRdtEventGraph]]'s
  * `state`, since translation preserves every edit).
  */
case class GeneratedHashDagEventGraph[T <: HashDagEntry](
    hashDag: HashDag[T],
    trace: Array[Array[Byte]],
    replicaIds: Array[PrivateIdentity],
    state: BenchmarkRdt
)
