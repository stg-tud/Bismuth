package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.DeltaCommitment
import replication.authz.{ArdtEventGraph, Authorization, DeltaValueStore}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object TraceGeneration {
  type BenchmarkRdtMutatorChoice = String

  /** Randomly partitions `paths` into `numParts` non-empty groups and turns each group into the
    * [[PermissionTree]] allowing exactly its paths. Since every path ends up in exactly one group, merging the
    * resulting trees yields the same permissions as `paths` as a whole, while no single one of them grants
    * everything.
    */
  def splitPermissions(paths: Seq[String], numParts: Int)(using random: Random): Seq[PermissionTree] = {
    require(numParts >= 1 && numParts <= paths.size)

    // Cut the shuffled paths at numParts - 1 distinct, interior positions, so that every group is non-empty
    val shuffled = random.shuffle(paths)
    val cuts     = random.shuffle((1 until paths.size).toVector).take(numParts - 1).sorted
    val bounds   = 0 +: cuts :+ paths.size
    bounds.init.lazyZip(bounds.tail).map { (from, until) =>
      PermissionTree.fromPathSet(shuffled.slice(from, until).toSet)
    }
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

  /** Builds a random [[ArdtEventGraph]].
    * Every non-root replica holds `numCapabilitiesPerReplica` capabilities.
    *
    * @param numReplicas number of participating replicas, the root replica included
    * @param numEvents total number of BenchmarkRdt mutations performed across all non-root replicas
    * @param concurrencyProbability probability, per mutation, that its event is made concurrent to a randomly
    *   chosen current head of the graph, by walking back a few steps along that head's ancestry and using the
    *   resulting, slightly older event as its sole parent. A value of 0 produces a graph where every event is
    *   causally ordered after all previously created events.
    * @param numCapabilitiesPerReplica number of capabilities each non-root replica's permissions are split into
    */
  def generateBenchmarkRdtEventGraph(
      numReplicas: Int,
      numEvents: Int,
      concurrencyProbability: Double,
      numCapabilitiesPerReplica: Int,
  )(using random: Random): GeneratedBenchmarkRdtEventGraph = {
    require(numReplicas >= 2) // The root replica authors no updates, so there has to be at least one other
    require(numEvents >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val replicaIds   = BenchmarkHelper.generateReplicaIds(numReplicas)
    val rootIdentity = replicaIds(0)

    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    var eventGraph      = ArdtEventGraph[BenchmarkRdt](genesisEvent)
    val deltaValueStore = DeltaValueStore[BenchmarkRdt]()

    // The permissions of one capability each, shared by all replicas alike
    val permissionSplit = splitPermissions(BenchmarkRdt.leafPaths, numCapabilitiesPerReplica)

    // The capability event authorizing each mutation of each non-root replica: whichever of its capabilities
    // holds the leaf that mutation writes. Since the leaves are split among those capabilities, exactly one of
    // them grants any given mutation.
    val capabilityEvent = mutable.Map.empty[PublicIdentity, Map[BenchmarkRdtMutatorChoice, Hash]]
    replicaIds.drop(1).foreach { identity =>
      val authorizingCapability = mutable.Map.empty[BenchmarkRdtMutatorChoice, Hash]
      permissionSplit.foreach { permissions =>
        val delegation = EventGraphBuilder.buildCapabilityEvent(
          holder = identity.getPublic,
          read = permissions,
          write = permissions,
          author = rootIdentity,
          parents = eventGraph.heads,
          authorization = genesisEvent.hash
        )
        eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, delegation)
        BenchmarkRdt.leafPaths.filter(path => PermissionTree.fromPath(path) <= permissions).foreach { mutatorChoice =>
          authorizingCapability(mutatorChoice) = delegation.hash
        }
      }

      capabilityEvent(identity.getPublic) = authorizingCapability.toMap
    }

    var sharedState = BenchmarkRdt.empty

    for _ <- 0 until numEvents do
        // The root replica only delegates, so authors are drawn from the remaining replicas
        val identity = replicaIds(1 + random.nextInt(numReplicas - 1))
        val author   = identity.getPublic

        given LocalUid    = LocalUid(Uid(author.id))
        val mutatorChoice = BenchmarkRdt.leafPaths.drop(random.nextInt(BenchmarkRdt.leafPaths.size)).head
        val delta         = BenchmarkRdt.applyBenchmarkRdtMutator(mutatorChoice, sharedState)
        sharedState = sharedState.merge(delta)

        val isConcurrentWrite = random.nextDouble() < concurrencyProbability
        val parents           =
          if isConcurrentWrite then
              val heads      = eventGraph.heads
              val chosenHead = heads.iterator.drop(random.nextInt(heads.size)).next()
              val backSteps  = 1 + random.nextInt(3)
              Set(walkBackAFewSteps(eventGraph, chosenHead, backSteps))
          else eventGraph.heads

        // Mirrors Replica.createUpdate: every decomposed part of the delta is authorized by the same
        // capability and built on top of the same parents, making them concurrent siblings of each other.
        val authorization    = capabilityEvent(author)(mutatorChoice)
        val decomposedEvents = delta.decomposed.map { decomposedDelta =>
          EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, authorization)
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
    val rootIdentity     = generated.replicaIds(0)

    val trace = mutable.ArrayBuffer.empty[Array[Byte]]

    val genesisEntry   = buildEntry(BenchmarkRdt.empty, rootIdentity, Set.empty)
    val genesisEncoded = writeToArray(genesisEntry)
    var hashDag        = HashDag.receiveOrThrow(
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
  def translateToSignedHashDag(generated: GeneratedBenchmarkRdtEventGraph)
      : GeneratedHashDagEventGraph[SignedHashDagEntry] =
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

case class GeneratedBenchmarkRdtEventGraph(
    eventGraph: ArdtEventGraph[BenchmarkRdt],
    deltaValueStore: DeltaValueStore[BenchmarkRdt],
    replicaIds: Array[PrivateIdentity],
    capabilityEvent: Map[PublicIdentity, Map[String, Hash]],
    state: BenchmarkRdt
)

case class GeneratedHashDagEventGraph[T <: HashDagEntry](
    hashDag: HashDag[T],
    trace: Array[Array[Byte]],
    replicaIds: Array[PrivateIdentity],
    state: BenchmarkRdt
)
