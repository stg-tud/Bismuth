package ex2026accessControl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import crypto.channels.{IdentityFactory, PrivateIdentity}
import crypto.{Hash, PublicIdentity}
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment}
import replication.authz.{ArdtEvent, ArdtEventGraph, Authorization, DeltaValueStore}

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
    * @param concurrencyProbability probability, per mutation, that an event is concurrent
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

    val replicaIds   = 0.until(numReplicas).map(_ => IdentityFactory.createNewIdentity).toArray
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

  /** Builds an [[ArdtEventGraph]] with a two-level delegation hierarchy, in four phases:
    *
    *   1. The root replica delegates write access to one top-level subtree (`a.*`, `b.*`, `c.*`) each to three
    *      new replicas.
    *   1. `numEventsPhase1` mutations, authored by the root replica and those three replicas.
    *   1. Each of the three replicas delegates one second-level subtree of its own (e.g. `a.a.*`, `a.b.*`,
    *      `a.c.*`) each to three new replicas.
    *   1. `numEventsPhase2` mutations, authored by all 13 replicas.
    *
    * Every capability allows reading everything. A mutation is concurrent with probability
    * `concurrencyProbability`, in which case its author builds it on top of its own previous event instead of the
    * current heads of the graph. A replica that has not authored anything yet treats the delegation event granting
    * its capability as its previous event.
    *
    * @param numEventsPhase1 number of mutations performed by the root and the top-level subtree replicas
    * @param numEventsPhase2 number of mutations performed by all replicas, after the second-level delegations
    * @param concurrencyProbability probability, per mutation, that an event is concurrent
    */
  def generateDelegationHierarchyEventGraph(
      numEventsPhase1: Int,
      numEventsPhase2: Int,
      concurrencyProbability: Double,
  )(using random: Random): GeneratedBenchmarkRdtEventGraph = {
    require(numEventsPhase1 >= 0 && numEventsPhase2 >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val subtreeLabels = Seq("a", "b", "c")

    val rootIdentity = IdentityFactory.createNewIdentity
    val replicaIds   = mutable.ArrayBuffer(rootIdentity)

    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    var eventGraph      = ArdtEventGraph[BenchmarkRdt](genesisEvent)
    val deltaValueStore = DeltaValueStore[BenchmarkRdt]()
    var sharedState     = BenchmarkRdt.empty

    // The capability event authorizing each mutation a replica may perform, i.e. each leaf it may write
    val capabilityEvent = mutable.Map(rootIdentity.getPublic -> BenchmarkRdt.leafPaths.map(_ -> genesisEvent.hash).toMap)
    // The event each replica would build a concurrent event on top of
    val previousEvent = mutable.Map(rootIdentity.getPublic -> genesisEvent.hash)

    // Delegates write access to `subtree` (and read access to everything) to a new replica
    def delegate(delegator: PrivateIdentity, subtree: String): PrivateIdentity = {
      val holder         = IdentityFactory.createNewIdentity
      val subtreeLeaves  = BenchmarkRdt.leafPaths.filter(_.startsWith(s"$subtree."))
      val delegatorCaps   = capabilityEvent(delegator.getPublic)
      val delegation     = EventGraphBuilder.buildCapabilityEvent(
        holder = holder.getPublic,
        read = PermissionTree.allow,
        write = PermissionTree.fromPath(s"$subtree.*"),
        author = delegator,
        parents = eventGraph.heads,
        authorization = delegatorCaps(subtreeLeaves.head)
      )
      eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, delegation)

      replicaIds += holder
      capabilityEvent(holder.getPublic) = subtreeLeaves.map(_ -> delegation.hash).toMap
      previousEvent(delegator.getPublic) = delegation.hash
      previousEvent(holder.getPublic) = delegation.hash
      holder
    }

    def performUpdates(numEvents: Int, authors: IndexedSeq[PrivateIdentity]): Unit = {
      // Sorted, so that the choice of mutation only depends on the seed
      val permittedMutations = authors.map(identity => capabilityEvent(identity.getPublic).keys.toIndexedSeq.sorted)

      for _ <- 0 until numEvents do
          val authorIndex = random.nextInt(authors.size)
          val identity    = authors(authorIndex)
          val author      = identity.getPublic

          given LocalUid    = LocalUid(Uid(author.id))
          val mutations     = permittedMutations(authorIndex)
          val mutatorChoice = mutations(random.nextInt(mutations.size))
          val delta         = BenchmarkRdt.applyBenchmarkRdtMutator(mutatorChoice, sharedState)
          sharedState = sharedState.merge(delta)

          val isConcurrentWrite = random.nextDouble() < concurrencyProbability
          val parents           = if isConcurrentWrite then Set(previousEvent(author)) else eventGraph.heads

          // Mirrors Replica.createUpdate: every decomposed part of the delta is authorized by the same
          // capability and built on top of the same parents, making them concurrent siblings of each other.
          val authorization = capabilityEvent(author)(mutatorChoice)
          delta.decomposed.foreach { decomposedDelta =>
            val (event, revealed) = EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, authorization)
            eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, event)
            deltaValueStore.put(revealed)
            previousEvent(author) = event.hash
          }
    }

    val subtreeHolders = subtreeLabels.map(label => delegate(rootIdentity, label))
    performUpdates(numEventsPhase1, (rootIdentity +: subtreeHolders).toIndexedSeq)

    subtreeHolders.lazyZip(subtreeLabels).foreach { (holder, label) =>
      subtreeLabels.foreach(subLabel => delegate(holder, s"$label.$subLabel"))
    }
    performUpdates(numEventsPhase2, replicaIds.toIndexedSeq)

    GeneratedBenchmarkRdtEventGraph(
      eventGraph,
      deltaValueStore,
      replicaIds.toArray,
      capabilityEvent.toMap,
      sharedState
    )
  }

  /** Appends to `generated` the root replica's revocation of the capability granting write access to `subtree`
    * (e.g. `"a"` or `"a.a"`), built on top of that capability's delegation event only. The revocation is thus
    * concurrent to every other event, and since it is the last event of the trace, receiving it invalidates every
    * already-received delta authorized by the revoked capability or any capability delegated from it.
    */
  def revokeConcurrently(generated: GeneratedBenchmarkRdtEventGraph, subtree: String): GeneratedBenchmarkRdtEventGraph =
    val capability = capabilityGrantingWrite(generated.eventGraph, subtree)
    val eventGraph = appendRevocation(generated, capability, Set(capability))
    // The revocation invalidates deltas, so the resulting state has to be recomputed
    generated.copy(eventGraph = eventGraph, state = Authorization.materialize(eventGraph, generated.deltaValueStore))

  /** Appends to `generated` the root replica's revocation of the capability granting write access to `subtree`
    * (e.g. `"a"` or `"a.a"`), built on top of the current heads. Since every other event is causally before the
    * revocation, it invalidates none of them.
    */
  def revokeAtHeads(generated: GeneratedBenchmarkRdtEventGraph, subtree: String): GeneratedBenchmarkRdtEventGraph =
    val capability = capabilityGrantingWrite(generated.eventGraph, subtree)
    // Nothing is invalidated, so the state stays the same. Recomputing it would also be slow: Authorization checks
    // every affected delta for being causally before the revocation, searching almost the entire graph each time.
    generated.copy(eventGraph = appendRevocation(generated, capability, generated.eventGraph.heads))

  /** The capability event granting write access to exactly `subtree` */
  private def capabilityGrantingWrite(eventGraph: ArdtEventGraph[BenchmarkRdt], subtree: String): Hash = {
    val write      = PermissionTree.fromPath(s"$subtree.*")
    val candidates = eventGraph.events.collect {
      case (hash, (ArdtEvent(Capability(_, _, `write`), _, _, _, _), _)) => hash
    }
    require(candidates.size == 1, s"Expected exactly one capability granting write access to $subtree.*")
    candidates.head
  }

  private def appendRevocation(
      generated: GeneratedBenchmarkRdtEventGraph,
      revokedCapability: Hash,
      parents: Set[Hash]
  ): ArdtEventGraph[BenchmarkRdt] = {
    // The genesis is part of every capability's authorization chain, so the root may revoke any of them
    val revocation = EventGraphBuilder.buildRevocationEvent(
      revokedCapability,
      author = generated.replicaIds(0),
      parents = parents,
      authorization = generated.eventGraph.genesis
    )
    EventGraphBuilder.receiveOrThrow(generated.eventGraph, revocation)
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
