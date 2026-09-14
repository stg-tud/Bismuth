package ex2026accessControl.evaluation

import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import ex2026accessControl.evaluation.BenchmarkHelper.BenchmarkRdtMutatorChoice
import ex2026accessControl.travelplanner.TravelPlan
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
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
    * @param numReplicas number of participating replicas
    * @param numEvents total number of BenchmarkRdt mutations performed across all replicas; for each one, a
    *   replica is chosen uniformly at random to author it.
    * @param concurrencyProbability see [[generateEventGraph]]
    */
  def generateBenchmarkRdtEventGraph(
      numReplicas: Int,
      numEvents: Int,
      concurrencyProbability: Double,
  )(using random: Random): GeneratedEventGraph[BenchmarkRdt] = {
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

    GeneratedEventGraph(eventGraph, deltaValueStore, replicaIds)
  }

  /** Pre-computes every random decision of a [[BenchmarkRdt]] edit trace (write permissions, which replica
    * authors each mutation, which field it touches, and which earlier events it is concurrent with) without
    * paying for the cost of actually creating any events, so that a benchmark can replay the resulting
    * [[BenchmarkRdtTracePlan]] and measure only the mechanical cost of authoring events: computing and
    * merging the delta, decomposing/signing/committing it, and inserting it into a running event graph and
    * delta store.
    *
    * To determine a realistic topology (i.e. which earlier events a concurrent write ends up built on top
    * of), this still builds a full, real [[ArdtEventGraph]] internally, exactly like
    * [[generateBenchmarkRdtEventGraph]] does; that graph is discarded once planning is done; only the
    * sequence of decisions survives, referencing earlier events by their position in the flattened creation
    * order (prefix events, i.e. the genesis event followed by one capability delegation per non-root
    * replica, followed by every mutation event created so far) rather than by hash, so that the plan can be
    * replayed into a different, independently created [[ArdtEventGraph]] whose event hashes need not match
    * the ones seen here (e.g. because [[rdts.datatypes.LastWriterWins]] timestamps are not reproducible
    * across separate runs).
    *
    * @param numReplicas number of participating replicas
    * @param numEvents total number of BenchmarkRdt mutations to plan across all replicas
    * @param concurrencyProbability see [[generateEventGraph]]
    */
  def planBenchmarkRdtTrace(
      numReplicas: Int,
      numEvents: Int,
      concurrencyProbability: Double,
  )(using random: Random): BenchmarkRdtTracePlan = {
    require(numReplicas >= 1)
    require(numEvents >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val replicaIds   = BenchmarkHelper.generateReplicaIds(numReplicas)
    val rootIdentity = replicaIds(0)

    val writePermissions =
      pickRandomPermissions(BenchmarkRdt.benchmarkRdtPerms, replicaIds.drop(1).map(_.getPublic))
      + (rootIdentity.getPublic -> PermissionTree.allow)

    val genesisEvent = Authorization.createGenesis(rootIdentity)
    var eventGraph   = ArdtEventGraph[BenchmarkRdt](genesisEvent)

    val capabilityEvent = mutable.Map(rootIdentity.getPublic -> genesisEvent.hash)
    val eventIndex      = mutable.Map(genesisEvent.hash -> 0)
    replicaIds.drop(1).zipWithIndex.foreach { case (identity, i) =>
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
      eventIndex(delegation.hash) = i + 1
    }

    val permittedMutators =
      replicaIds.map(identity => BenchmarkHelper.permittedBenchmarkRdtMutators(writePermissions(identity.getPublic)))

    var sharedState   = BenchmarkRdt.empty
    var nextIndex     = numReplicas
    val mutationSteps = Array.newBuilder[BenchmarkRdtMutationStep]

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

        mutationSteps += BenchmarkRdtMutationStep(replicaIndex, mutatorChoice, parents.map(eventIndex))

        delta.decomposed.foreach { decomposedDelta =>
          val (event, _) =
            EventGraphBuilder.buildDeltaEvent(decomposedDelta, identity, parents, capabilityEvent(author))
          eventGraph = EventGraphBuilder.receiveOrThrow(eventGraph, event)
          eventIndex(event.hash) = nextIndex
          nextIndex += 1
        }

    BenchmarkRdtTracePlan(replicaIds, writePermissions, mutationSteps.result())
  }
}

case class GeneratedEventGraph[T](
    eventGraph: ArdtEventGraph[T],
    deltaValueStore: DeltaValueStore[T],
    replicaIds: Array[PrivateIdentity]
)

/** One planned mutation of a [[BenchmarkRdtTracePlan]]: `authorIndex` selects the authoring replica out of
  * [[BenchmarkRdtTracePlan.replicaIds]], `mutatorChoice` selects which leaf field of [[BenchmarkRdt]] to
  * mutate, and `parentIndices` names the events (by position in the flattened creation order described on
  * [[TraceGeneration.planBenchmarkRdtTrace]]) that the resulting event(s) are built on top of.
  */
case class BenchmarkRdtMutationStep(
    authorIndex: Int,
    mutatorChoice: BenchmarkRdtMutatorChoice,
    parentIndices: Set[Int]
)

/** A pre-computed [[BenchmarkRdt]] edit trace, produced by [[TraceGeneration.planBenchmarkRdtTrace]]: every
  * random decision has already been made, so that replaying it only involves the mechanical cost of
  * authoring events.
  */
case class BenchmarkRdtTracePlan(
    replicaIds: Array[PrivateIdentity],
    writePermissions: Map[PublicIdentity, PermissionTree],
    mutationSteps: Array[BenchmarkRdtMutationStep]
)
