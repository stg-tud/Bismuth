package ex2026accessControl.evaluation

import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
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
      replicaIds: Array[PublicIdentity]
  )(using random: Random): Map[PublicIdentity, PermissionTree] = {
    // Pick one to three random permissions
    def pickRandomPermissions: PermissionTree = {
      var resultingPerm = PermissionTree.empty
      // Pick one to three distinct permissions
      var numPerms         = random.between(1, 4)
      var remainingChoices = Seq("title", "bucketList", "expenses")
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
  )(using random: Random): GeneratedEventGraph = {
    require(numReplicas >= 1)
    require(numEvents >= 0)
    require(concurrencyProbability >= 0.0 && concurrencyProbability <= 1.0)

    val replicaIds   = BenchmarkHelper.generateReplicaIds(numReplicas)
    val rootIdentity = replicaIds(0)

    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    var eventGraph      = ArdtEventGraph[TravelPlan](genesisEvent)
    val deltaValueStore = DeltaValueStore[TravelPlan]()

    val writePermissions =
      pickRandomPermissions(replicaIds.drop(1).map(_.getPublic)) + (rootIdentity.getPublic -> PermissionTree.allow)

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
        val delta = BenchmarkHelper.randomTravelPlanDelta(
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
}

case class GeneratedEventGraph(
    eventGraph: ArdtEventGraph[TravelPlan],
    deltaValueStore: DeltaValueStore[TravelPlan],
    replicaIds: Array[PrivateIdentity]
)
