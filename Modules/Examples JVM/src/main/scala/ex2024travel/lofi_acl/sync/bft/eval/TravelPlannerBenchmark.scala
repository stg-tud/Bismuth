package ex2024travel.lofi_acl.sync.bft.eval

import crypto.PublicIdentity
import crypto.channels.{IdentityFactory, PrivateIdentity}
import ex2024travel.lofi_acl.sync.bft.*
import ex2024travel.lofi_acl.sync.bft.eval.SavedTrace.{DeltaTrace, NotificationTrace}
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.*
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.TravelPlanMutator.*
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class TravelPlannerBenchmark private[TravelPlannerBenchmark] (
    val numReplicas: Int,
    val identities: Array[PrivateIdentity],
    val aclRoot: SerializedAclOp
)(using val registry: MockConnectionRegistry) {
  require(numReplicas == identities.length)
  val antiEntropyInstances: Array[BftFilteringAntiEntropy[RDT]] = Array.ofDim(numReplicas)
  var ids: Array[PublicIdentity]                                = identities.map(_.getPublic)
  var indices: Map[PublicIdentity, Int]                         = ids.zipWithIndex.toMap
  var replicas: Array[ReplicaWithBftAcl[RDT]]                   = scala.compiletime.uninitialized

  def performRandomRdtAction(
      permittedMutators: Array[Array[TravelPlanMutator]],
      authoringReplica: Int,
      minEntriesPerMap: Int,
      maxEntriesPerMap: Int
  )(using random: Random): (Int, RDT) = {
    given LocalUid = LocalUid(Uid(ids(authoringReplica).id))
    given Random   = random

    val mutators = permittedMutators(authoringReplica)
    require(mutators.length > 0)

    val state = replicas(authoringReplica).currentState
    val delta = retryUntilSuccess { // Need to retry, because removal/update doesn't work on empty collection
      mutators(random.nextInt(mutators.length)) match {
        case SET_TITLE                                                            => state.setTitle(dummy)
        case ADD_BUCKET_LIST_ENTRY if state.bucketList.size < maxEntriesPerMap    => state.addBucketListEntry(dummy)
        case REMOVE_BUCKET_LIST_ENTRY if state.bucketList.size > minEntriesPerMap =>
          state.removeBucketListEntry(pickOne(state.bucketList.keySet))
        case SET_BUCKET_LIST_ENTRY_TEXT => state.setBucketListEntryText(pickOne(state.bucketList.keySet), dummy)
        case ADD_EXPENSE if state.expenses.size < maxEntriesPerMap    => state.addExpense(dummy, dummy)
        case REMOVE_EXPENSE if state.expenses.size > minEntriesPerMap =>
          state.removeExpense(pickOne(state.expenses.keySet))
        case SET_EXPENSE_AMOUNT      => state.setExpenseAmount(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_DESCRIPTION => state.setExpenseDescription(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_COMMENT     => state.setExpenseComment(pickOne(state.expenses.keySet), dummy)
        case _                       => ???
      }
    }
    (authoringReplica, delta)
  }

  def connect(connectionMap: Map[Int, Set[Int]]): Unit = {
    connectionMap.foreach { (from, destinations) =>
      destinations.foreach(peerIndex =>
          val peer         = ids(peerIndex)
          val (host, port) = registry.address(peer)
          replicas(from).connect(peer, s"$host:$port")
      )
    }
  }

  def permittedMutators: Array[Array[TravelPlanMutator]] = replicas.indices.map(replicaIdx =>
      val writePerm = replicas(replicaIdx).currentAcl.write(ids(replicaIdx))
      val mutators  = mutable.ListBuffer.empty[TravelPlanMutator]
      if PermissionTree.fromPath("title") <= writePerm then mutators.addOne(SET_TITLE): Unit
      if PermissionTree.fromPath("bucketList") <= writePerm then
          mutators.addOne(ADD_BUCKET_LIST_ENTRY).addOne(REMOVE_BUCKET_LIST_ENTRY)
            .addOne(SET_BUCKET_LIST_ENTRY_TEXT): Unit
      if PermissionTree.fromPath("expenses") <= writePerm then
          mutators
            .addOne(ADD_EXPENSE).addOne(REMOVE_EXPENSE)
            .addOne(SET_EXPENSE_AMOUNT).addOne(SET_EXPENSE_DESCRIPTION).addOne(SET_EXPENSE_COMMENT): Unit
      mutators.toArray
  ).toArray

  def replayTrace(trace: DeltaTrace, notificationTrace: NotificationTrace, numOps: Int): Array[Long] = {
    require(numOps % 1_000 == 0)

    while antiEntropyInstances.exists(_.msgQueue.size() > 0) do
        antiEntropyInstances.foreach(_.processAllMessagesInInbox(0))

    var opCounter = 0
    val times     = Array.ofDim[Long](numOps / 1_000 + 1)

    trace.indices.foreach { roundIndex =>
      trace(roundIndex).foreach { (authorIndex, delta) =>
        if opCounter % 1_000 == 0 then times(opCounter / 1_000) = System.nanoTime()
        opCounter += 1
        replicas(authorIndex).mutateState(_ => delta)
      }
      antiEntropyInstances.foreach { antiEntropy =>
        antiEntropy.processAllMessagesInInbox(incomingMessagePollTimeoutMillis = 0)
      }
      antiEntropyInstances.indices.foreach(replicaIdx =>
        antiEntropyInstances(replicaIdx).notifyPeerAboutLocalState(ids(notificationTrace(roundIndex)(replicaIdx)))
      )
    }
    times(opCounter / 1_000) = System.nanoTime()
    require(opCounter == numOps)
    times
  }

  def assignPermission(from: Int, to: Int, readPerm: PermissionTree, writePerm: PermissionTree): Unit =
    replicas(from).grantPermissions(ids(to), readPerm.merge(writePerm), writePerm)
}

object TravelPlannerBenchmark {
  type RDT = TravelPlan

  def antiEntropyFactory(bench: TravelPlannerBenchmark)(
      id: PrivateIdentity,
      aclRoot: SerializedAclOp,
      sync: ReplicaWithBftAcl[RDT]
  )(using registry: MockConnectionRegistry): BftFilteringAntiEntropy[RDT] = {
    val antiEntropy = BftFilteringAntiEntropy[RDT](
      id,
      aclRoot,
      sync,
      (id, rcv) => {
        val mgr = MockConnectionManager(id.getPublic, rcv)
        mgr.acceptIncomingConnections() // usually start() of anti entropy does this for us
        mgr
      },
      autoConnect = false,
      Random(42)
    )
    bench.antiEntropyInstances(bench.indices(id.getPublic)) = antiEntropy
    antiEntropy
  }

  def nonFilteringAntiEntropyFactory(bench: TravelPlannerBenchmark)(
      id: PrivateIdentity,
      aclRoot: SerializedAclOp,
      sync: ReplicaWithBftAcl[RDT]
  )(using registry: MockConnectionRegistry): BftFilteringAntiEntropy[RDT] = {
    val antiEntropy = NonFilteringAntiEntropy[RDT](
      id,
      aclRoot,
      sync,
      (id, rcv) => {
        val mgr = MockConnectionManager(id.getPublic, rcv)
        mgr.acceptIncomingConnections() // usually start() of anti entropy does this for us
        mgr
      },
      autoConnect = false
    )
    bench.antiEntropyInstances(bench.indices(id.getPublic)) = antiEntropy
    antiEntropy
  }

  def apply(
      numReplicas: Int,
      identities: Array[PrivateIdentity],
      aclRoot: SerializedAclOp,
      withAcl: Boolean = true
  ): TravelPlannerBenchmark = {
    given MockConnectionRegistry = MockConnectionRegistry()
    val bench                    = new TravelPlannerBenchmark(numReplicas, identities, aclRoot)

    bench.replicas =
      identities.map(identity =>
        ReplicaWithBftAcl[RDT](
          identity,
          aclRoot,
          (_: RDT) => (),
          if withAcl then antiEntropyFactory(bench) else nonFilteringAntiEntropyFactory(bench)
        )
      )
    bench
  }

  def apply(numReplicas: Int): TravelPlannerBenchmark = {
    val identities               = Array.fill(numReplicas)(IdentityFactory.createNewIdentity)
    val aclRoot: SerializedAclOp = BftAclOpGraph.createSelfSignedRoot(identities(0))
    apply(numReplicas, identities, aclRoot)
  }

  def createTrace(
      numReplicas: Int,
      numOperations: Int,
      connectionMap: Map[Int, Set[Int]],
      permissionAssignmentFunction: TravelPlannerBenchmark => Unit,
      mapMinEntries: Int = 0, // Don't remove values from a map if it contains less or equal to this amount of entries
      mapMaxEntries: Int = Int.MaxValue // Don't add values to a map if it already contains this amount of entries
  ): (DeltaTrace, NotificationTrace, TravelPlannerBenchmark) = {
    require(numReplicas >= 2)
    require(mapMinEntries <= mapMaxEntries)
    given random: Random = Random(42)

    val bench = TravelPlannerBenchmark(4)
    bench.connect(connectionMap)

    permissionAssignmentFunction(bench)

    while bench.antiEntropyInstances.exists(_.msgQueue.size() > 0) ||
        bench.replicas.map(_.currentOpGraph.heads).toSet.size > 1
    do {
      bench.antiEntropyInstances.foreach(_.processAllMessagesInInbox(0))
      bench.antiEntropyInstances.foreach(antiEntropy =>
        antiEntropy.connectedPeers.foreach(antiEntropy.notifyPeerAboutLocalState)
      )
      bench.antiEntropyInstances.foreach(_.processAllMessagesInInbox(0))
    }

    val permittedMutators = bench.permittedMutators

    var mutationRoundIndex                   = 0 // Counts the number of rounds in which deltas are created
    var numCreatedDeltas                     = 0 // Counts the number of all (including concurrent) deltas
    val deltas: Array[Seq[(Int, RDT)]]       = Array.ofDim(numOperations)
    val notificationTrace: Array[Array[Int]] = Array.ofDim(numOperations, numReplicas)
    while numCreatedDeltas < numOperations do {
      // Always perform at least one update per round
      val authorA = random.nextInt(numReplicas)
      deltas(mutationRoundIndex) =
        Seq(bench.performRandomRdtAction(permittedMutators, authorA, mapMinEntries, mapMaxEntries))
      numCreatedDeltas += 1

      // Sometimes perform a second (concurrent) update (1 in 3)
      if random.nextInt(numReplicas) == 0 && numCreatedDeltas < numOperations then
          var authorB = random.nextInt(numReplicas)
          // Choose a different replica
          while authorB == authorA do authorB = random.nextInt(numReplicas)
          deltas(mutationRoundIndex) =
            deltas(mutationRoundIndex) :+ bench.performRandomRdtAction(
              permittedMutators,
              authorB,
              mapMinEntries,
              mapMaxEntries
            )
          numCreatedDeltas += 1

      // Apply deltas to state of replicas and broadcast deltas from authoring replica to peers
      deltas(mutationRoundIndex).foreach { (replicaIdx, delta) =>
        bench.replicas(replicaIdx).mutateState(_ => delta)
      }

      // TODO: Parameterize this as strategy?
      bench.antiEntropyInstances.foreach { antiEntropy =>
        antiEntropy.processAllMessagesInInbox(incomingMessagePollTimeoutMillis = 0)
      }
      bench.antiEntropyInstances.zipWithIndex.foreach { (antiEntropy, sendingReplicaIdx) =>
        val peerId = antiEntropy.connectedPeers.drop(random.nextInt(antiEntropy.connectedPeers.size)).head
        antiEntropy.notifyPeerAboutLocalState(peerId)
        notificationTrace(mutationRoundIndex)(sendingReplicaIdx) = bench.ids.indexOf(peerId)
      }

      // Increment index
      mutationRoundIndex += 1
      if numCreatedDeltas % 10_000 == 0 then print("#")
    }

    (deltas.slice(0, mutationRoundIndex).toVector, notificationTrace.slice(0, mutationRoundIndex), bench)
  }

  enum TravelPlanMutator:
      case SET_TITLE
      case ADD_BUCKET_LIST_ENTRY
      case SET_BUCKET_LIST_ENTRY_TEXT
      case REMOVE_BUCKET_LIST_ENTRY
      case ADD_EXPENSE
      case REMOVE_EXPENSE
      case SET_EXPENSE_AMOUNT
      case SET_EXPENSE_DESCRIPTION
      case SET_EXPENSE_COMMENT

  def dummy(using random: Random): String = random.alphanumeric.take(20).mkString("")

  def pickOne[V](set: Set[V])(using random: Random): V = set.drop(random.nextInt(set.size)).head

  @tailrec
  def retryUntilSuccess[T](action: => T): T =
    try
      action
    catch {
      case _: Throwable => retryUntilSuccess(action)
    }
}
