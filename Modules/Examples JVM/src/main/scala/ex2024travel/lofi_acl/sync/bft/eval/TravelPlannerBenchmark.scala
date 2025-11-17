package ex2024travel.lofi_acl.sync.bft.eval

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.channels.{IdentityFactory, PrivateIdentity}
import crypto.{Ed25519Util, PublicIdentity}
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.*
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.TravelPlanMutator.*
import ex2024travel.lofi_acl.sync.bft.{BftAclOpGraph, BftFilteringAntiEntropy, ReplicaWithBftAcl, SerializedAclOp}
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import org.bouncycastle.cert.X509CertificateHolder
import rdts.base.{LocalUid, Uid}
import rdts.filters.PermissionTree
import replication.JsoniterCodecs

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.security.KeyPair
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
      authoringReplica: Int
  )(using random: Random): (Int, RDT) = {
    given LocalUid = LocalUid(Uid(ids(authoringReplica).id))
    given Random   = random

    val mutators = permittedMutators(authoringReplica)
    require(mutators.length > 0)

    val state = replicas(authoringReplica).currentState
    val delta = retryUntilSuccess { // Need to retry, because removal/update doesn't work on empty collection
      mutators(random.nextInt(mutators.length)) match {
        case SET_TITLE                  => state.setTitle(dummy)
        case ADD_BUCKET_LIST_ENTRY      => state.addBucketListEntry(dummy)
        case REMOVE_BUCKET_LIST_ENTRY   => state.removeBucketListEntry(pickOne(state.bucketList.keySet))
        case SET_BUCKET_LIST_ENTRY_TEXT => state.setBucketListEntryText(pickOne(state.bucketList.keySet), dummy)
        case ADD_EXPENSE                => state.addExpense(dummy, dummy)
        case REMOVE_EXPENSE             => state.removeExpense(pickOne(state.expenses.keySet))
        case SET_EXPENSE_AMOUNT         => state.setExpenseAmount(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_DESCRIPTION    => state.setExpenseDescription(pickOne(state.expenses.keySet), dummy)
        case SET_EXPENSE_COMMENT        => state.setExpenseComment(pickOne(state.expenses.keySet), dummy)
      }
    }
    (authoringReplica, delta)
  }

  def connectAll(): Unit = connect((0 until (numReplicas - 1)).map(i => i -> ((i + 1) until numReplicas).toSet).toMap)

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

  def replayTrace(trace: DeltaTrace, notificationTrace: NotificationTrace): Unit = {
    trace.zip(notificationTrace).foreach { (concurrentOps, notificationTrace) =>
      concurrentOps.foreach { (authorIndex, delta) =>
        replicas(authorIndex).mutateState(_ => delta)
      }
      antiEntropyInstances.foreach { antiEntropy =>
        antiEntropy.processAllMessagesInInbox(incomingMessagePollTimeoutMillis = 0)
      }
      antiEntropyInstances.indices.foreach(replicaIdx =>
        antiEntropyInstances(replicaIdx).notifyPeerAboutLocalState(ids(notificationTrace(replicaIdx)))
      )
    }
  }

  def assignPermission(from: Int, to: Int, readPerm: PermissionTree, writePerm: PermissionTree): Unit =
    replicas(from).grantPermissions(ids(to), readPerm.merge(writePerm), writePerm)
}

object TravelPlannerBenchmark {
  type RDT               = TravelPlan
  type DeltaTrace        = Vector[Seq[(Int, RDT)]]
  type NotificationTrace = Array[Array[Int]]

  def apply(numReplicas: Int, identities: Array[PrivateIdentity], aclRoot: SerializedAclOp): TravelPlannerBenchmark = {
    given MockConnectionRegistry = MockConnectionRegistry()
    val bench                    = new TravelPlannerBenchmark(numReplicas, identities, aclRoot)
    val antiEntropyFactory: (PrivateIdentity, SerializedAclOp, ReplicaWithBftAcl[RDT]) => BftFilteringAntiEntropy[RDT] =
      (id, aclRoot, sync: ReplicaWithBftAcl[RDT]) => {
        val antiEntropy = BftFilteringAntiEntropy[RDT](
          id,
          aclRoot,
          sync,
          (id, rcv) => {
            val mgr = MockConnectionManager(id.getPublic, rcv)
            mgr.acceptIncomingConnections() // usually start() of anti entropy does this for us
            mgr
          }
        )
        bench.antiEntropyInstances(bench.indices(id.getPublic)) = antiEntropy
        antiEntropy
      }
    bench.replicas =
      identities.map(identity => ReplicaWithBftAcl[RDT](identity, aclRoot, (_: RDT) => (), antiEntropyFactory))
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
      permissionAssignmentFunction: TravelPlannerBenchmark => Unit
  ): (DeltaTrace, NotificationTrace, TravelPlannerBenchmark) = {
    require(numReplicas >= 2)
    given random: Random = Random(42)

    val bench = TravelPlannerBenchmark(4)
    // bench.connectAll()
    bench.connect(Map(0 -> Set(1, 2, 3)))

    permissionAssignmentFunction(bench)

    val permittedMutators = bench.permittedMutators

    var mutationRoundIndex                   = 0 // Counts the number of rounds in which deltas are created
    var numCreatedDeltas                     = 0 // Counts the number of all (including concurrent) deltas
    val deltas: Array[Seq[(Int, RDT)]]       = Array.ofDim(numOperations)
    val notificationTrace: Array[Array[Int]] = Array.ofDim(numOperations, numReplicas)
    while numCreatedDeltas < numOperations do {
      // Always perform at least one update per round
      val authorA = random.nextInt(numReplicas)
      deltas(mutationRoundIndex) = Seq(bench.performRandomRdtAction(permittedMutators, authorA))
      numCreatedDeltas += 1

      // Sometimes perform a second (concurrent) update (1 in 3)
      if random.nextInt(numReplicas) == 0 && numCreatedDeltas < numOperations then
          var authorB = random.nextInt(numReplicas)
          // Choose a different replica
          while authorB == authorA do authorB = random.nextInt(numReplicas)
          deltas(mutationRoundIndex) =
            deltas(mutationRoundIndex) :+ bench.performRandomRdtAction(permittedMutators, authorB)
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
        val notifiedPeer = random.nextInt(antiEntropy.connectedPeers.size)
        antiEntropy.notifyPeerAboutLocalState(bench.ids(notifiedPeer))
        notificationTrace(mutationRoundIndex)(sendingReplicaIdx) = notifiedPeer
      }

      // Increment index
      mutationRoundIndex += 1
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

object TravelPlannerBenchmarkRunner extends App {

  import TravelPlan.jsonCodec

  given privateIdentityCodec: JsonValueCodec[SavedTrace] = {
    given JsonValueCodec[KeyPair] = JsoniterCodecs.bimapCodec[Array[Byte], KeyPair](
      JsonCodecMaker.make,
      arr => if arr.isEmpty then null else Ed25519Util.rawPrivateKeyBytesToKeyPair(arr),
      keypair => if keypair eq null then Array.empty else Ed25519Util.privateKeyToRawPrivateKeyBytes(keypair.getPrivate)
    )
    given JsonValueCodec[X509CertificateHolder] = JsoniterCodecs.bimapCodec[Array[Byte], X509CertificateHolder](
      JsonCodecMaker.make,
      encoded => if encoded.isEmpty then null else X509CertificateHolder(encoded),
      cert => if cert eq null then Array.empty else cert.getEncoded
    )
    JsonCodecMaker.make
  }

  case class SavedTrace(
      identities: Array[PrivateIdentity],
      aclRoot: SerializedAclOp,
      deltaTrace: DeltaTrace,
      notificationTrace: NotificationTrace
  )

  def permissionAssignment(bench: TravelPlannerBenchmark): Unit = {
    // bench.assignPermission(0, 1, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 2, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 2, PermissionTree.fromPath("expenses"), PermissionTree.fromPath("expenses"))
    // bench.assignPermission(0, 3, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 3, PermissionTree.fromPath("bucketList"), PermissionTree.fromPath("bucketList"))

    // bench.assignPermission(0, 1, PermissionTree.allow, PermissionTree.allow)
    // bench.assignPermission(0, 2, PermissionTree.allow, PermissionTree.allow)
    // bench.assignPermission(0, 3, PermissionTree.allow, PermissionTree.allow)

    bench.assignPermission(0, 1, PermissionTree.empty, PermissionTree.fromPath("title"))
    bench.assignPermission(
      0,
      2,
      PermissionTree.fromPath("title").merge(PermissionTree.fromPath("bucketList")),
      PermissionTree.fromPath("expenses")
    )
    bench.assignPermission(0, 3, PermissionTree.fromPath("title"), PermissionTree.fromPath("bucketList"))

    bench.antiEntropyInstances.foreach(_.processAllMessagesInInbox(0))
  }

  // ########
  val numOps                        = 1_000_000
  val start                         = System.nanoTime()
  val (trace, notifications, bench) = createTrace(4, numOps, permissionAssignment)
  val stop                          = System.nanoTime()
  println(s"${(stop - start) / 1_000_000}ms")
  // ########

  val traceFile = Paths.get("./results/lofi_acl/trace.json")
  Files.createDirectories(traceFile.getParent)
  val traceOutputStream = Files.newOutputStream(
    traceFile,
    StandardOpenOption.WRITE,
    StandardOpenOption.CREATE,
    StandardOpenOption.TRUNCATE_EXISTING
  )
  val savedTrace = SavedTrace(bench.identities, bench.aclRoot, trace, notifications)
  writeToStream(savedTrace, traceOutputStream)
  traceOutputStream.close()

  bench.replicas.zipWithIndex.foreach { (replica, idx) =>
    val path = Paths.get(s"./results/lofi_acl/replica-$idx")
    val out  = Files.newOutputStream(
      path,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    writeToStream(replica.currentState, out)
    out.close()
  }

  {
    val restoredTrace = readFromStream[SavedTrace](Files.newInputStream(traceFile, StandardOpenOption.READ))
    val bench = TravelPlannerBenchmark(restoredTrace.identities.length, restoredTrace.identities, restoredTrace.aclRoot)
    val start = System.nanoTime()
    bench.replayTrace(restoredTrace.deltaTrace, restoredTrace.notificationTrace)
    val stop = System.nanoTime()
    println(s"${(stop - start) / 1_000_000}ms")
  }

  // bench.replicas.foreach { replica =>
  //  println(replica.currentAcl)
  //  println(replica.currentState)
  //  println()
  // }
}
