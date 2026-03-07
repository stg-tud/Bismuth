package lofi_acl.evaluation

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, AclRdt, BftDelta}
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.sync.insecure.NonEnforcingSync
import lofi_acl.travelplanner.TravelPlan

import java.util.concurrent.Executors
import scala.util.Random

class MeshBenchmark {}

object MeshBenchmark {
  val SEED                            = 42
  val NUM_REPLICAS                    = 10
  val NUM_DELTAS_PER_REPLICA          = 1_000
  val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100

  def main(args: Array[String]): Unit = run(enforcementEnabled = true)

  def run(enforcementEnabled: Boolean): Unit = {
    given Random = Random(SEED)

    val replicas = setup(enforcementEnabled)

    println(replicas(0).sync.currentAcl)

    val trace = TraceGeneration.generateDeltas(
      replicas(0).sync.currentAcl,
      replicas.map(_.identity.getPublic),
      NUM_DELTAS_PER_REPLICA,
      MIN_ENTRIES_PER_MAP_PER_REPLICA,
      MAX_ENTRIES_PER_MAP_PER_REPLICA
    )

    // Sleeping should not be needed, as trace generation takes more than enough time for all replicas to connect
    // Thread.sleep(100)
    require(replicas.forall(_.sync.connectedPeers.size == 9))
    // println(replicas.map(r =>
    //   Debug.shorten(r.identity.getPublic) + ": " + r.sync.connectedPeers.toSeq.map(Debug.shorten).mkString(", ")
    // ).mkString("\n"))

    println("Starting benchmark")

    Executors.newVirtualThreadPerTaskExecutor()
    replicas.zipWithIndex
      .foreach((replica, idx) =>
        Thread.startVirtualThread(() =>
            val startTime = System.nanoTime()
            replica.applyAllMutations(trace(idx))
            val endTime = System.nanoTime()
            println(s"Replica$idx submitted everything after ${(endTime - startTime) / 1_000_000}ms")
        )
      )

    // replicas.foreach(_.stop())
  }

  private def setup(enforcementEnabled: Boolean)(using random: Random): Array[MeshBenchmarkReplica] = {
    val replicaIds = TraceGeneration.generateReplicaIds(NUM_REPLICAS)
    val genesis    = AclRdt.createSelfSignedRoot(replicaIds(0))
    // Generate permissions for non-root replicas:
    val replicas = replicaIds.map(id => MeshBenchmarkReplica("localhost", id, genesis, enforcementEnabled))

    // Assign permissions
    val permissionsToAssign = TraceGeneration.getAclWithRandomWritePermissions(replicaIds.drop(1).map(_.getPublic))
    replicas(0).sync.delegatePermission(permissionsToAssign)

    // Connect to root (should lead to all other replicas connecting to each other)
    replicas.foreach(_.start())
    replicas.foreach { replica =>
      replica.sync.connect(replicas(0).identity.getPublic, "localhost", replicas(0).sync.listenAddress.get._2)
    }
    replicas
  }
}

class MeshBenchmarkReplica(
    val ifAddress: String,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforcing: Boolean
) {
  val sync: AclEnforcingSync[TravelPlan] = {
    if enforcing then
        AclEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => () // TODO: Hook in here for termination check?
        )
    else
        NonEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => () // TODO: Hook in here for termination check?
        )
  }

  def applyAllMutations(trace: Array[TravelPlan]): Unit =
    trace.foreach(delta => sync.mutate(_ => delta))

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
