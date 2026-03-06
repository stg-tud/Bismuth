package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, AclRdt, BftDelta}
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.sync.insecure.NonEnforcingSync
import lofi_acl.travelplanner.TravelPlan

import scala.util.Random

class MeshBenchmark {}

object MeshBenchmark {
  val NUM_REPLICAS = 10

  def main(args: Array[String]): Unit = {
    val replicas = doSetup(true)
    println("done")
  }

  def doSetup(enforcementEnabled: Boolean): Array[MeshBenchmarkReplica] = {
    given Random   = Random(42)
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

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
