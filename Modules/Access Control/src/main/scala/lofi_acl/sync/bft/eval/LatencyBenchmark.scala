package lofi_acl.sync.bft.eval

import channels.MessageBuffer
import crypto.PublicIdentity
import crypto.channels.IdentityFactory
import lofi_acl.sync.MessageReceiver
import lofi_acl.sync.bft.{BftAclOpGraph, BftFilteringAntiEntropy, ReplicaWithBftAcl, SerializedAclOp}
import lofi_acl.travelplanner.TravelPlan

class LatencyBenchmark(
    numReplicas: Int,
    numOperations: Int,
    latencyMilliSeconds: Int,
    connectionMap: Map[Int, Set[Int]],
    permissionAssignmentFunction: TravelPlannerBenchmark => Unit,
    withAcl: Boolean = true
)(using val registry: MockConnectionRegistry) {
  type RDT = TravelPlan

  val identities                              = Array.fill(numReplicas)(IdentityFactory.createNewIdentity)
  var ids: Array[PublicIdentity]              = identities.map(_.getPublic)
  var indices: Map[PublicIdentity, Int]       = ids.zipWithIndex.toMap
  val aclRoot: SerializedAclOp                = BftAclOpGraph.createSelfSignedRoot(identities(0))
  val connMans                                = Array.ofDim[SimulatedLatency](numReplicas)
  var replicas: Array[ReplicaWithBftAcl[RDT]] = identities.map(identity =>
    ReplicaWithBftAcl[RDT](
      identity,
      aclRoot,
      (_: RDT) => (),
      (id, rootOp, replica) =>
        if withAcl then
            BftFilteringAntiEntropy[RDT](
              id,
              rootOp,
              replica,
              (id, rcv) => SimulatedLatency(latencyMilliSeconds)(id.getPublic, rcv)
            )
        else
            NonFilteringAntiEntropy[RDT](
              id,
              rootOp,
              replica,
              (id, rcv) => SimulatedLatency(latencyMilliSeconds)(id.getPublic, rcv)
            )
    )
  )
}

/*
Idea: We have 4 replicas, running in separate threads. If we don't care about the order of messages, we can dispatch a
virtual thread with a timeout.
 */

class SimulatedLatency(val latencyMilliSeconds: Int)(
    override val localUserId: PublicIdentity,
    override val messageReceiver: MessageReceiver[MessageBuffer]
)(using registry: MockConnectionRegistry) extends MockConnectionManager(localUserId, messageReceiver) {
  override def send(user: PublicIdentity, msg: MessageBuffer): Unit = {
    Thread.ofVirtual().start { () =>
      Thread.sleep(latencyMilliSeconds)
      super.send(user, msg)
    }: Unit
  }
}
