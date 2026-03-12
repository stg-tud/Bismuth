package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.evaluation.centralized.ForwardingSync
import lofi_acl.travelplanner.TravelPlan

class BenchmarkRelayReplica(
    val ifAddress: String,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforceAcl: Boolean,
    delayMillis: Option[Int]
) {
  // TODO: Should the relay merge updates into the local state?
  val sync: ForwardingSync[TravelPlan] =
    ForwardingSync(
      identity,
      BenchmarkHelper.delayedReceiveChannelConnectionManagerProvider(delayMillis),
      aclGenesis,
      enforceAcl
    )

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
