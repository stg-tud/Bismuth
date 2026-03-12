package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.evaluation.centralized.ForwardingSync
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.travelplanner.TravelPlan

import java.net.InetAddress

class BenchmarkRelayReplica(
    val ifAddress: InetAddress,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforceAcl: Boolean,
) {
  // TODO: Should the relay merge updates into the local state?
  val sync: ForwardingSync[TravelPlan] =
    ForwardingSync(
      identity,
      (id, recv) => ChannelConnectionManager(id, recv, ifAddress),
      aclGenesis,
      enforceAcl
    )

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
