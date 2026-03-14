package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.evaluation.centralized.ForwardingSync
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.travelplanner.TravelPlan
import rdts.time.Dots

import java.net.InetAddress

class BenchmarkRelayReplica(
    val ifAddress: InetAddress,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforceAcl: Boolean,
    listenPort: Int = 0,
    onRdtChanged: (Dots, BenchmarkRelayReplica) => Unit = (_, _) => ()
) {
  val sync: ForwardingSync[TravelPlan] = ForwardingSync(
    identity,
    (id, recv) => ChannelConnectionManager(id, recv, ifAddress, requestedListenPort = listenPort),// disableLogging = false),
    aclGenesis,
    enforceAcl,
    dots => onRdtChanged(dots, this)
  )

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
