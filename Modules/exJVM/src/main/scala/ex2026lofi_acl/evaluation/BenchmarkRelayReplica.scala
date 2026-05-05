package ex2026lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import ex2026lofi_acl.evaluation.centralized.ForwardingSync
import ex2026lofi_acl.travelplanner.TravelPlan
import rdts.time.Dots
import replication.acl.bft.{Acl, BftDelta}
import replication.acl.sync.ChannelConnectionManager

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
    (id, recv) =>
      ChannelConnectionManager(id, recv, ifAddress, requestedListenPort = listenPort), // disableLogging = false),
    aclGenesis,
    enforceAcl,
    dots => onRdtChanged(dots, this)
  )

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
