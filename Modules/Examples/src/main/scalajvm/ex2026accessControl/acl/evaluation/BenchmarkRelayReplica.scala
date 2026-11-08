package ex2026accessControl.acl.evaluation

import channels.crypto.PrivateIdentity
import ex2026accessControl.acl.evaluation.centralized.ForwardingSync
import ex2026accessControl.authz.travelplanner.TravelPlan
import rdts.time.Dots
import ex2026accessControl.acl.{Acl, BftDelta}
import ex2026accessControl.acl.sync.ChannelConnectionManager

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
