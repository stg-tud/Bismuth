package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.evaluation.insecure.NonEnforcingSync
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.travelplanner.TravelPlan
import rdts.time.Dots

import java.net.InetAddress

class BenchmarkReplica(
    val ifAddress: InetAddress,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforcing: Boolean,
    onRdtChange: (Dots, TravelPlan, BenchmarkReplica) => Unit,
    listenPort: Int = 0,
) {
  val sync: AclEnforcingSync[TravelPlan] = {
    if enforcing then
        AclEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv, ifAddress, listenPort),
          aclGenesis,
          (dots, delta) => onRdtChange(dots, delta, this)
        )
    else
        NonEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv, ifAddress, listenPort),
          aclGenesis,
          (dots, delta) => onRdtChange(dots, delta, this)
        )
  }

  def applyMutation(delta: TravelPlan): Unit = sync.mutate(_ => delta)

  def applyAllMutations(trace: Array[TravelPlan]): Unit =
    trace.foreach(delta => sync.mutate(_ => delta))

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
