package ex2026lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import ex2026lofi_acl.evaluation.insecure.NonEnforcingSync
import ex2026lofi_acl.travelplanner.TravelPlan
import rdts.time.Dots
import replication.acl.bft.{Acl, BftDelta}
import replication.acl.sync.ChannelConnectionManager
import replication.acl.sync.anti_entropy.AclEnforcingSync

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
