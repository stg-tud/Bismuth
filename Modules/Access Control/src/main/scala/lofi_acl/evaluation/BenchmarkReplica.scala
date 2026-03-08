package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.evaluation.insecure.NonEnforcingSync
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.travelplanner.TravelPlan

class BenchmarkReplica(
    val ifAddress: String,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforcing: Boolean,
    onRdtChange: BenchmarkReplica => Unit
) {
  val sync: AclEnforcingSync[TravelPlan] = {
    if enforcing then
        AclEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => onRdtChange(this)
        )
    else
        NonEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => onRdtChange(this)
        )
  }

  def applyAllMutations(trace: Array[TravelPlan]): Unit =
    trace.foreach(delta => sync.mutate(_ => delta))

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
