package webapps.ex2024textRDT

import channels.connection.{LocalMessageQueue, QueuedLocalConnection}
import channels.routing.CausalBroadcast
import channels.{MessagingIO, Timers}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import ex2024textRDT.{FugueFactory, Replica}
import rdts.base.ReplicaId

/** Replicates a Fugue [[Replica]] over a [[CausalBroadcast]]. Local operations are sent as one batch on [[publish]]. */
final class TextReplication[F <: FugueFactory](val replica: Replica[F]) {

  given JsonValueCodec[replica.state.factoryContext.MSG] = replica.state.factoryContext.messageCodec
  given JsonValueCodec[replica.Ops]                      = JsonCodecMaker.make

  /** Timers only fire when a [[TextReplication.LocalLink]] syncs. */
  val timers: Timers.Manual = Timers.Manual()

  val causal: CausalBroadcast[replica.Ops] =
    CausalBroadcast(ReplicaId.predefined(replica.state.replicaId), replica.applyRemoteUnchecked, config = MessagingIO.Config(timers = timers))

  def io = causal

  def publish(): Unit =
    val ops = replica.takeOutgoing()
    if ops.nonEmpty then causal.broadcast(ops): Unit
}

object TextReplication {

  /** An in-memory connection between two replications whose messages are only delivered on [[sync]]. */
  final class LocalLink(a: TextReplication[?], b: TextReplication[?]) {
    private val queue = LocalMessageQueue()
    private val link  = QueuedLocalConnection(s"${a.replica.state.replicaId}-${b.replica.state.replicaId}", queue)

    a.io.addServerConnection(link.server)
    b.io.addClientConnection(link.client(b.replica.state.replicaId))

    /** Exchange messages until both sides are quiet (bounded, as repair timers may keep firing). */
    def sync(): Unit = {
      var progress = true
      var rounds   = 0
      while progress && rounds < 100 do {
        rounds += 1
        a.publish()
        b.publish()
        progress = Seq(queue.deliverAll(), a.timers.fireAll(), b.timers.fireAll()).sum > 0
      }
    }
  }
}
