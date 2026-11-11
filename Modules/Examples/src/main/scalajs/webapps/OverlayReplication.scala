package webapps

import _root_.ex2026overlaydemo.OverlayStatusProtocol
import _root_.ex2026overlaydemo.OverlayStatusProtocol.{Status, given}
import channels.broadcast.PlumtreeMessage.Payload
import channels.{DeltaStorage, MessagingIO}
import channels.broadcast.PlumtreeBroadcast
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalajs.dom
import rdts.base.{Bottom, Lattice, ReplicaId}

import scala.collection.immutable.Queue
import scala.scalajs.js.Date

/** The replicated state of a case study: the application data `A`, combined with the overlay status
  * (who is connected to whom, see [[OverlayStatusProtocol]]), such that both are replicated over the same connections.
  * Deltas usually only contain one of the two.
  */
case class Replicated[A](app: Option[A] = None, overlay: Status = OverlayStatusProtocol.empty)
object Replicated {
  given codec[A: JsonValueCodec]: JsonValueCodec[Replicated[A]] = JsonCodecMaker.make
}

/** Remembers the last `size` application deltas for replay, and merges all overlay status deltas (one is
  * published every second) into a single payload, as they would otherwise quickly evict the application deltas.
  */
final case class ReplicatedHistory[A](
    size: Int,
    app: Queue[Payload[Replicated[A]]] = Queue.empty,
    overlay: Option[Payload[Replicated[A]]] = None,
) extends DeltaStorage[Replicated[A]] {
  override def getHistory: List[Payload[Replicated[A]]] = overlay.toList ++ app

  override def remember(message: Payload[Replicated[A]]): DeltaStorage[Replicated[A]] =
    if message.data.app.isEmpty then
        copy(overlay = Some(overlay.fold(message) { existing =>
          Payload(existing.dots.merge(message.dots), Replicated(overlay = existing.data.overlay.merge(message.data.overlay)))
        }))
    else
        val next = app.enqueue(message)
        copy(app = if next.sizeIs > size then next.drop(1) else next)
}

/** A [[MessagingIO]] for [[Replicated]] state, which regularly publishes its own connections as overlay status,
  * and collects the overlay status of all other replicas in [[status]] (e.g., to draw it with
  * [[ex2026overlaydemo.OverlayNetworkGraph.animate]]).
  * `receive` gets the application part of incoming deltas.
  */
class OverlayReplication[A: JsonValueCodec](val replicaId: ReplicaId, receive: A => Unit) {

  @volatile var status: Status = OverlayStatusProtocol.empty

  val io: MessagingIO.Plumtree[Replicated[A]] = MessagingIO.usingPlumtree[Replicated[A]](
    replicaId,
    payload => {
      status = status.merge(payload.data.overlay)
      payload.data.app.foreach(receive)
    },
    history = ReplicatedHistory[A](size = 1000, app = Queue.empty[Payload[Replicated[A]]]),
  )

  def broadcast(delta: A): Unit = io.broadcast(Replicated(app = Some(delta)))

  /** application deltas known locally */
  def appPayloads: List[A] = io.allPayloads.flatMap(_.data.app)

  private def publishStatus(): Unit = {
    given ReplicaId = replicaId
    val delta       = OverlayStatusProtocol.statusDelta(status, io, Date.now().toLong)
    status = status.merge(delta)
    io.broadcast(Replicated(overlay = delta))
  }

  private val ticker = dom.window.setInterval(() => publishStatus(), 1000)

  def stop(): Unit = {
    dom.window.clearInterval(ticker)
    io.globalAbort.abort()
  }
}
