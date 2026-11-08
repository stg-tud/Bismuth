package channels

import channels.BroadcastIO.executeImmediately
import channels.broadcast.PlumtreeMessage.Payload
import channels.broadcast.BroadcastProtocol
import channels.connection.{Abort, ChannelResolver}
import channels.experiments.Aead
import channels.overlay.OverlayController
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import rdts.base.ReplicaId
import rdts.time.{Causal, CausalBuffer, Dot, Dots}

import scala.concurrent.ExecutionContext

object CausalBroadcast {

  /** Causal broadcast on top of a [[BroadcastIO]]. Remote values are passed to `deliver` in causal order and at most
    * once. Local values are not delivered: the caller applies them before calling [[CausalBroadcast.broadcast]].
    * Missing predecessors are not requested separately; the broadcast layer is responsible for (best effort)
    * reliable delivery. A [[MergingHistory]] can be used if `A` is a lattice (see [[Causal.lattice]]).
    */
  def apply[A](
      replicaId: ReplicaId,
      deliver: A => Unit,
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = executeImmediately,
      globalAbort: Abort = Abort(),
      broadcast: Option[BroadcastProtocol[Causal[A]]] = None,
      aead: Aead = Aead.identity,
      timers: Timers = Timers.Manual(),
      timerDelays: TimerDelays = TimerDelays(),
  )(using JsonValueCodec[A]): CausalBroadcast[A] =
    val delivery = CausalDelivery[A](deliver)
    new CausalBroadcast[A](
      delivery,
      BroadcastIO[Causal[A]](
        replicaId,
        delivery.receive,
        overlay = overlay,
        resolver = resolver,
        sendingActor = sendingActor,
        globalAbort = globalAbort,
        broadcast = broadcast,
        aead = aead,
        timers = timers,
        timerDelays = timerDelays,
      ))

  /** Encodes `Causal(predecessors, value)` as `[predecessors, value]`. */
  given causalCodec[A](using codec: JsonValueCodec[A]): JsonValueCodec[Causal[A]] = new JsonValueCodec[Causal[A]] {
    private val dotsCodec = JsoniterCodecs.CausalContextCodec

    override def decodeValue(in: JsonReader, default: Causal[A]): Causal[A] = {
      if !in.isNextToken('[') then in.decodeError("expected [")
      val predecessors = dotsCodec.decodeValue(in, dotsCodec.nullValue)
      if !in.isNextToken(',') then in.commaError()
      val value = codec.decodeValue(in, codec.nullValue)
      if !in.isNextToken(']') then in.arrayEndError()
      Causal(predecessors, value)
    }

    override def encodeValue(x: Causal[A], out: JsonWriter): Unit = {
      out.writeArrayStart()
      dotsCodec.encodeValue(x.predecessors, out)
      codec.encodeValue(x.value, out)
      out.writeArrayEnd()
    }

    override def nullValue: Causal[A] = null.asInstanceOf[Causal[A]]
  }
}

/** Receiver-side state of a [[CausalBroadcast]]: a [[CausalBuffer]] guarded by a lock.
  *
  * Concurrency: the buffer is updated under `lock`; `deliver` is called after the lock is released, in causal order.
  */
final class CausalDelivery[A](deliver: A => Unit) {

  private val lock: AnyRef            = new {}
  private var buffer: CausalBuffer[A] = CausalBuffer()

  def applied: Dots = lock.synchronized(buffer.applied)
  def waiting: Dots = lock.synchronized(buffer.waiting)

  /** Feed a payload as delivered by the broadcast layer. */
  def receive(payload: Payload[Causal[A]]): Unit =
    lock.synchronized(update(_.receive(payload.dots, payload.data))).foreach(deliver)

  /** Wraps a local value with its predecessors, sends it with `send`, and marks the assigned dot as applied. */
  def broadcast(value: A, send: Causal[A] => Option[Dot]): Option[Dot] = {
    val (assigned, ready) = lock.synchronized {
      val assigned = send(Causal(buffer.applied, value))
      (assigned, assigned.fold(Nil)(dot => update(_.markApplied(Dots.single(dot)))))
    }
    ready.foreach(deliver)
    assigned
  }

  private def update(f: CausalBuffer[A] => (CausalBuffer[A], List[A])): List[A] = {
    val (next, ready) = f(buffer)
    buffer = next
    ready
  }
}

/** Optional causal delivery layer over a [[BroadcastIO]]; see [[CausalBroadcast.apply]]. `deliver` may call
  * [[broadcast]] again.
  */
final class CausalBroadcast[A](val delivery: CausalDelivery[A], val io: BroadcastIO[Causal[A]]) {

  /** Dots of all local and delivered remote values. */
  def applied: Dots = delivery.applied

  /** Dots that were received, but are still waiting for some of their predecessors. */
  def waiting: Dots = delivery.waiting

  /** Disseminate a value that was already applied locally. Its predecessors are everything applied so far. */
  def broadcast(value: A): Option[Dot] = delivery.broadcast(value, io.broadcast)
}
