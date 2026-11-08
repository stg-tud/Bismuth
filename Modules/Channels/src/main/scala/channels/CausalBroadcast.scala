package channels

import channels.PlumtreeIO.executeImmediately
import channels.broadcast.PlumtreeMessage.Payload
import channels.broadcast.{BroadcastProtocol, BroadcastTimer, Effect, PlumtreeBroadcast, PlumtreeMessage, Step}
import channels.connection.{Abort, ChannelResolver, Peer}
import channels.experiments.Aead
import channels.overlay.OverlayController
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import rdts.base.{ReplicaId, Uid}
import rdts.time.{Causal, CausalBuffer, Dots}

import scala.concurrent.ExecutionContext

/** A [[MessagingIO]] running [[CausalPlumtree]]: delivers plain values `A` in causal order. */
type CausalBroadcast[A] = MessagingIO.of[CausalPlumtree.Protocol[A]]

/** Plumtree extended with causal delivery. Remote values are delivered in causal order and at most once.
  * Local values are not delivered: the caller applies them before calling [[CausalPlumtree.broadcastLocal]].
  * Missing predecessors are not requested separately; plumtree is responsible for (best effort) reliable delivery.
  * A [[MergingHistory]] can be used if `A` is a lattice (see [[Causal.lattice]]).
  */
final case class CausalPlumtree[A](
    plumtree: PlumtreeBroadcast[Causal[A]] =
      PlumtreeBroadcast[Causal[A]](deltaStorage = DiscardingHistory[Causal[A]](size = 1000)),
    buffer: CausalBuffer[A] = CausalBuffer[A](),
) {
  import CausalPlumtree.Result

  /** Dots of all local and delivered remote values. */
  def applied: Dots = buffer.applied

  /** Dots that were received, but are still waiting for some of their predecessors. */
  def waiting: Dots = buffer.waiting

  /** Runs a plumtree transition, passing its delivered payloads through the causal buffer. */
  def lift(step: PlumtreeBroadcast.Result[Causal[A]]): Result[A] =
    step.events.foldLeft[Result[A]](Step(copy(plumtree = step.state), Vector.empty)) {
      case (Step(state, acc), Effect.Deliver(payload)) =>
        val delivered = state.deliver(payload)
        Step(delivered.state, acc ++ delivered.events)
      case (Step(state, acc), Effect.Send(peers, message)) => Step(state, acc :+ Effect.Send(peers, message))
      case (Step(state, acc), Effect.StartTimer(timer))    => Step(state, acc :+ Effect.StartTimer(timer))
    }

  /** Feed a payload delivered by plumtree into the causal buffer. */
  def deliver(payload: Payload[Causal[A]]): Result[A] = {
    val (next, ready) = buffer.receive(payload.dots, payload.data)
    Step(copy(buffer = next), ready.map(Effect.Deliver(_)))
  }
}

object CausalPlumtree {

  type Result[A] = Step[CausalPlumtree[A], PlumtreeMessage[Causal[A]], A]

  final class Protocol[A] extends BroadcastProtocol {
    type State = CausalPlumtree[A]
    type Msg   = PlumtreeMessage[Causal[A]]
    type Out   = A

    def addPeer(state: State, peer: Peer): Result[A]    = state.lift(state.plumtree.addPeer(peer))
    def removePeer(state: State, peer: Peer): Result[A] = state.lift(state.plumtree.removePeer(peer))
    def handleMessage(state: State, from: Peer, message: Msg): Result[A] =
      state.lift(state.plumtree.handleMessage(from, message))
    def timerFired(state: State, timer: BroadcastTimer): Result[A] =
      state.lift(state.plumtree.timerFired(timer))
  }

  given protocol[A]: Protocol[A] = Protocol[A]()

  /** Local operation: wrap `value` with everything applied so far, disseminate it, and mark it as applied.
    * Values released by marking it applied are delivered (they were only waiting on local values).
    */
  def broadcastLocal[A](self: Uid, value: A)(state: CausalPlumtree[A]): (Result[A]) = {
    val (sent, dot)     = PlumtreeBroadcast.broadcastLocal(self, Causal(state.buffer.applied, value))(state.plumtree)
    val lifted          = state.lift(sent)
    val (buffer, ready) = lifted.state.buffer.markApplied(Dots.single(dot))
    Step(lifted.state.copy(buffer = buffer), lifted.events ++ ready.map(Effect.Deliver(_)))
  }

  /** Application API of a [[CausalBroadcast]]. `deliver` may call [[broadcast]] again. */
  extension [A](io: CausalBroadcast[A]) {

    /** Disseminate a value that was already applied locally. Its predecessors are everything applied so far. */
    def broadcast(value: A): Unit = io.update(broadcastLocal(io.selfPeer.uid, value)): Unit

  }
}

object CausalBroadcast {

  /** See [[CausalPlumtree]]. */
  def apply[A](
      replicaId: ReplicaId,
      deliver: A => Unit,
      overlay: Option[OverlayController] = None,
      resolver: ChannelResolver = ChannelResolver.disconnected,
      sendingActor: ExecutionContext = executeImmediately,
      globalAbort: Abort = Abort(),
      initial: CausalPlumtree[A] = CausalPlumtree[A](),
      aead: Aead = Aead.identity,
      timers: Timers = Timers.Manual(),
      timerDelays: TimerDelays = TimerDelays(),
  )(using JsonValueCodec[A]): CausalBroadcast[A] =
    MessagingIO[CausalPlumtree.Protocol[A]](using CausalPlumtree.protocol[A])(
      replicaId,
      initial,
      deliver,
      overlay = overlay,
      resolver = resolver,
      sendingActor = sendingActor,
      globalAbort = globalAbort,
      aead = aead,
      timers = timers,
      overlayDelays = timerDelays,
      protocolDelays = timerDelays.apply,
    )(using PlumtreeBroadcast.messageCodec[Causal[A]])

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
