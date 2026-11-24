package ex2026policyBroadcast

import channels.MessagingIO
import channels.connection.Peer
import channels.routing.Routing
import channels.routing.Routing.{Effect, Step}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Lattice

/** Decides which deltas of a replicated state `S` may flow between replicas. Both checks see the current local state,
  * so policies may depend on it (e.g., permissions stored in the state).
  */
trait BroadcastPolicy[S] {

  /** Whether `delta` may be sent to `reader`. */
  def mayRead(state: S, reader: Peer, delta: S): Boolean

  /** Whether `writer` may write `delta`, checked for local writes (with the local peer) and for received deltas. */
  def mayWrite(state: S, writer: Peer, delta: S): Boolean
}

/** Direct dissemination of the deltas of `S`, filtered by a [[BroadcastPolicy]].
  *
  * Every replica keeps the history of its own deltas. There is no forwarding: local writes are sent to the neighbors
  * that may read them, and when a peer connects it is sent the history, as far as it may read it. As replicas only
  * send their own deltas, the sender of a delta is its writer, and a received delta is only accepted if the sender
  * may write it. Thus, replicas only learn the writes of replicas they are (eventually) connected to.
  */
object PolicyBroadcast {

  /** `history` are the local deltas, newest first. */
  final case class State[S](value: S, history: List[S] = Nil, peers: Set[Peer] = Set.empty)

  def messageCodec[S: JsonValueCodec]: JsonValueCodec[List[S]] = JsonCodecMaker.make

  final class Protocol[S: Lattice](policy: BroadcastPolicy[S]) extends Routing {
    type State = PolicyBroadcast.State[S]
    type Msg   = List[S]
    type Out   = S

    private type Result = Step[State, Msg, Out]

    def addPeer(state: State, peer: Peer): Result =
      if state.peers.contains(peer) then Step(state, Nil)
      else
          val readable = state.history.reverse.filter(policy.mayRead(state.value, peer, _))
          Step(state.copy(peers = state.peers + peer), send(List(peer), readable))

    def removePeer(state: State, peer: Peer): Result           = Step(state.copy(peers = state.peers - peer), Nil)
    def timerFired(state: State, timer: Routing.Timer): Result = Step(state, Nil)

    def handleMessage(state: State, from: Peer, deltas: Msg): Result = {
      val next = deltas.foldLeft(state.value) { (value, delta) =>
        if policy.mayWrite(value, from, delta) then value.merge(delta) else value
      }
      if next == state.value then Step(state, Nil)
      else Step(state.copy(value = next), List(Effect.Deliver(next)))
    }

    /** Local operation: apply `delta`, written by `self`, and send it to all neighbors that may read it.
      * @throws IllegalArgumentException if `self` may not write `delta`
      */
    def write(self: Peer, delta: S)(state: State): Result = {
      require(policy.mayWrite(state.value, self, delta), s"$self may not write $delta")
      val next    = state.copy(value = state.value.merge(delta), history = delta :: state.history)
      val readers = next.peers.filter(policy.mayRead(next.value, _, delta)).toList
      Step(next, Effect.Deliver(next.value) :: send(readers, List(delta)))
    }

    private def send(peers: List[Peer], deltas: List[S]): List[Effect[Msg, Out]] =
      if peers.isEmpty || deltas.isEmpty then Nil else List(Effect.Send(peers, deltas))
  }

  extension [S](io: MessagingIO[Protocol[S]]) {

    /** Apply the delta `mutator` computes from the current state. Throws if not permitted. */
    def mutate(mutator: S => S): Unit = io.update(s => io.protocol.write(io.selfPeer, mutator(s.value))(s))
  }
}
