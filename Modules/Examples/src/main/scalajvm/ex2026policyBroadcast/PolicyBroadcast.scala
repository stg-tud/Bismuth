package ex2026policyBroadcast

import channels.MessagingIO
import channels.connection.Peer
import channels.crypto.{PrivateIdentity, PublicIdentity, Verifier}
import channels.routing.Routing
import channels.routing.Routing.{Effect, Step}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.Lattice

/** Decides which deltas of a replicated state `S` may flow between replicas. Both checks see the current local state,
  * so policies may depend on it (e.g., permissions stored in the state).
  */
trait BroadcastPolicy[S] {

  /** Whether `delta` may be sent to `reader`. */
  def mayRead(state: S, reader: Peer, delta: S): Boolean

  /** Whether `author` may write `delta`, checked for local writes and for received deltas. */
  def mayWrite(state: S, author: PublicIdentity, delta: S): Boolean
}

/** Direct dissemination of signed deltas of `S`, filtered by a [[BroadcastPolicy]].
  *
  * Every replica keeps the history of all signed deltas it accepted. There is no forwarding: local writes are sent to
  * the neighbors that may read them, and when a peer connects it is sent the history, as far as it may read it.
  * A received delta is accepted if its signature is valid and its author may write it, regardless of who sent it.
  * Thus, replicas also learn writes of replicas they never connect to.
  */
object PolicyBroadcast {

  /** `history` are the accepted deltas, newest first. */
  final case class State[S](value: S, history: List[Signed[S]] = Nil, peers: Set[Peer] = Set.empty)

  def messageCodec[S: JsonValueCodec]: JsonValueCodec[Signed[S]] = Signed.codec[S]

  final class Protocol[S: {Lattice, JsonValueCodec}](identity: PrivateIdentity, policy: BroadcastPolicy[S])(using
      Verifier
  ) extends Routing {
    type State = PolicyBroadcast.State[S]
    type Msg   = Signed[S]
    type Out   = S

    private type Result = Step[State, Msg, Out]

    def addPeer(state: State, peer: Peer): Result =
      if state.peers.contains(peer) then Step(state, Nil)
      else
          Step(state.copy(peers = state.peers + peer), state.history.reverse.flatMap(send(state, Set(peer), _)))

    def removePeer(state: State, peer: Peer): Result           = Step(state.copy(peers = state.peers - peer), Nil)
    def timerFired(state: State, timer: Routing.Timer): Result = Step(state, Nil)

    def handleMessage(state: State, from: Peer, signed: Msg): Result =
      if state.history.contains(signed) || !signed.verify || !policy.mayWrite(state.value, signed.author, signed.delta)
      then Step(state, Nil)
      else
          val next = state.copy(value = state.value.merge(signed.delta), history = signed :: state.history)
          Step(next, if next.value == state.value then Nil else List(Effect.Deliver(next.value)))

    /** Local operation: sign and apply `delta`, and send it to all neighbors that may read it.
      * @throws IllegalArgumentException if this replica may not write `delta`
      */
    def write(delta: S)(state: State): Result = {
      require(policy.mayWrite(state.value, identity.getPublic, delta), s"${identity.getPublic} may not write $delta")
      val signed  = Signed(identity, delta)
      val next    = state.copy(value = state.value.merge(delta), history = signed :: state.history)
      Step(next, Effect.Deliver(next.value) :: send(next, next.peers, signed))
    }

    /** Send `signed` to those of `peers` that may read it. */
    private def send(state: State, peers: Set[Peer], signed: Signed[S]): List[Effect[Msg, Out]] =
        val readers = peers.filter(policy.mayRead(state.value, _, signed.delta)).toList
        if readers.isEmpty then Nil else List(Effect.Send(readers, signed))
  }

  extension [S](io: MessagingIO[Protocol[S]]) {

    /** Apply the delta `mutator` computes from the current state. Throws if not permitted. */
    def mutate(mutator: S => S): Unit = io.update(s => io.protocol.write(mutator(s.value))(s))
  }
}
