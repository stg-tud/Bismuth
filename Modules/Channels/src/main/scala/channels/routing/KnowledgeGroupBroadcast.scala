package channels.broadcast

import channels.JsoniterCodecs.given
import channels.MessagingIO
import channels.broadcast.Routing.{Effect, Step}
import channels.connection.Peer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, ReplicaId, Uid}
import rdts.time.{Dot, Dots}

case class KnowledgeGroup[A](ids: Set[Uid], path: A => A, enabled: A => Boolean) {
  def matches(delta: A)(using Bottom[A]): Boolean = {
    val pathDelta = path(delta)
    enabled(delta) && !Bottom[A].isEmpty(pathDelta)
  }
}

case class PrdtSystem[A](knowledgeGroups: Set[KnowledgeGroup[A]]) {
  def groupsFor(uid: Uid): Set[KnowledgeGroup[A]] =
    knowledgeGroups.filter(_.ids.contains(uid))

  def matches(delta: A, receiver: Uid)(using Bottom[A], ReplicaId): Boolean =
      val localReplicaId = summon[ReplicaId].uid
      knowledgeGroups.exists(g => g.ids.contains(localReplicaId) && g.ids.contains(receiver) && g.matches(delta))

  def matches(delta: A, receivers: Set[Uid])(using Bottom[A], ReplicaId): Boolean =
      val localReplicaId = summon[ReplicaId].uid
      knowledgeGroups.exists(g => g.ids.contains(localReplicaId) && g.ids == receivers && g.matches(delta))

}

/** Dissemination restricted by [[KnowledgeGroup]]s, assuming a full mesh overlay.
  *
  * A delta is sent directly to every neighbor that shares a matching knowledge group with the sender.
  * There is no relaying within a group (full mesh), only forwarding across groups:
  * a replica receiving an unseen delta forwards it unchanged (same dots) to its matching neighbors,
  * except members of the groups it was received through (the matching groups shared with the sender,
  * which the sender already covered). Each replica forwards each delta at most once.
  */
object KnowledgeGroupBroadcast {

  final case class Message[+A](dots: Dots, data: A)

  /** What the application receives. */
  final case class Delivery[+A](dots: Dots, data: A)

  final case class State(context: Dots = Dots.empty, peers: Set[Peer] = Set.empty)

  def messageCodec[A: JsonValueCodec]: JsonValueCodec[Message[A]] = JsonCodecMaker.make

  final class Protocol[A](system: PrdtSystem[A])(using Bottom[A], ReplicaId) extends Routing {
    type State = KnowledgeGroupBroadcast.State
    type Msg   = Message[A]
    type Out   = Delivery[A]

    private type Result = Step[State, Msg, Out]

    def addPeer(state: State, peer: Peer): Result              = Step(state.copy(peers = state.peers + peer), Nil)
    def removePeer(state: State, peer: Peer): Result           = Step(state.copy(peers = state.peers - peer), Nil)
    def timerFired(state: State, timer: Routing.Timer): Result = Step(state, Nil)

    def handleMessage(state: State, from: Peer, message: Msg): Result =
      if message.dots <= state.context then Step(state, Nil)
      else
          val next    = state.copy(context = state.context.merge(message.dots))
          val covered = system.knowledgeGroups.iterator
            .filter(g => g.ids.contains(from.uid) && g.ids.contains(self) && g.matches(message.data))
            .flatMap(_.ids).toSet + from.uid
          val fwd = disseminate(next, message.dots, message.data, covered)
          Step(next, Effect.Deliver(Delivery(message.dots, message.data)) +: fwd)

    /** Local operation: assign a new dot to `delta` and send it to all matching neighbors. */
    def publish(self: Uid, delta: A)(state: State): (res: Result, dot: Dot) = {
      val dot  = state.context.nextDot(self)
      val dots = Dots.single(dot)
      val next = state.copy(context = state.context.merge(dots))
      (Step(next, disseminate(next, dots, delta, Set.empty)), dot)
    }

    private val self: Uid = summon[ReplicaId].uid

    /** Send to neighbors sharing a matching group with us, except those already `covered`. */
    private def disseminate(state: State, dots: Dots, data: A, covered: Set[Uid]): List[Effect[Msg, Out]] = {
      val targets = state.peers.filter(p => !covered.contains(p.uid) && system.matches(data, p.uid)).toList
      if targets.isEmpty then Nil
      else List(Effect.Send(targets, Message(dots, data)))
    }
  }

  extension [A](io: MessagingIO[Protocol[A]]) {
    def publish(delta: A): Unit = io.update(s => io.protocol.publish(io.selfPeer.uid, delta)(s).res)
  }
}
