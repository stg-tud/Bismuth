package replication

import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event.Disseminate
import replication.ProtocolMessage.*

object PlumtreeBroadcast {

  final case class Peer(uid: Uid)

  enum PeerRole {
    case Eager, Lazy
  }

  enum Event[+State] {
    case Deliver(payload: Payload[State])
    case Disseminate(peers: List[Peer], payload: ProtocolMessage[State])
  }

  final case class Result[State](
      state: PlumtreeBroadcast[State],
      events: List[Event[State]],
  )
}

final case class PlumtreeBroadcast[State](
    self: Uid,
    localContext: Dots = Dots.empty,
    deltaStorage: DeltaStorage[State] = NoHistory(),
    peerRoles: Map[PlumtreeBroadcast.Peer, PlumtreeBroadcast.PeerRole] = Map.empty,
) {
  import PlumtreeBroadcast.*

  def eagerPeers: Set[Uid] = peerRoles.collect { case (Peer(uid), PeerRole.Eager) => uid }.toSet

  def addPeer(peer: Peer): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(peerRoles = peerRoles.updated(peer, PeerRole.Eager))
    Result(next, List(Disseminate(peer :: Nil, Graft(self, localContext))))
  }

  def removePeer(peer: Peer): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.removed(peer))

  def broadcast(payload: Payload[State], except: Set[Peer] = Set.empty): Result[State] = {
    val next: PlumtreeBroadcast[State] = copy(
      localContext = localContext.merge(payload.dots),
      deltaStorage = deltaStorage.remember(payload),
    )
    next.disseminate(payload, except)
  }

  def handleMessage(from: Peer, message: ProtocolMessage[State]): Result[State] =
    message match
      case Ping(time) =>
        Result(this, List(Disseminate(from :: Nil, Pong(time))))

      case Pong(_) =>
        Result(this, Nil)

      case Prune(_) =>
        Result(withRole(from, PeerRole.Lazy), Nil)

      case IHave(_, knows) =>
        if !(knows <= localContext) then Result(this, List(Disseminate(from :: Nil, Graft(self, localContext))))
        else Result(this, Nil)

      case Graft(_, knows) =>
        val next = withRole(from, PeerRole.Eager)
        val relevant = next.deltaStorage.getHistory.filterNot(payload => payload.dots <= knows)
        if relevant.isEmpty && !(next.localContext <= knows) then
          println(s"plumtree graft unsatisfied from=$from local=${next.localContext} knows=$knows storage=${next.deltaStorage.getClass.getSimpleName}")
        Result(next, relevant.map(payload => Disseminate(from :: Nil, payload)))

      case payload @ Payload(context, _, _) =>
        if context <= localContext then
          Result(withRole(from, PeerRole.Lazy), List(Disseminate(from :: Nil, Prune(self))))
        else
          val next: PlumtreeBroadcast[State] = copy(
            localContext = localContext.merge(context),
            deltaStorage = deltaStorage.remember(payload),
          ).withRole(from, PeerRole.Eager)
          val forwarded = next.disseminate(payload, except = Set(from))
          forwarded.copy(events = Event.Deliver(payload) :: forwarded.events)

  private def withRole(peer: Peer, role: PeerRole): PlumtreeBroadcast[State] =
    copy(peerRoles = peerRoles.updated(peer, role))

  private def disseminate(payload: Payload[State], except: Set[Peer]): Result[State] = {
    val recipients = peerRoles.keySet -- except
    val eager = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Eager)).toList
    val lazyPeers = recipients.filter(peer => peerRoles.get(peer).contains(PeerRole.Lazy)).toList

    val eagerSend = Option.when(eager.nonEmpty)(Disseminate(eager, payload: ProtocolMessage[State]))
    val lazySend = Option.when(lazyPeers.nonEmpty)(Disseminate(lazyPeers, IHave(self, localContext): ProtocolMessage[State]))

    Result(this, List(eagerSend, lazySend).flatten)
  }
}
