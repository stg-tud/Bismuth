package replication.overlay

import channels.{Connection, ConnectionDescriptor, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.overlay.OverlayController.OverlayMessage.*
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

/** Immutable membership state machine for HyParView.
  *
  * Deviations from the paper:
  * - peers are only inserted into active/passive state if `canConnectTo(peer)` is true.
  *   The paper assumes any advertised peer can be contacted. In this implementation, some connection
  *   kinds are not supported by all peers, so shuffle/join/neighbor handling filters out undialable peers.
  *   This particularly affects passive-view learning from forwarded joins and shuffles.
  *
  * Additionally, active-view entries may carry transport connection objects.
  * These are managed purely as opaque handles for routing outgoing messages; protocol decisions do not depend on them.
  * Newly established but not-yet-identified connections are kept separately until a received protocol message reveals
  * the remote peer identity.
  */
object HyParViewStateMachine {

  case class HyParViewConfig(
      activeViewSize: Int,
      passiveViewSize: Int,
      activeRandomWalkLength: Int,
      passiveRandomWalkLength: Int,
      shuffleRandomWalkLength: Int,
      shuffleActiveSample: Int,
      shufflePassiveSample: Int,
  )

  object HyParViewConfig {
    val default: HyParViewConfig = fromEstimatedNetworkSize(10_000)

    def fromEstimatedNetworkSize(estimatedNetworkSize: Int): HyParViewConfig = {
      val n       = math.max(2, estimatedNetworkSize)
      val active  = math.max(3, math.ceil(math.log10(n.toDouble)).toInt + 1)
      val passive = active * 6
      HyParViewConfig(
        active,
        passive,
        active + 1,
        math.max(1, (active + 1) / 2),
        active,
        math.min(3, math.max(1, active - 1)),
        4
      )
    }
  }

  /** Active-view entry enriched with the currently known live connection. */
  final case class ActivePeer(peer: PeerConnectInfo, connection: Connection)

  /** Pending outbound connection attempts tracked only by peer identity. */
  final case class PendingConnection(peer: PeerConnectInfo)

  final case class Result(state: HyParViewStateMachine, actions: List[OverlayAction])

  def empty(
      self: PeerConnectInfo,
      config: HyParViewConfig,
      randomIndex: (Int, Int) => Int,
      canConnectTo: PeerConnectInfo => Boolean,
  ): HyParViewStateMachine =
    HyParViewStateMachine(
      self = self,
      config = config,
      known = Map(self.uid -> self),
      active = Vector.empty,
      pendingShuffleSamples = Map.empty,
      pendingConnections = Vector.empty,
      randomIndex = randomIndex,
      canConnectTo = canConnectTo,
    )
}

final case class HyParViewStateMachine(
    self: PeerConnectInfo,
    config: HyParViewConfig,
    known: Map[Uid, PeerConnectInfo],
    active: Vector[HyParViewStateMachine.ActivePeer],
    pendingShuffleSamples: Map[Uid, Set[PeerConnectInfo]],
    pendingConnections: Vector[HyParViewStateMachine.PendingConnection],
    randomIndex: (Int, Int) => Int,
    canConnectTo: PeerConnectInfo => Boolean,
) extends OverlayController {
  import HyParViewStateMachine.*

  override def addSelfConnectionDescriptor(descriptor: ConnectionDescriptor): OverlayController = {
    val updatedSelf = self.copy(channelConnectors = self.channelConnectors + descriptor)
    copy(self = updatedSelf, known = known.updated(updatedSelf.uid, updatedSelf))
  }

  override def selfConnectionDescriptors: Set[ConnectionDescriptor] = self.channelConnectors

  def activeView: Set[Uid]               = active.iterator.map(_.peer.uid).toSet
  def activePeers: Set[PeerConnectInfo]  = active.iterator.map(_.peer).toSet
  def passivePeers: Set[PeerConnectInfo] =
    known.valuesIterator.filter(peer =>
      peer.uid != self.uid && !activeView.contains(peer.uid) && canConnectTo(peer)
    ).toSet
  def passiveView: Set[Uid] = passivePeers.iterator.map(_.uid).toSet

  /** Paper passive-view maintenance step: initiate one shuffle through a random active peer. */
  def shuffleTick(): Result =
    choose(active) match
        case None         => Result(this, Nil)
        case Some(target) =>
          val sample = Set(self) ++ randomSubset(
            active.iterator.map(_.peer).toSet,
            config.shuffleActiveSample
          ) ++ randomSubset(passivePeers, config.shufflePassiveSample)
          Result(
            copy(pendingShuffleSamples = pendingShuffleSamples.updated(target.peer.uid, sample)),
            List(OverlayAction.Send(
              target.connection,
              Shuffle(self, sample, config.shuffleRandomWalkLength, self.uid)
            ))
          )

  /** Overlay lifecycle tick: retry promotion first, then run passive-view shuffle maintenance. */
  override def tick(): (OverlayController, List[OverlayAction]) = {
    val Result(shuffled, a2) = shuffleTick()
    (shuffled, a2)
  }

  override def discoverPassive(peers: Set[PeerConnectInfo]): (OverlayController, List[OverlayAction]) = {
    val next = peers.foldLeft(this)((state, peer) => state.rememberPeer(peer))
    (next, Nil)
  }

  override def bootstrapVia(contact: ConnectionDescriptor): (OverlayController, List[OverlayAction]) =
    (
      this,
      List(OverlayAction.SendJoin(Set(contact), self.uid, Join(self)))
    )

  /** Handle one HyParView protocol message according to the paper's join, neighbor, disconnect, and shuffle rules. */
  override def receiveActions(message: OverlayMessage, conn: Connection): (OverlayController, List[OverlayAction]) = {
    val result = receive(message, conn)
    (result.state, result.actions)
  }

  def receive(message: OverlayMessage, from: Connection): Result = {
    message match
        case Join(newNode) =>
          // Paper join contact handling: the contact immediately activates the newcomer on the incoming
          // connection and forwards `ForwardJoin` along its current active view to seed additional peers.
          val remembered      = rememberPeer(newNode)
          val existingTargets = remembered.active.filterNot(_.peer.uid == newNode.uid)
          val forwardActions  = existingTargets.map(target =>
            OverlayAction.Send(
              target.connection,
              ForwardJoin(newNode, config.activeRandomWalkLength, self.uid)
            )
          ).toList
          Result(
            remembered,
            OverlayAction.Send(
              from,
              OverlayMessage.Neighbor(self, highPriority = remembered.active.isEmpty)
            ) :: forwardActions
          )

        case ForwardJoin(newNode, ttl, sender) =>
          // Paper forwarded-join random walk: intermediate hops remember the newcomer, optionally add it
          // to the passive view at PRWL, and either forward to another active peer or terminate by
          // initiating a direct Neighbor handshake with the newcomer.
          def sendJoin(nextState: HyParViewStateMachine) = {
            Result(
              nextState,
              List(
                OverlayAction.SendJoin(
                  newNode.channelConnectors,
                  newNode.uid,
                  Neighbor(self, highPriority = nextState.active.isEmpty)
                )
              )
            )
          }

          if newNode.uid == self.uid then Result(this, Nil)
          else if ttl == 0 || active.size <= 1 then
              val remembered = rememberPeer(newNode)
              sendJoin(remembered)
          else {
            val withPassive =
              if ttl == config.passiveRandomWalkLength && canConnectTo(newNode)
              then rememberPeer(newNode)
              else this

            val nextHop = choose(withPassive.active.filterNot(_.peer.uid == sender))
            nextHop match
                case Some(target) =>
                  Result(
                    withPassive,
                    List(OverlayAction.Send(target.connection, ForwardJoin(newNode, ttl - 1, self.uid)))
                  )
                case None =>
                  sendJoin(withPassive)
          }

        case Neighbor(fromPeer, highPriority) =>
          // Paper active-view repair handshake: high priority always accepted, low priority only if there is a free active slot.
          val afterRemember = rememberPeer(fromPeer)
          val accepted      = highPriority || afterRemember.active.size < config.activeViewSize
          if accepted then {
            val (next, actions) = afterRemember.addActive(fromPeer, from)
            Result(next, actions ::: List(OverlayAction.Send(from, NeighborReply(self.uid, accepted = true))))
          } else Result(afterRemember, List(OverlayAction.Send(from, NeighborReply(self.uid, accepted = false))))

        case NeighborReply(fromPeer, accepted) =>
          // Paper promotion result: accepted peers move to active; rejected peers stay passive.
          val base            = clearPendingPeer(fromPeer)
          val (next, actions) =
            if accepted then known.get(fromPeer).map(peer => base.addActive(peer, from)).getOrElse((base, Nil))
            else (base, Nil)
          Result(next, actions)

        case Shuffle(origin, sample, ttl, sender) =>
          // Paper shuffle walk: intermediate hops only forward; the endpoint replies and merges the received sample.
          val remembered = sample.foldLeft(rememberPeer(origin))((state, peer) => state.rememberPeer(peer))
          if ttl > 0 && remembered.active.size > 1 then
              choose(remembered.active.filterNot(_.peer.uid == sender)) match
                  case Some(target) => Result(
                      remembered,
                      List(OverlayAction.Send(target.connection, Shuffle(origin, sample, ttl - 1, self.uid)))
                    )
                  case None => remembered.acceptShuffle(origin, sample)
          else remembered.acceptShuffle(origin, sample)

        case ShuffleReply(fromPeer, sample) =>
          // Paper shuffle completion at the initiator: merge the reply sample, preferring eviction of entries previously sent to the peer.
          val remembered = sample.foldLeft(this)((state, peer) => state.rememberPeer(peer))
          val next       =
            remembered.mergeShuffleSample(sample, remembered.pendingShuffleSamples.getOrElse(fromPeer, Set.empty)).copy(
              pendingShuffleSamples = remembered.pendingShuffleSamples.removed(fromPeer)
            )
          Result(next, Nil)

  }

  override def removeConnection(
      conn: Connection,
      connectInfo: Option[channels.ConnectionDescriptor] = None
  ): (OverlayController, List[OverlayAction]) =
    active.find(_.connection == conn) match
        case Some(activePeer) =>
          val next                      = copy(active = active.filterNot(_.peer.uid == activePeer.peer.uid))
          val Result(promoted, actions) =
            next.withPromotionIfNeeded(List(OverlayAction.ActiveConnectionRemoved(activePeer.peer.uid)))
          (promoted, actions)
        case None =>
          connectInfo.flatMap(info => known.valuesIterator.find(_.channelConnectors.contains(info))) match
              case Some(peer) =>
                (forgetPeer(peer.uid), Nil)
              case _ =>
                (this, Nil)

  override def connectionFor(peer: Uid): Option[Connection] =
    active.find(_.peer.uid == peer).map(_.connection)

  private def rememberPeer(peer: PeerConnectInfo): HyParViewStateMachine =
    if peer.uid == self.uid then this else copy(known = known.updated(peer.uid, peer))

  private def addActive(peer: PeerConnectInfo, conn: Connection): (HyParViewStateMachine, List[OverlayAction]) =
    if peer.uid == self.uid || active.exists(_.peer.uid == peer.uid) || !canConnectTo(peer) then (this, Nil)
    else {
      val (evictedState, evictedActions) =
        if active.size >= config.activeViewSize then dropRandomActive() else (this, Nil)
      val next = evictedState.copy(
        active = evictedState.active.filterNot(_.peer.uid == peer.uid) :+ ActivePeer(peer, conn),
        known = evictedState.known.updated(peer.uid, peer)
      ).clearPendingPeer(peer.uid)
      (next, evictedActions ::: List(OverlayAction.ActiveConnectionAdded(peer.uid)))
    }

  private def dropRandomActive(): (HyParViewStateMachine, List[OverlayAction]) =
    choose(active) match
        case None          => (this, Nil)
        case Some(dropped) =>
          val next = copy(active = active.filterNot(_.peer.uid == dropped.peer.uid))
          (next, List(OverlayAction.Disconnect(dropped.connection)))

  private def acceptShuffle(origin: PeerConnectInfo, incomingSample: Set[PeerConnectInfo]): Result = {
    val replySample = randomSubset(passivePeers, incomingSample.size)
    val next        = mergeShuffleSample(incomingSample, replySample)
    Result(next, next.connectionFor(origin.uid).toList.map(OverlayAction.Send(_, ShuffleReply(self.uid, replySample))))
  }

  private def mergeShuffleSample(
      incomingSample: Set[PeerConnectInfo],
      sentToPeer: Set[PeerConnectInfo]
  ): HyParViewStateMachine = {
    val preferredVictims = sentToPeer.map(_.uid)
    incomingSample.foldLeft(this) { (state, peer) =>
      if peer.uid == self.uid || !state.canConnectTo(peer) || state.active.exists(
            _.peer.uid == peer.uid
          ) || state.passiveView.contains(peer.uid)
      then state
      else {
        val afterRemember = state.rememberPeer(peer)
        afterRemember.trimKnownPassiveIfNeeded(preferredVictims)
      }
    }
  }

  private def clearPendingPeer(peer: Uid): HyParViewStateMachine =
    copy(pendingConnections = pendingConnections.filterNot(_.peer.uid == peer))

  private def forgetPeer(peer: Uid): HyParViewStateMachine =
    copy(known = known.removed(peer)).clearPendingPeer(peer)

  private def trimKnownPassiveIfNeeded(preferredVictims: Set[Uid]): HyParViewStateMachine = {
    val passive = passivePeers.toIndexedSeq
    if passive.size <= config.passiveViewSize then this
    else
        passive.find(p => preferredVictims.contains(p.uid)).orElse(choose(passive)) match
            case Some(victim) => forgetPeer(victim.uid)
            case None         => this
  }

  private def withPromotionIfNeeded(existingActions: List[OverlayAction]): Result =
    if active.size >= config.activeViewSize then Result(this, existingActions)
    else
        choose(passivePeers.filterNot(peer => pendingConnections.exists(_.peer.uid == peer.uid)).toIndexedSeq) match
            case Some(candidate) =>
              Result(
                copy(pendingConnections = pendingConnections :+ PendingConnection(candidate)),
                existingActions :+ OverlayAction.SendJoin(
                  candidate.channelConnectors,
                  candidate.uid,
                  Neighbor(self, highPriority = active.isEmpty)
                )
              )
            case None => Result(this, existingActions)

  private def choose[A](values: Seq[A]): Option[A] =
    Option.when(values.nonEmpty)(values(randomIndex(0, values.size)))

  private def randomSubset[A](values: Set[A], maxSize: Int): Set[A] = {
    val vec   = values.toVector
    val count = math.min(maxSize, vec.size)
    (0 until count).foldLeft((Vector.empty[A], vec)) { case ((chosen, remaining), _) =>
      val idx = randomIndex(0, remaining.size)
      (chosen :+ remaining(idx), remaining.patch(idx, Nil, 1))
    }._1.toSet
  }
}
