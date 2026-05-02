package replication.overlay

import channels.ChannelConnectDescriptor
import rdts.base.Uid
import replication.overlay.HyParViewMultiplexed.PeerRef
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.overlay.HyParViewUnified.HyParViewMessage
import replication.overlay.HyParViewUnified.HyParViewMessage.*

/** Immutable membership state machine for HyParView.
  *
  * Deviations from the paper:
  * - peers are only inserted into active/passive state if `canConnectTo(peer)` is true.
  *   The paper assumes any advertised peer can be contacted. In this implementation, some connection
  *   kinds are not supported by all peers, so shuffle/join/neighbor handling filters out undialable peers.
  *   This particularly affects passive-view learning from forwarded joins and shuffles.
  */
object HyParViewStateMachine {

  enum Action {
    case Send(to: PeerRef, message: HyParViewMessage)
    case SendJoin(details: Set[ChannelConnectDescriptor], message: HyParViewMessage)
  }

  final case class Result(state: HyParViewStateMachine, actions: List[Action])

  def empty(
      self: PeerRef,
      config: HyParViewConfig,
      randomIndex: (Int, Int) => Int,
      canConnectTo: PeerRef => Boolean,
  ): HyParViewStateMachine =
    HyParViewStateMachine(
      self = self,
      config = config,
      known = Map(self.uid -> self),
      active = Vector.empty,
      passive = Vector.empty,
      pendingPromotions = Set.empty,
      pendingShuffleSamples = Map.empty,
      randomIndex = randomIndex,
      canConnectTo = canConnectTo,
    )
}

final case class HyParViewStateMachine(
    self: PeerRef,
    config: HyParViewConfig,
    known: Map[Uid, PeerRef],
    active: Vector[PeerRef],
    passive: Vector[PeerRef],
    pendingPromotions: Set[Uid],
    pendingShuffleSamples: Map[Uid, Set[PeerRef]],
    randomIndex: (Int, Int) => Int,
    canConnectTo: PeerRef => Boolean,
) {
  import HyParViewStateMachine.*

  def activeView: Set[Uid]      = active.iterator.map(_.uid).toSet
  def passiveView: Set[Uid]     = passive.iterator.map(_.uid).toSet
  def activePeers: Set[PeerRef] = active.toSet
  def passivePeers: Set[PeerRef] = passive.toSet
  def knownPeer(uid: Uid): Option[PeerRef] = known.get(uid)

  def initiateJoin(details: Set[ChannelConnectDescriptor]): Result =
    Result(this, List(Action.SendJoin(details, Join(self))))

  def shuffleTick(): Result =
    if active.isEmpty then Result(this, Nil)
    else
      val target = choose(active)
      val sample = Set(self) ++ randomSubset(active.toSet, config.shuffleActiveSample) ++ randomSubset(passive.toSet, config.shufflePassiveSample)
      Result(copy(pendingShuffleSamples = pendingShuffleSamples.updated(target.uid, sample)), List(Action.Send(target, Shuffle(self, sample, config.shuffleRandomWalkLength, self.uid))))

  def discoverPeers(peers: Set[PeerRef]): Result = {
    val next = peers.foldLeft(this)((state, peer) => state.rememberPeer(peer).addPassiveIfEligible(peer))
    next.withPromotionIfNeeded(Nil)
  }

  def peerLost(peer: Uid): Result = {
    val next = copy(
      known = known.removed(peer).updated(self.uid, self),
      active = active.filterNot(_.uid == peer),
      passive = passive.filterNot(_.uid == peer),
      pendingPromotions = pendingPromotions - peer,
      pendingShuffleSamples = pendingShuffleSamples.removed(peer),
    )
    next.withPromotionIfNeeded(Nil)
  }

  def reset: HyParViewStateMachine = HyParViewStateMachine.empty(self, config, randomIndex, canConnectTo)

  def receive(message: HyParViewMessage): Result =
    message match
      case Join(newNode) =>
        val afterRemember = rememberPeer(newNode)
        val (afterActive, activeActions) = afterRemember.addActive(newNode)
        val forwardActions = afterActive.active.filterNot(_.uid == newNode.uid).map(peer => Action.Send(peer, ForwardJoin(newNode, config.activeRandomWalkLength, self.uid))).toList
        Result(afterActive, activeActions ::: Action.Send(newNode, Neighbor(self, highPriority = true)) :: forwardActions)

      case ForwardJoin(newNode, ttl, sender) =>
        val afterRemember = rememberPeer(newNode)
        if newNode.uid == self.uid then Result(afterRemember, Nil)
        else if ttl == 0 || afterRemember.active.size <= 1 then
          val (next, actions) = afterRemember.addActive(newNode)
          Result(next, actions :+ Action.Send(newNode, Neighbor(self, highPriority = true)))
        else
          val withPassive =
            if ttl == config.passiveRandomWalkLength then afterRemember.addPassiveIfEligible(newNode)
            else afterRemember
          val nextPeers = withPassive.active.filterNot(_.uid == sender)
          if nextPeers.nonEmpty then Result(withPassive, List(Action.Send(withPassive.choose(nextPeers), ForwardJoin(newNode, ttl - 1, self.uid))))
          else
            val (next, actions) = withPassive.addActive(newNode)
            Result(next, actions :+ Action.Send(newNode, Neighbor(self, highPriority = true)))

      case Neighbor(from, highPriority) =>
        val afterRemember = rememberPeer(from)
        val accepted = highPriority || afterRemember.active.size < config.activeViewSize
        if accepted then
          val (next, actions) = afterRemember.addActive(from)
          Result(next, actions :+ Action.Send(from, NeighborReply(self.uid, accepted = true)))
        else Result(afterRemember, List(Action.Send(from, NeighborReply(self.uid, accepted = false))))

      case NeighborReply(from, accepted) =>
        val base = copy(pendingPromotions = pendingPromotions - from)
        val (next, actions) =
          if accepted then known.get(from).map(base.addActive).getOrElse((base, Nil))
          else known.get(from).map(peer => (base.addPassiveIfEligible(peer), Nil)).getOrElse((base, Nil))
        if accepted then next.withPromotionIfNeeded(actions)
        else Result(next, actions)

      case Disconnect(peer) =>
        active.find(_.uid == peer) match
          case Some(dropped) =>
            val next = copy(active = active.filterNot(_.uid == peer), pendingPromotions = pendingPromotions - peer).addPassiveIfEligible(dropped)
            next.withPromotionIfNeeded(Nil)
          case None => Result(this, Nil)

      case Leave(leaving) =>
        peerLost(leaving.uid)

      case Shuffle(origin, sample, ttl, sender) =>
        val remembered = sample.foldLeft(rememberPeer(origin))((state, peer) => state.rememberPeer(peer))
        if ttl > 0 && remembered.active.size > 1 then
          val nextPeers = remembered.active.filterNot(_.uid == sender)
          if nextPeers.nonEmpty then Result(remembered, List(Action.Send(remembered.choose(nextPeers), Shuffle(origin, sample, ttl - 1, self.uid))))
          else remembered.acceptShuffle(origin, sample)
        else remembered.acceptShuffle(origin, sample)

      case ShuffleReply(from, sample) =>
        val remembered = sample.foldLeft(this)((state, peer) => state.rememberPeer(peer))
        val next = remembered.mergeShuffleSample(sample, remembered.pendingShuffleSamples.getOrElse(from, Set.empty)).copy(
          pendingShuffleSamples = remembered.pendingShuffleSamples.removed(from)
        )
        next.withPromotionIfNeeded(Nil)

  private def rememberPeer(peer: PeerRef): HyParViewStateMachine =
    if peer.uid == self.uid then this else copy(known = known.updated(peer.uid, peer))

  private def addActive(peer: PeerRef): (HyParViewStateMachine, List[Action]) =
    if peer.uid == self.uid || active.exists(_.uid == peer.uid) || !canConnectTo(peer) then (this, Nil)
    else
      val (evictedState, evictedActions) = if active.size >= config.activeViewSize then dropRandomActive() else (this, Nil)
      val next = evictedState.copy(
        active = evictedState.active.filterNot(_.uid == peer.uid) :+ peer,
        passive = evictedState.passive.filterNot(_.uid == peer.uid),
        pendingPromotions = evictedState.pendingPromotions - peer.uid,
        known = evictedState.known.updated(peer.uid, peer)
      )
      (next, evictedActions)

  private def addPassiveIfEligible(peer: PeerRef): HyParViewStateMachine =
    if peer.uid == self.uid || !canConnectTo(peer) || active.exists(_.uid == peer.uid) || passive.exists(_.uid == peer.uid) then this
    else
      val trimmed = if passive.size >= config.passiveViewSize then passive.filterNot(_.uid == choose(passive).uid) else passive
      copy(passive = trimmed :+ peer, known = known.updated(peer.uid, peer))

  private def dropRandomActive(): (HyParViewStateMachine, List[Action]) =
    if active.isEmpty then (this, Nil)
    else
      val dropped = choose(active)
      val next = copy(active = active.filterNot(_.uid == dropped.uid)).addPassiveIfEligible(dropped)
      (next, List(Action.Send(dropped, Disconnect(self.uid))))

  private def acceptShuffle(origin: PeerRef, incomingSample: Set[PeerRef]): Result = {
    val replySample = randomSubset(passive.toSet, incomingSample.size)
    val next = mergeShuffleSample(incomingSample, replySample)
    Result(next, List(Action.Send(origin, ShuffleReply(self.uid, replySample))))
  }

  private def mergeShuffleSample(incomingSample: Set[PeerRef], sentToPeer: Set[PeerRef]): HyParViewStateMachine = {
    val preferredVictims = sentToPeer.map(_.uid)
    incomingSample.foldLeft(this) { (state, peer) =>
      if peer.uid == self.uid || !state.canConnectTo(peer) || state.active.exists(_.uid == peer.uid) || state.passive.exists(_.uid == peer.uid) then state
      else
        val afterEvict =
          if state.passive.size >= config.passiveViewSize then state.evictPassiveForShuffle(preferredVictims)
          else state
        if afterEvict.passive.size < config.passiveViewSize then
          afterEvict.copy(passive = afterEvict.passive :+ peer, known = afterEvict.known.updated(peer.uid, peer))
        else afterEvict
    }
  }

  private def evictPassiveForShuffle(preferredVictims: Set[Uid]): HyParViewStateMachine = {
    val victim = passive.find(p => preferredVictims.contains(p.uid)).getOrElse(choose(passive))
    copy(passive = passive.filterNot(_.uid == victim.uid))
  }

  private def withPromotionIfNeeded(existingActions: List[Action]): Result =
    if active.size >= config.activeViewSize then Result(this, existingActions)
    else
      passive.find(peer => !pendingPromotions.contains(peer.uid)) match
        case Some(candidate) =>
          Result(copy(pendingPromotions = pendingPromotions + candidate.uid), existingActions :+ Action.Send(candidate, Neighbor(self, highPriority = active.isEmpty)))
        case None => Result(this, existingActions)

  private def choose[A](values: Vector[A]): A = values(randomIndex(0, values.size))

  private def randomSubset[A](values: Set[A], maxSize: Int): Set[A] = {
    val vec = values.toVector
    val count = math.min(maxSize, vec.size)
    (0 until count).foldLeft((Vector.empty[A], vec)) { case ((chosen, remaining), _) =>
      val idx = randomIndex(0, remaining.size)
      (chosen :+ remaining(idx), remaining.patch(idx, Nil, 1))
    }._1.toSet
  }
}
