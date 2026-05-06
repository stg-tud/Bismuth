package replication.overlay

import channels.{ChannelConnectInfo, Connection, PeerConnectInfo}
import rdts.base.Uid
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.overlay.OverlayController.OverlayMessage.*
import replication.overlay.OverlayController.{OverlayAction, OverlayMessage}

trait OverlayController {

  /** Handle one overlay control-plane message and return the next controller state plus side-effect actions.
    * If a concrete connection is supplied, the controller may learn which peer sent the message and attach that
    * opaque connection handle to the corresponding active-view entry.
    */
  def receiveActions(
      message: OverlayMessage,
      from: Connection
  ): (OverlayController, List[OverlayAction]) =
    (this, Nil)

  /** Register a newly established connection before the remote peer identity is known. */
  def registerConnection(conn: Connection): OverlayController = this

  /** Remove a connection previously registered with the controller.
    * Returns the next state and the peer that had been associated with the connection, if any.
    */
  def removeConnection(conn: Connection): (OverlayController, Option[Uid]) = (this, None)

  /** Lookup the currently known connection for a peer, if one is attached to an active-view entry. */
  def connectionFor(peer: Uid): Option[Connection] = None

  /** Reverse lookup from connection object to peer identity, if known. */
  def peerForConnection(conn: Connection): Option[Uid] = None
}

object OverlayController {
  object none extends OverlayController

  enum OverlayMessage {
    case Join(newNode: PeerConnectInfo)
    case ForwardJoin(newNode: PeerConnectInfo, ttl: Int, sender: Uid)
    case Neighbor(from: PeerConnectInfo, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(origin: PeerConnectInfo, sample: Set[PeerConnectInfo], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[PeerConnectInfo])

    def getSender: Uid = this match
        case Join(newNode)             => newNode.uid
        case ForwardJoin(_, _, sender) => sender
        case Neighbor(from, _)         => from.uid
        case NeighborReply(from, _)    => from
        case Disconnect(from)          => from
        case Shuffle(_, _, _, sender)  => sender
        case ShuffleReply(from, _)     => from
  }

  enum OverlayAction {

    /** Send a control-plane message to a peer.
      * If `connection` is defined, it is the currently known live connection and callers may use it directly.
      * Otherwise callers should establish a connection using the peer's advertised details.
      */
    case Send(to: PeerConnectInfo, connection: Option[Connection], message: OverlayMessage)

    /** Bootstrap-only send where no peer identity is known yet, only raw connection details. */
    case SendJoin(to: Set[ChannelConnectInfo], message: OverlayMessage)
  }
}

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

  /** Active-view entry enriched with the currently known live connection, if any. */
  final case class ActivePeer(peer: PeerConnectInfo, connection: Option[Connection])

  /** Pure state machine result: next immutable state plus requested network actions. */
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
      passive = Vector.empty,
      pendingPromotions = Set.empty,
      pendingShuffleSamples = Map.empty,
      unknownConnections = Vector.empty,
      randomIndex = randomIndex,
      canConnectTo = canConnectTo,
    )
}

final case class HyParViewStateMachine(
    self: PeerConnectInfo,
    config: HyParViewConfig,
    known: Map[Uid, PeerConnectInfo],
    active: Vector[HyParViewStateMachine.ActivePeer],
    passive: Vector[PeerConnectInfo],
    pendingPromotions: Set[Uid],
    pendingShuffleSamples: Map[Uid, Set[PeerConnectInfo]],
    unknownConnections: Vector[Connection],
    randomIndex: (Int, Int) => Int,
    canConnectTo: PeerConnectInfo => Boolean,
) extends OverlayController {
  import HyParViewStateMachine.*

  def activeView: Set[Uid]               = active.iterator.map(_.peer.uid).toSet
  def passiveView: Set[Uid]              = passive.iterator.map(_.uid).toSet
  def activePeers: Set[PeerConnectInfo]  = active.iterator.map(_.peer).toSet
  def passivePeers: Set[PeerConnectInfo] = passive.toSet

  /** Paper bootstrap step: send `Join` to a known contact node. */
  def initiateJoin(details: Set[ChannelConnectInfo]): Result =
    Result(this, List(OverlayAction.SendJoin(details, Join(self))))

  /** Paper passive-view maintenance step: initiate one shuffle through a random active peer. */
  def shuffleTick(): Result =
    if active.isEmpty then Result(this, Nil)
    else
        val target = choose(active)
        val sample =
          Set(self) ++ randomSubset(active.iterator.map(_.peer).toSet, config.shuffleActiveSample) ++ randomSubset(
            passive.toSet,
            config.shufflePassiveSample
          )
        Result(
          copy(pendingShuffleSamples = pendingShuffleSamples.updated(target.peer.uid, sample)),
          List(OverlayAction.Send(
            target.peer,
            target.connection,
            Shuffle(self, sample, config.shuffleRandomWalkLength, self.uid)
          ))
        )

  /** Local liveness/progress hook: if we are under target active view size, retry promotion from the passive view.
    * This is useful when attempted promotions get stuck or when the active set has shrunk but is not yet empty.
    */
  def promotionTick(): Result =
    if active.size >= config.activeViewSize || passive.isEmpty then Result(this, Nil)
    else copy(pendingPromotions = Set.empty).withPromotionIfNeeded(Nil)

  /** Implementation hook: learn externally discovered peers by placing dialable ones in the passive view, then try to heal the active view. */
  def discoverPeers(peers: Set[PeerConnectInfo]): Result = {
    val next = peers.foldLeft(this)((state, peer) => state.rememberPeer(peer).addPassiveIfEligible(peer))
    next.withPromotionIfNeeded(Nil)
  }

  /** Handle one HyParView protocol message according to the paper's join, neighbor, disconnect, and shuffle rules. */
  override def receiveActions(
      message: OverlayMessage,
      from: Connection
  ): (OverlayController, List[OverlayAction]) = {
    val attached = attachConnection(message.getSender, from)
    val result   = attached.receive(message)
    (result.state, result.actions)
  }

  def receive(message: OverlayMessage): Result = {
    message match
        case Join(newNode) =>
          // Paper join handling at the contact: add the newcomer to active and start forwarding `ForwardJoin` through current active peers.
          val afterRemember                = rememberPeer(newNode)
          val (afterActive, activeActions) = afterRemember.addActive(newNode)
          val forwardActions               = afterActive.active.filterNot(_.peer.uid == newNode.uid).map(peer =>
            OverlayAction.Send(
              peer.peer,
              peer.connection,
              ForwardJoin(newNode, config.activeRandomWalkLength, self.uid)
            )
          ).toList
          Result(
            afterActive,
            activeActions ::: OverlayAction.Send(
              newNode,
              afterActive.connectionFor(newNode.uid),
              Neighbor(self, highPriority = true)
            ) :: forwardActions
          )

        case ForwardJoin(newNode, ttl, sender) =>
          // Paper random-walk join propagation: maybe add to passive at PRWL, stop at ttl==0 or singleton active view, otherwise keep forwarding.
          val afterRemember = rememberPeer(newNode)
          if newNode.uid == self.uid then Result(afterRemember, Nil)
          else if ttl == 0 || afterRemember.active.size <= 1 then
              val (next, actions) = afterRemember.addActive(newNode)
              Result(
                next,
                actions :+ OverlayAction.Send(
                  newNode,
                  next.connectionFor(newNode.uid),
                  Neighbor(self, highPriority = true)
                )
              )
          else
              val withPassive =
                if ttl == config.passiveRandomWalkLength then afterRemember.addPassiveIfEligible(newNode)
                else afterRemember
              val nextPeers = withPassive.active.filterNot(_.peer.uid == sender)
              if nextPeers.nonEmpty then
                  Result(
                    withPassive,
                    List(OverlayAction.Send(
                      withPassive.choose(nextPeers).peer,
                      withPassive.choose(nextPeers).connection,
                      ForwardJoin(newNode, ttl - 1, self.uid)
                    ))
                  )
              else
                  val (next, actions) = withPassive.addActive(newNode)
                  Result(
                    next,
                    actions :+ OverlayAction.Send(
                      newNode,
                      next.connectionFor(newNode.uid),
                      Neighbor(self, highPriority = true)
                    )
                  )

        case Neighbor(from, highPriority) =>
          // Paper active-view repair handshake: high priority always accepted, low priority only if there is a free active slot.
          val afterRemember = rememberPeer(from)
          val accepted      = highPriority || afterRemember.active.size < config.activeViewSize
          if accepted then
              val (next, actions) = afterRemember.addActive(from)
              Result(
                next,
                actions :+ OverlayAction.Send(
                  from,
                  next.connectionFor(from.uid),
                  NeighborReply(self.uid, accepted = true)
                )
              )
          else
              Result(
                afterRemember,
                List(OverlayAction.Send(
                  from,
                  afterRemember.connectionFor(from.uid),
                  NeighborReply(self.uid, accepted = false)
                ))
              )

        case NeighborReply(from, accepted) =>
          // Paper promotion result: accepted peers move to active; rejected peers stay passive.
          val base            = copy(pendingPromotions = pendingPromotions - from)
          val (next, actions) =
            if accepted then known.get(from).map(base.addActive).getOrElse((base, Nil))
            else known.get(from).map(peer => (base.addPassiveIfEligible(peer), Nil)).getOrElse((base, Nil))
          if accepted then next.withPromotionIfNeeded(actions)
          else Result(next, actions)

        case Disconnect(peer) =>
          // Paper symmetric-link maintenance: a dropped active neighbor is removed from active and retained as a passive backup.
          active.find(_.peer.uid == peer) match
              case Some(dropped) =>
                val next = copy(
                  active = active.filterNot(_.peer.uid == peer),
                  pendingPromotions = pendingPromotions - peer
                ).addPassiveIfEligible(dropped.peer)
                next.withPromotionIfNeeded(Nil)
              case None => Result(this, Nil)

        case Shuffle(origin, sample, ttl, sender) =>
          // Paper shuffle walk: intermediate hops only forward; the endpoint replies and merges the received sample.
          val remembered = sample.foldLeft(rememberPeer(origin))((state, peer) => state.rememberPeer(peer))
          if ttl > 0 && remembered.active.size > 1 then
              val nextPeers = remembered.active.filterNot(_.peer.uid == sender)
              if nextPeers.nonEmpty then
                  Result(
                    remembered,
                    List(OverlayAction.Send(
                      remembered.choose(nextPeers).peer,
                      remembered.choose(nextPeers).connection,
                      Shuffle(origin, sample, ttl - 1, self.uid)
                    ))
                  )
              else remembered.acceptShuffle(origin, sample)
          else remembered.acceptShuffle(origin, sample)

        case ShuffleReply(from, sample) =>
          // Paper shuffle completion at the initiator: merge the reply sample, preferring eviction of entries previously sent to the peer.
          val remembered = sample.foldLeft(this)((state, peer) => state.rememberPeer(peer))
          val next       =
            remembered.mergeShuffleSample(sample, remembered.pendingShuffleSamples.getOrElse(from, Set.empty)).copy(
              pendingShuffleSamples = remembered.pendingShuffleSamples.removed(from)
            )
          next.withPromotionIfNeeded(Nil)
  }

  override def registerConnection(conn: Connection): OverlayController =
    if peerForConnection(conn).nonEmpty || unknownConnections.contains(conn) then
        throw IllegalStateException(s"Connection already registered: ${conn.toString}")
    else copy(unknownConnections = unknownConnections :+ conn)

  override def removeConnection(conn: Connection): (OverlayController, Option[Uid]) =
    active.find(_.connection.contains(conn)) match
        case Some(activePeer) =>
          (
            copy(
              active = active.map(ap => if ap.peer.uid == activePeer.peer.uid then ap.copy(connection = None) else ap),
              unknownConnections = unknownConnections.filterNot(_ == conn)
            ),
            Some(activePeer.peer.uid)
          )
        case None =>
          (copy(unknownConnections = unknownConnections.filterNot(_ == conn)), None)

  override def connectionFor(peer: Uid): Option[Connection] =
    active.find(_.peer.uid == peer).flatMap(_.connection)

  override def peerForConnection(conn: Connection): Option[Uid] =
    active.find(_.connection.contains(conn)).map(_.peer.uid)

  private def attachConnection(peer: Uid, conn: Connection): HyParViewStateMachine = {
    if peerForConnection(conn).contains(peer)
    then this
    else {
      val stripped = copy(
        active = active.map(ap =>
          if ap.connection.contains(conn) && ap.peer.uid != peer then ap.copy(connection = None) else ap
        ),
        unknownConnections = unknownConnections.filterNot(_ == conn)
      )
      stripped.copy(active =
        stripped.active.map(ap => if ap.peer.uid == peer then ap.copy(connection = Some(conn)) else ap)
      )
    }
  }

  private def rememberPeer(peer: PeerConnectInfo): HyParViewStateMachine =
    if peer.uid == self.uid then this else copy(known = known.updated(peer.uid, peer))

  private def addActive(peer: PeerConnectInfo): (HyParViewStateMachine, List[OverlayAction]) =
    if peer.uid == self.uid || active.exists(_.peer.uid == peer.uid) || !canConnectTo(peer) then (this, Nil)
    else
        val (evictedState, evictedActions) =
          if active.size >= config.activeViewSize then dropRandomActive() else (this, Nil)
        val next = evictedState.copy(
          active = evictedState.active.filterNot(_.peer.uid == peer.uid) :+ ActivePeer(
            peer,
            evictedState.connectionFor(peer.uid)
          ),
          passive = evictedState.passive.filterNot(_.uid == peer.uid),
          pendingPromotions = evictedState.pendingPromotions - peer.uid,
          known = evictedState.known.updated(peer.uid, peer)
        )
        (next, evictedActions)

  private def addPassiveIfEligible(peer: PeerConnectInfo): HyParViewStateMachine =
    if peer.uid == self.uid || !canConnectTo(peer) || active.exists(_.peer.uid == peer.uid) || passive.exists(
          _.uid == peer.uid
        )
    then this
    else {
      val trimmed =
        if passive.size >= config.passiveViewSize then passive.filterNot(_.uid == choose(passive).uid) else passive
      copy(passive = trimmed :+ peer, known = known.updated(peer.uid, peer))
    }

  private def dropRandomActive(): (HyParViewStateMachine, List[OverlayAction]) =
    if active.isEmpty then (this, Nil)
    else {
      val dropped = choose(active)
      val next    = copy(active = active.filterNot(_.peer.uid == dropped.peer.uid)).addPassiveIfEligible(dropped.peer)
      (next, List(OverlayAction.Send(dropped.peer, dropped.connection, Disconnect(self.uid))))
    }

  private def acceptShuffle(origin: PeerConnectInfo, incomingSample: Set[PeerConnectInfo]): Result = {
    val replySample = randomSubset(passive.toSet, incomingSample.size)
    val next        = mergeShuffleSample(incomingSample, replySample)
    Result(next, List(OverlayAction.Send(origin, next.connectionFor(origin.uid), ShuffleReply(self.uid, replySample))))
  }

  private def mergeShuffleSample(
      incomingSample: Set[PeerConnectInfo],
      sentToPeer: Set[PeerConnectInfo]
  ): HyParViewStateMachine = {
    val preferredVictims = sentToPeer.map(_.uid)
    incomingSample.foldLeft(this) { (state, peer) =>
      if peer.uid == self.uid || !state.canConnectTo(peer) || state.active.exists(
            _.peer.uid == peer.uid
          ) || state.passive.exists(_.uid == peer.uid)
      then state
      else {
        val afterEvict =
          if state.passive.size >= config.passiveViewSize then state.evictPassiveForShuffle(preferredVictims)
          else state
        if afterEvict.passive.size < config.passiveViewSize then
            afterEvict.copy(passive = afterEvict.passive :+ peer, known = afterEvict.known.updated(peer.uid, peer))
        else afterEvict
      }
    }
  }

  private def evictPassiveForShuffle(preferredVictims: Set[Uid]): HyParViewStateMachine = {
    val victim = passive.find(p => preferredVictims.contains(p.uid)).getOrElse(choose(passive))
    copy(passive = passive.filterNot(_.uid == victim.uid))
  }

  private def withPromotionIfNeeded(existingActions: List[OverlayAction]): Result =
    if active.size >= config.activeViewSize then Result(this, existingActions)
    else
        passive.find(peer => !pendingPromotions.contains(peer.uid)) match
            case Some(candidate) =>
              Result(
                copy(pendingPromotions = pendingPromotions + candidate.uid),
                existingActions :+ OverlayAction.Send(
                  candidate,
                  connectionFor(candidate.uid),
                  Neighbor(self, highPriority = active.isEmpty)
                )
              )
            case None => Result(this, existingActions)

  private def choose[A](values: Vector[A]): A = values(randomIndex(0, values.size))

  private def randomSubset[A](values: Set[A], maxSize: Int): Set[A] = {
    val vec   = values.toVector
    val count = math.min(maxSize, vec.size)
    (0 until count).foldLeft((Vector.empty[A], vec)) { case ((chosen, remaining), _) =>
      val idx = randomIndex(0, remaining.size)
      (chosen :+ remaining(idx), remaining.patch(idx, Nil, 1))
    }._1.toSet
  }
}
