package channels.overlay

import channels.connection.{ConnectionDescriptor, PeerConnectInfo}
import channels.overlay.HyParViewStateMachine.{HyParViewConfig, Link, Purpose, Result}
import channels.overlay.OverlayController.OverlayMessage.*
import channels.overlay.OverlayController.{OverlayAction, OverlayEvent, OverlayMessage, OverlayTimer}
import rdts.base.Uid

/** Immutable membership state machine for HyParView.
  *
  * All connections the overlay knows about are tracked in a single table:
  * - `dialing`: outgoing attempts that have not yet produced a connection, with the purpose of the attempt
  * - `links`: live connections, each either `Pending` (handshake not settled) or `Active` (an active-view edge)
  *
  * Every event is a transition of that table. After each event the active view is refilled from the passive view
  * as long as there are free slots and untried candidates, and a shuffle timer is kept running while the node has
  * neighbors.
  *
  * Deviations from the paper:
  * - peers are only dialed or kept passive if `canConnectTo(peer)` is true. The paper assumes any advertised peer
  *   can be contacted; here some connection kinds are not supported by all peers.
  * - a node with a free active slot actively fills it from the passive view (low priority), instead of only
  *   reacting to failures. Candidates that refused are skipped until the next shuffle round.
  * - simultaneous Neighbor handshakes between the same two peers are resolved by keeping the connection
  *   dialed by the peer with the smaller id.
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

  /** State of a live connection. */
  enum Link {

    /** Connected, handshake not settled. `peer` is known once a Join/Neighbor identified the remote side.
      * `requestedByUs` is true if we sent the Neighbor request and are waiting for the reply.
      */
    case Pending(peer: Option[PeerConnectInfo], requestedByUs: Boolean)

    /** An active-view edge. */
    case Active(peer: PeerConnectInfo)
  }

  /** Why an outgoing connection is dialed; decides what happens once it is established. */
  enum Purpose {

    /** Bootstrap through a contact: send `Join` and wait for the contact's `Neighbor`. */
    case Join

    /** Ask `peer` to become an active neighbor. */
    case Neighbor(peer: PeerConnectInfo, highPriority: Boolean)

    /** Deliver a single message and close again (paper: "using a temporary TCP connection"). */
    case OneShot(peer: PeerConnectInfo, message: OverlayMessage)
  }

  final case class Result(state: HyParViewStateMachine, actions: List[OverlayAction]) {
    def andThen(f: HyParViewStateMachine => Result): Result = {
      val Result(next, more) = f(state)
      Result(next, actions ++ more)
    }
    def emit(more: OverlayAction*): Result = Result(state, actions ++ more)
  }

  def empty(
      self: PeerConnectInfo,
      config: HyParViewConfig,
      randomIndex: (Int, Int) => Int,
      canConnectTo: PeerConnectInfo => Boolean,
  ): HyParViewStateMachine =
    HyParViewStateMachine(
      self = self,
      config = config,
      known = Map.empty,
      links = Map.empty,
      dialing = Map.empty,
      refused = Set.empty,
      nextAttempt = 0L,
      shuffleScheduled = false,
      randomIndex = randomIndex,
      canConnectTo = canConnectTo,
    )
}

final case class HyParViewStateMachine(
    self: PeerConnectInfo,
    config: HyParViewConfig,
    /** All peers we know about other than ourselves; the passive view is `known` minus the active view. */
    known: Map[Uid, PeerConnectInfo],
    links: Map[ConnId, Link],
    dialing: Map[AttemptId, Purpose],
    /** Candidates that refused or failed a handshake; not retried before the next shuffle round. */
    refused: Set[Uid],
    nextAttempt: Long,
    shuffleScheduled: Boolean,
    randomIndex: (Int, Int) => Int,
    canConnectTo: PeerConnectInfo => Boolean,
) extends OverlayController {

  // ------------------------------------------------------------------------------------------------
  // views
  // ------------------------------------------------------------------------------------------------

  /** Active edges in a deterministic order (by connection id). */
  def activeEntries: Vector[(ConnId, PeerConnectInfo)] =
    links.toVector.collect { case (conn, Link.Active(peer)) => conn -> peer }.sortBy(_._1.value)

  def activeView: Set[Uid]               = activeEntries.iterator.map(_._2.uid).toSet
  def activePeers: Set[PeerConnectInfo]  = activeEntries.iterator.map(_._2).toSet
  def passivePeers: Set[PeerConnectInfo] =
      val act = activeView
      known.valuesIterator.filter(peer => !act.contains(peer.uid) && canConnectTo(peer)).toSet
  def passiveView: Set[Uid] = passivePeers.iterator.map(_.uid).toSet

  override def neighbors: Set[Uid] = activeView

  override def connectionFor(peer: Uid): Option[ConnId] =
    activeEntries.collectFirst { case (conn, p) if p.uid == peer => conn }

  override def selfConnectionDescriptors: Set[ConnectionDescriptor] = self.channelConnectors

  /** Peers we asked to become neighbors and are waiting for (dialing, or Neighbor sent). */
  private def requested: Set[Uid] =
    links.valuesIterator.collect { case Link.Pending(Some(peer), true) => peer.uid }.toSet ++
    dialing.valuesIterator.collect { case Purpose.Neighbor(peer, _) => peer.uid }

  /** Active, or a handshake in either direction is in flight. */
  private def busyWith(peer: Uid): Boolean =
    activeView.contains(peer) || requested.contains(peer) ||
    links.valuesIterator.exists { case Link.Pending(Some(p), _) => p.uid == peer; case _ => false }

  // ------------------------------------------------------------------------------------------------
  // event handling
  // ------------------------------------------------------------------------------------------------

  override def step(event: OverlayEvent): (OverlayController, List[OverlayAction]) = {
    val Result(next, actions) = handle(event).andThen(_.refill()).andThen(_.keepShuffling())
    (next, actions)
  }

  private def done: Result = Result(this, Nil)

  def handle(event: OverlayEvent): Result = event match
      case OverlayEvent.Bootstrap(contact) =>
        dial(Purpose.Join, Set(contact))

      case OverlayEvent.Discovered(peers) =>
        Result(peers.foldLeft(this)(_.rememberPeer(_)), Nil)

      case OverlayEvent.SelfDescriptorAdded(descriptor) =>
        Result(copy(self = self.copy(channelConnectors = self.channelConnectors + descriptor)), Nil)

      case OverlayEvent.Incoming(conn) =>
        Result(
          copy(links = links.updated(conn, Link.Pending(None, requestedByUs = false))),
          List(startHandshakeTimer(conn))
        )

      case OverlayEvent.Established(attempt, conn) =>
        val base = copy(dialing = dialing.removed(attempt))
        dialing.get(attempt) match
            case None               => Result(base, List(OverlayAction.Close(conn)))
            case Some(Purpose.Join) =>
              Result(
                base.copy(links = links.updated(conn, Link.Pending(None, requestedByUs = false))),
                List(OverlayAction.Send(conn, Join(self)), startHandshakeTimer(conn))
              )
            case Some(Purpose.Neighbor(peer, highPriority)) =>
              if activeView.contains(peer.uid) then Result(base, List(OverlayAction.Close(conn)))
              else
                  Result(
                    base.copy(links = links.updated(conn, Link.Pending(Some(peer), requestedByUs = true))),
                    List(OverlayAction.Send(conn, Neighbor(self, highPriority)), startHandshakeTimer(conn))
                  )
            case Some(Purpose.OneShot(_, message)) =>
              Result(base, List(OverlayAction.Send(conn, message), OverlayAction.Close(conn)))

      case OverlayEvent.DialFailed(attempt) =>
        val base = copy(dialing = dialing.removed(attempt))
        dialing.get(attempt) match
            // paper: a peer that can not be contacted is removed from the passive view
            case Some(Purpose.Neighbor(peer, _)) => Result(base.forgetPeer(peer.uid), Nil)
            case _                               => Result(base, Nil)

      case OverlayEvent.Closed(conn) =>
        val base = copy(links = links.removed(conn))
        links.get(conn) match
            case Some(Link.Active(peer))           => Result(base, List(OverlayAction.NeighborDown(peer.uid)))
            case Some(Link.Pending(Some(peer), _)) => Result(base.copy(refused = refused + peer.uid), Nil)
            case _                                 => Result(base, Nil)

      case OverlayEvent.TimerFired(OverlayTimer.Handshake(conn)) =>
        links.get(conn) match
            case Some(Link.Pending(peer, _)) =>
              Result(
                copy(links = links.removed(conn), refused = refused ++ peer.map(_.uid)),
                List(OverlayAction.Close(conn))
              )
            case _ => done

      case OverlayEvent.TimerFired(OverlayTimer.Shuffle) =>
        copy(shuffleScheduled = false, refused = Set.empty).shuffle()

      case OverlayEvent.Received(conn, message) =>
        if links.contains(conn) then receive(conn, message) else done

  private def startHandshakeTimer(conn: ConnId) = OverlayAction.StartTimer(OverlayTimer.Handshake(conn))

  private def receive(conn: ConnId, message: OverlayMessage): Result = message match
      case Join(newNode) =>
        // Paper join contact handling: the contact adds the newcomer (forced, high priority) and forwards
        // `ForwardJoin` along its current active view to seed additional peers.
        if newNode.uid == self.uid then done
        else {
          val forwards = activeEntries.collect {
            case (target, peer) if peer.uid != newNode.uid =>
              OverlayAction.Send(target, ForwardJoin(newNode, config.activeRandomWalkLength, self.uid))
          }
          Result(
            rememberPeer(newNode).copy(links = links.updated(conn, Link.Pending(Some(newNode), requestedByUs = true))),
            OverlayAction.Send(conn, Neighbor(self, highPriority = true)) :: forwards.toList
          )
        }

      case ForwardJoin(newNode, ttl, sender) =>
        // Paper forwarded-join random walk: intermediate hops forward to another active peer (remembering the
        // newcomer at PRWL); the last hop adds the newcomer, unless it already is (or is becoming) a neighbor.
        def terminate(state: HyParViewStateMachine): Result =
          if state.busyWith(newNode.uid) || !canConnectTo(newNode) then Result(state, Nil)
          else state.dial(Purpose.Neighbor(newNode, highPriority = true), newNode.channelConnectors)

        if newNode.uid == self.uid then done
        else if ttl == 0 || activeEntries.size <= 1 then terminate(rememberPeer(newNode))
        else {
          val withPassive = if ttl == config.passiveRandomWalkLength then rememberPeer(newNode) else this
          // never walk back to the sender or to the newcomer itself (it would just drop the walk)
          choose(activeEntries.filterNot((_, p) => p.uid == sender || p.uid == newNode.uid)) match
              case Some((target, _)) =>
                Result(withPassive, List(OverlayAction.Send(target, ForwardJoin(newNode, ttl - 1, self.uid))))
              case None => terminate(withPassive)
        }

      case Neighbor(from, highPriority) =>
        val remembered = rememberPeer(from)
        if from.uid == self.uid then Result(remembered, List(OverlayAction.Close(conn)))
        else if remembered.ownHandshakeWins(from.uid, conn) then
            // both sides dialed each other; the connection dialed by the smaller id is kept
            Result(remembered, List(OverlayAction.Send(conn, NeighborReply(self.uid, accepted = false))))
        else if highPriority || activeView.contains(from.uid) || activeEntries.size < config.activeViewSize then
            remembered.addActive(from, conn).emit(OverlayAction.Send(conn, NeighborReply(self.uid, accepted = true)))
        else
            Result(
              remembered.copy(links = links.updated(conn, Link.Pending(Some(from), requestedByUs = false))),
              List(OverlayAction.Send(conn, NeighborReply(self.uid, accepted = false)))
            )

      case NeighborReply(_, accepted) =>
        links.get(conn) match
            case Some(Link.Pending(Some(peer), true)) =>
              if accepted then addActive(peer, conn)
              else
                  Result(
                    copy(links = links.removed(conn), refused = refused + peer.uid),
                    List(OverlayAction.Close(conn))
                  )
            case _ => done

      case Shuffle(origin, sample, ttl, sender) =>
        // Paper shuffle walk: intermediate hops only forward; the endpoint replies and merges the received sample.
        val remembered = sample.foldLeft(rememberPeer(origin))(_.rememberPeer(_))
        if ttl > 0 && activeEntries.size > 1 then
            choose(activeEntries.filterNot((_, p) => p.uid == sender)) match
                case Some((target, _)) =>
                  Result(remembered, List(OverlayAction.Send(target, Shuffle(origin, sample, ttl - 1, self.uid))))
                case None => remembered.acceptShuffle(origin, sample)
        else remembered.acceptShuffle(origin, sample)

      case ShuffleReply(_, sample, answeredSample) =>
        // Paper shuffle completion at the initiator: merge the reply sample, preferring eviction of entries
        // previously sent to the peer. The reply echoes what we sent, so no per-shuffle state is needed.
        val remembered = sample.foldLeft(this)(_.rememberPeer(_))
        Result(remembered.mergeShuffleSample(sample, answeredSample), Nil)

  // ------------------------------------------------------------------------------------------------
  // transitions
  // ------------------------------------------------------------------------------------------------

  /** True if we have our own handshake with `peer` in flight on another connection, and ours wins the tie-break. */
  private def ownHandshakeWins(peer: Uid, incoming: ConnId): Boolean =
      val ownInFlight = links.exists {
        case (c, Link.Pending(Some(p), true)) => c != incoming && p.uid == peer
        case _                                => false
      } || dialing.valuesIterator.exists { case Purpose.Neighbor(p, _) => p.uid == peer; case _ => false }
      ownInFlight && Uid.unwrap(self.uid) < Uid.unwrap(peer)

  private def dial(purpose: Purpose, to: Set[ConnectionDescriptor]): Result = {
    val attempt = AttemptId(nextAttempt)
    Result(
      copy(nextAttempt = nextAttempt + 1, dialing = dialing.updated(attempt, purpose)),
      List(OverlayAction.Dial(attempt, to))
    )
  }

  /** Make `conn` the active edge to `peer`, evicting a random neighbor if the active view is full. */
  private def addActive(peer: PeerConnectInfo, conn: ConnId): Result =
    connectionFor(peer.uid) match
        case Some(existing) if existing == conn => done
        case Some(existing)                     =>
          // a second edge to the same neighbor (e.g. a re-join); keep the new one. Messages in flight on the old
          // connection are lost, so the broadcast layer has to restart its handshake with the peer.
          Result(
            copy(links = links.removed(existing).updated(conn, Link.Active(peer))),
            List(
              OverlayAction.Close(existing),
              OverlayAction.NeighborDown(peer.uid),
              OverlayAction.NeighborUp(peer.uid)
            )
          )
        case None =>
          val evicted =
            if activeEntries.size < config.activeViewSize then done
            else
                choose(activeEntries) match
                    case None                   => done
                    case Some((evConn, evPeer)) =>
                      Result(
                        copy(links = links.removed(evConn)),
                        List(OverlayAction.Close(evConn), OverlayAction.NeighborDown(evPeer.uid))
                      )
          evicted.andThen { s =>
            Result(
              s.rememberPeer(peer).copy(
                links = s.links.updated(conn, Link.Active(peer)),
                refused = s.refused - peer.uid
              ),
              List(OverlayAction.NeighborUp(peer.uid))
            )
          }

  /** Fill free active slots from the passive view, one low-priority request per free slot. */
  private def refill(): Result = {
    val free       = config.activeViewSize - activeEntries.size - requested.size
    val candidates = passivePeers.filterNot(p => busyWith(p.uid) || refused.contains(p.uid)).toVector
    if free <= 0 then done
    else
        choose(candidates) match
            case None            => done
            case Some(candidate) =>
              dial(Purpose.Neighbor(candidate, highPriority = activeEntries.isEmpty), candidate.channelConnectors)
                .andThen(_.refill())
  }

  /** Keep exactly one shuffle timer running while there are neighbors. */
  private def keepShuffling(): Result =
    if shuffleScheduled || activeEntries.isEmpty then done
    else Result(copy(shuffleScheduled = true), List(OverlayAction.StartTimer(OverlayTimer.Shuffle)))

  /** Paper passive-view maintenance step: initiate one shuffle through a random active peer. */
  private def shuffle(): Result =
    choose(activeEntries) match
        case None              => done
        case Some((target, _)) =>
          val sample = Set(self) ++ randomSubset(activePeers, config.shuffleActiveSample) ++
            randomSubset(passivePeers, config.shufflePassiveSample)
          Result(
            this,
            List(OverlayAction.Send(target, Shuffle(self, sample, config.shuffleRandomWalkLength, self.uid)))
          )

  private def acceptShuffle(origin: PeerConnectInfo, incomingSample: Set[PeerConnectInfo]): Result = {
    val replySample = randomSubset(passivePeers, incomingSample.size)
    val next        = mergeShuffleSample(incomingSample, replySample)
    val reply       = ShuffleReply(self.uid, replySample, incomingSample)
    // Paper: the reply is sent back using a temporary connection if the origin is not an active neighbor.
    next.connectionFor(origin.uid) match
        case Some(conn) => Result(next, List(OverlayAction.Send(conn, reply)))
        case None if origin.uid != self.uid && next.canConnectTo(origin) =>
          next.dial(Purpose.OneShot(origin, reply), origin.channelConnectors)
        case None => Result(next, Nil)
  }

  private def mergeShuffleSample(
      incomingSample: Set[PeerConnectInfo],
      sentToPeer: Set[PeerConnectInfo]
  ): HyParViewStateMachine = {
    val preferredVictims = sentToPeer.map(_.uid)
    incomingSample.foldLeft(this) { (state, peer) =>
      if peer.uid == self.uid || !state.canConnectTo(peer) || state.activeView.contains(peer.uid) ||
          state.passiveView.contains(peer.uid)
      then state
      else state.rememberPeer(peer).trimKnownPassiveIfNeeded(preferredVictims)
    }
  }

  private def rememberPeer(peer: PeerConnectInfo): HyParViewStateMachine =
    if peer.uid == self.uid then this else copy(known = known.updated(peer.uid, peer))

  private def forgetPeer(peer: Uid): HyParViewStateMachine =
    copy(known = known.removed(peer), refused = refused - peer)

  private def trimKnownPassiveIfNeeded(preferredVictims: Set[Uid]): HyParViewStateMachine = {
    val passive = passivePeers.toIndexedSeq
    if passive.size <= config.passiveViewSize then this
    else
        passive.find(p => preferredVictims.contains(p.uid)).orElse(choose(passive)) match
            case Some(victim) => forgetPeer(victim.uid)
            case None         => this
  }

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
