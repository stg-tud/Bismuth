package replication.overlay

import channels.*
import rdts.base.Uid
import rdts.time.Dots
import replication.PlumtreeBroadcast.Event.Disseminate
import replication.PlumtreeBroadcast.{Event, Peer}
import replication.PlumtreeMessage.Payload
import replication.{DeltaStorage, PlumtreeBroadcast, PlumtreeMessage}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object HyParViewMultiplexed {
  case class PeerRef(uid: Uid, channelConnectors: Set[ChannelConnectDescriptor])

  enum Envelope[State] {
    case Membership(message: HyParViewUnified.HyParViewMessage)
    case Dissemination(message: PlumtreeMessage[State])
  }

}

object HyParViewUnified {
  import HyParViewMultiplexed.PeerRef

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

  enum HyParViewMessage {
    case Join(newNode: PeerRef)
    case ForwardJoin(newNode: PeerRef, ttl: Int, sender: Uid)
    case Neighbor(from: PeerRef, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(origin: PeerRef, sample: Set[PeerRef], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[PeerRef])
  }
}

class HyParViewMultiplexedNode[State](
    val self: HyParViewMultiplexed.PeerRef,
    receiveCallback: State => Unit,
    localServer: LatentConnection[HyParViewMultiplexed.Envelope[State]],
    resolver: ChannelResolver[HyParViewMultiplexed.Envelope[State]],
    contactNode: Option[Set[ChannelConnectDescriptor]],
    rnd: Random,
    deltaStorage: DeltaStorage[State],
    config: HyParViewUnified.HyParViewConfig = HyParViewUnified.HyParViewConfig.default,
    debug: Boolean = false,
    onViewChanged: (HyParViewStateMachine, HyParViewStateMachine) => Unit = (_, _) => (),
    onPeerRolesChanged: () => Unit = () => (),
    onPeerDisconnected: Uid => Unit = (_: Uid) => (),
) {
  import HyParViewMultiplexed.*
  import HyParViewStateMachine.*
  import HyParViewUnified.*

  private val abort                     = Abort()
  private def log(msg: => String): Unit = if debug then println(s"[hyparview ${Uid.unwrap(self.uid)}] $msg")

  private var membership = HyParViewStateMachine.empty(
    self,
    config,
    (_, upperExclusive) => rnd.nextInt(upperExclusive),
    peer => peer.channelConnectors.exists(resolver.canConnect) || peer.uid == self.uid,
  )

  private val connections       = mutable.LinkedHashMap.empty[Uid, Connection[Envelope[State]]]
  private val connectionToPeer  = mutable.LinkedHashMap.empty[Connection[Envelope[State]], Uid]
  private val connecting        = mutable.LinkedHashSet.empty[Uid]
  private val pendingMembership = mutable.LinkedHashMap.empty[Uid, Vector[HyParViewMessage]]
  private var plumtree          = PlumtreeBroadcast[State](self.uid, deltaStorage = deltaStorage)

  def state: HyParViewStateMachine = membership
  def activeView: Set[Uid]         = membership.activeView
  def activePeers: Set[PeerRef]    = membership.activePeers
  def passiveView: Set[Uid]        = membership.passiveView
  def passivePeers: Set[PeerRef]   = membership.passivePeers
  def eagerView: Set[Uid]          = plumtree.eagerPeers

  def applyDelta(delta: State): Unit = {
    val nextDot = plumtree.localContext.nextDot(self.uid)
    applyPlumtreeResult(plumtree.broadcast(Payload(Dots.single(nextDot), delta)))
  }

  private def logFailure(context: String, ex: Throwable): Unit =
    log(s"$context: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}")

  def startServer(): Unit =
    localServer.prepare(receive(None)).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => logFailure("server stopped", ex)
    }

  def join(): Unit = contactNode.foreach(join)

  def join(contact: Set[ChannelConnectDescriptor]): Unit =
    applyTransition(membership.initiateJoin(contact))

  def shuffleTick(): Unit   = applyTransition(membership.shuffleTick())
  def promotionTick(): Unit = {
    applyTransition(membership.promotionTick())
    val missingActiveSlots = math.max(0, config.activeViewSize - membership.activeView.size)
    if missingActiveSlots > 0 then
        membership.passivePeers.iterator
          .filterNot(peer => connections.contains(peer.uid) || connecting.contains(peer.uid))
          .take(missingActiveSlots)
          .foreach(ensurePromotionAttempt)
  }
  def repairTick(): Unit                            = applyPlumtreeResult(plumtree.repairTick())
  def discoverPeers(peers: Iterable[PeerRef]): Unit = applyTransition(membership.discoverPeers(peers.toSet))

  def addIncomingConnection(latent: LatentConnection[Envelope[State]]): Unit =
    latent.prepare(receive(None)).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => logFailure("incoming connection failed", ex)
    }

  def stop(): Unit = {
    val knownPeers = (membership.active.toSet ++ membership.passive.toSet).filterNot(_.uid == self.uid)
    knownPeers.foreach(sendDisconnectAnnouncement)
    connections.values.foreach(_.close())
    connections.clear()
    connectionToPeer.clear()
    connecting.clear()
    pendingMembership.clear()
    val before = membership
    onViewChanged(before, membership)
    abort.abort()
  }

  private def sendDisconnectAnnouncement(peer: PeerRef): Unit =
    connections.get(peer.uid) match
        case Some(conn) =>
          conn.send(Envelope.Membership(HyParViewMessage.Disconnect(self.uid))).run {
            case Success(_) => ()
            case Failure(_) => ()
          }
        case None =>
          connectToPeerDetails(
            peer.channelConnectors,
            s"${Uid.unwrap(self.uid)}-leave-${Uid.unwrap(peer.uid)}"
          ).foreach { latent =>
            val tempAbort = Abort()
            latent.prepare(receive(Some(peer.uid))).runIn(tempAbort) {
              case Success(conn) =>
                conn.send(Envelope.Membership(HyParViewMessage.Disconnect(self.uid))).run {
                  case Success(_) => conn.close()
                  case Failure(_) => conn.close()
                }
              case Failure(_) => ()
            }
          }

  private def receive(expectedPeer: Option[Uid]): Receive[Envelope[State]] =
    (conn: Connection[Envelope[State]]) => {
      expectedPeer.foreach(peer => rememberConnection(peer, conn))
      {
        case Success(Envelope.Membership(message)) =>
          val sender = expectedPeer.orElse(connectionToPeer.get(conn)).orElse(inferSender(message))
          sender.foreach(rememberPeerIdentity(_, conn))
          message match
              case HyParViewMessage.Disconnect(peer) => applyTransition(membership.peerLost(peer))
              case _                                 => applyTransition(membership.receive(message))
        case Success(Envelope.Dissemination(message)) =>
          val peer = expectedPeer.orElse(connectionToPeer.get(conn)).orElse(inferDisseminationSender(message))
          peer.foreach { uid =>
            rememberPeerIdentity(uid, conn)
            applyPlumtreeResult(plumtree.handleMessage(Peer(uid), message))
          }
        case Failure(ex) =>
          expectedPeer.orElse(connectionToPeer.get(conn)).foreach { peer =>
            val reason =
              s"incoming connection closed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
            if membership.activeView.contains(peer) then handleDisconnectedPeer(peer, reason)
            else cleanupInactiveConnection(peer, conn, reason)
          }
      }
    }

  private def rememberPeerIdentity(peer: Uid, conn: Connection[Envelope[State]]): Unit =
    rememberConnection(peer, conn)

  private def inferSender(message: HyParViewMessage): Option[Uid] =
    message match
        case HyParViewMessage.Join(newNode)             => Some(newNode.uid)
        case HyParViewMessage.ForwardJoin(_, _, sender) => Some(sender)
        case HyParViewMessage.Neighbor(from, _)         => Some(from.uid)
        case HyParViewMessage.NeighborReply(from, _)    => Some(from)
        case HyParViewMessage.Disconnect(_)             => None
        case HyParViewMessage.Shuffle(_, _, _, sender)  => Some(sender)
        case HyParViewMessage.ShuffleReply(from, _)     => Some(from)

  private def inferDisseminationSender(message: PlumtreeMessage[State]): Option[Uid] =
    message match
        case PlumtreeMessage.Graft(sender, _) => Some(sender)
        case PlumtreeMessage.IHave(sender, _) => Some(sender)
        case PlumtreeMessage.Prune(sender)    => Some(sender)
        case _                                => None

  private def applyTransition(result: Result): Unit = {
    val before = membership
    membership = result.state
    result.actions.foreach {
      case Action.Send(to, message)      => sendMembership(to, message)
      case Action.SendJoin(details, msg) => sendJoin(details, msg)
    }
    syncViewSideEffects(before, membership)
  }

  private def syncViewSideEffects(before: HyParViewStateMachine, after: HyParViewStateMachine): Unit = {
    val removedActive = before.activePeers -- after.activePeers
    val removedAny    =
      (before.activePeers ++ before.passivePeers).map(_.uid) -- (after.activePeers ++ after.passivePeers).map(_.uid)
    val plumtreeBefore = plumtree
    removedAny.foreach { uid =>
      plumtree = plumtree.removePeer(Peer(uid))
    }
    removedActive.foreach(peer => log(s"active removed ${Uid.unwrap(peer.uid)}"))
    (after.activePeers -- before.activePeers).foreach { peer =>
      log(s"active added ${Uid.unwrap(peer.uid)}")
      if connections.contains(peer.uid) then applyPlumtreeResult(plumtree.addPeer(Peer(peer.uid)))
      else startConnection(peer)
    }
    if plumtreeBefore != plumtree then onPeerRolesChanged()
    if before != after then onViewChanged(before, after)
  }

  private def cleanupInactiveConnection(peer: Uid, conn: Connection[Envelope[State]], reason: String): Unit = {
    val removedStored = connections.get(peer).contains(conn)
    if removedStored then connections.remove(peer): Unit
    connectionToPeer.remove(conn)
    if removedStored then
        log(
          s"drop inactive connection peer=${Uid.unwrap(peer)} reason=$reason keptPassive=${membership.passiveView.contains(peer)}"
        )
  }

  private def handleDisconnectedPeer(peer: Uid, reason: String): Unit = {
    val hadMembership = membership.activeView.contains(peer) || membership.passiveView.contains(peer)
    val conn          = connections.remove(peer)
    conn.foreach(connectionToPeer.remove)
    val plumtreeBefore = plumtree
    plumtree = plumtree.removePeer(Peer(peer))
    connecting.remove(peer)
    pendingMembership.remove(peer)
    applyTransition(membership.peerLost(peer))
    if plumtreeBefore != plumtree then onPeerRolesChanged()
    if hadMembership || conn.nonEmpty then {
      log(s"disconnect peer=${Uid.unwrap(peer)} reason=$reason")
      onPeerDisconnected(peer)
    }
  }

  private def rememberConnection(peer: Uid, conn: Connection[Envelope[State]]): Unit = {
    connectionToPeer.update(conn, peer)
    connections.getOrElseUpdate(peer, conn): Unit
  }

  private def connectToPeerDetails(
      details: Set[ChannelConnectDescriptor],
      label: String
  ): Option[LatentConnection[Envelope[State]]] =
    details.iterator.collectFirst(Function.unlift(detail => resolver.connect(detail, label)))

  private def ensurePromotionAttempt(peer: PeerRef): Unit = {
    val existing      = pendingMembership.getOrElse(peer.uid, Vector.empty)
    val highPriority  = membership.activeView.isEmpty
    val alreadyQueued = existing.exists {
      case HyParViewMessage.Neighbor(from, hp) if from.uid == self.uid && hp == highPriority => true
      case _                                                                                 => false
    }
    if !alreadyQueued then
        pendingMembership.update(peer.uid, existing :+ HyParViewMessage.Neighbor(self, highPriority = highPriority))
    startConnection(peer)
  }

  private def startConnection(peer: PeerRef): Unit =
    if !connections.contains(peer.uid) && !connecting.contains(peer.uid) then
        connectToPeerDetails(peer.channelConnectors, s"${Uid.unwrap(self.uid)}->${Uid.unwrap(peer.uid)}") match
            case Some(latent) =>
              connecting += peer.uid
              latent.prepare(receive(Some(peer.uid))).runIn(abort) {
                case Success(conn) =>
                  connecting -= peer.uid
                  rememberConnection(peer.uid, conn)
                  if membership.activeView.contains(peer.uid) then applyPlumtreeResult(plumtree.addPeer(Peer(peer.uid)))
                  pendingMembership.remove(peer.uid).getOrElse(Vector.empty).foreach(sendMembership(peer, _))
                case Failure(ex) =>
                  connecting -= peer.uid
                  pendingMembership.remove(peer.uid)
                  logFailure(s"outgoing connect failed peer=${Uid.unwrap(peer.uid)}", ex)
                  handleDisconnectedPeer(
                    peer.uid,
                    s"outgoing connect failed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
                  )
              }
            case None => log(s"cannot connect to ${Uid.unwrap(peer.uid)} because resolver has no route")

  private def sendMembership(peer: PeerRef, message: HyParViewMessage): Unit =
    if peer.uid != self.uid then
        connections.get(peer.uid) match
            case Some(conn) =>
              conn.send(Envelope.Membership(message)).run {
                case Success(_)  => ()
                case Failure(ex) =>
                  logFailure(s"membership send failed peer=${Uid.unwrap(peer.uid)}", ex)
                  handleDisconnectedPeer(
                    peer.uid,
                    s"membership send failed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
                  )
              }
            case None =>
              pendingMembership.update(peer.uid, pendingMembership.getOrElse(peer.uid, Vector.empty) :+ message)
              startConnection(peer)

  private def sendJoin(details: Set[ChannelConnectDescriptor], message: HyParViewMessage): Unit =
    connectToPeerDetails(details, s"${Uid.unwrap(self.uid)}-join") match
        case Some(latent) =>
          latent.prepare(receive(None)).runIn(abort) {
            case Success(conn) =>
              conn.send(Envelope.Membership(message)).run {
                case Success(_)  => ()
                case Failure(ex) => logFailure("join send failed", ex)
              }
            case Failure(ex) => logFailure("join connection failed", ex)
          }
        case None => log(s"join skipped self=${Uid.unwrap(self.uid)} reason=no route to contact")

  private def applyPlumtreeResult(result: PlumtreeBroadcast.Result[State]): Unit = {
    val rolesChanged = plumtree.peerRoles != result.state.peerRoles
    plumtree = result.state
    if rolesChanged then onPeerRolesChanged()
    result.events.foreach(handlePlumtreeEvent)
  }

  private def handlePlumtreeEvent(event: Event[State]): Unit =
    event match
        case Event.Deliver(payload)      => receiveCallback(payload.data)
        case Disseminate(peers, message) =>
          peers.foreach { peer =>
            connections.get(peer.uid).foreach { conn =>
              conn.send(Envelope.Dissemination(message)).run {
                case Success(_)  => ()
                case Failure(ex) =>
                  logFailure(s"dissemination send failed peer=${Uid.unwrap(peer.uid)}", ex)
                  handleDisconnectedPeer(
                    peer.uid,
                    s"dissemination send failed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
                  )
              }
            }
          }
}
