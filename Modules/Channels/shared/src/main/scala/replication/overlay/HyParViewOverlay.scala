package replication.overlay

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Callback, Sync}
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.{PlumtreeDissemination, ProtocolMessage}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object HyParViewMultiplexed {
  case class PeerRef(uid: Uid, channelConnectors: Set[ChannelConnectDescriptor])

  enum Envelope[State] {
    case Membership(message: HyParViewUnified.HyParViewMessage)
    case Dissemination(message: ProtocolMessage[State])
  }

  def envelopeCodec[State: JsonValueCodec]: JsonValueCodec[Envelope[State]] = JsonCodecMaker.make
  given membershipCodec: JsonValueCodec[HyParViewUnified.HyParViewMessage] = JsonCodecMaker.make
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
      val n = math.max(2, estimatedNetworkSize)
      val active = math.max(3, math.ceil(math.log10(n.toDouble)).toInt + 1)
      val passive = active * 6
      HyParViewConfig(active, passive, active + 1, math.max(1, (active + 1) / 2), active, math.min(3, math.max(1, active - 1)), 4)
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
                                       plumtree: PlumtreeDissemination[State],
                                       localServer: LatentConnection[HyParViewMultiplexed.Envelope[State]],
                                       resolver: ChannelResolver[HyParViewMultiplexed.Envelope[State]],
                                       contactNode: Option[Set[ChannelConnectDescriptor]],
                                       rnd: Random,
                                       config: HyParViewUnified.HyParViewConfig = HyParViewUnified.HyParViewConfig.default,
                                       debug: Boolean = false,
                                       onViewChanged: (HyParViewStateMachine, HyParViewStateMachine) => Unit = (_, _) => (),
                                       onPeerDisconnected: Uid => Unit = (_: Uid) => (),
) {
  import HyParViewMultiplexed.*
  import HyParViewStateMachine.*
  import HyParViewUnified.*

  private val abort = Abort()
  private def log(msg: => String): Unit = if debug then println(s"[hyparview ${Uid.unwrap(self.uid)}] $msg")

  private var membership = HyParViewStateMachine.empty(
    self,
    config,
    (_, upperExclusive) => rnd.nextInt(upperExclusive),
    peer => peer.channelConnectors.exists(resolver.canConnect) || peer.uid == self.uid,
  )

  private val connections = mutable.LinkedHashMap.empty[Uid, Connection[Envelope[State]]]
  private val connectionToPeer = mutable.LinkedHashMap.empty[Connection[Envelope[State]], Uid]
  private val plumtreeAttached = mutable.LinkedHashSet.empty[Uid]
  private val plumtreeIncoming = mutable.LinkedHashMap.empty[Uid, Callback[ProtocolMessage[State]]]
  private val connecting = mutable.LinkedHashSet.empty[Uid]
  private val pendingMembership = mutable.LinkedHashMap.empty[Uid, Vector[HyParViewMessage]]

  def state: HyParViewStateMachine = membership
  def activeView: Set[Uid] = membership.activeView
  def activePeers: Set[PeerRef] = membership.activePeers
  def passiveView: Set[Uid] = membership.passiveView
  def passivePeers: Set[PeerRef] = membership.passivePeers

  def startServer(): Unit =
    localServer.prepare(receive(None)).runIn(abort) {
      case Success(_) => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def join(): Unit = contactNode.foreach(join)

  def join(contact: Set[ChannelConnectDescriptor]): Unit =
    applyTransition(membership.initiateJoin(contact))

  def shuffleTick(): Unit = applyTransition(membership.shuffleTick())
  def discoverPeers(peers: Iterable[PeerRef]): Unit = applyTransition(membership.discoverPeers(peers.toSet))

  def addIncomingConnection(latent: LatentConnection[Envelope[State]]): Unit =
    latent.prepare(receive(None)).runIn(abort) {
      case Success(_) => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def stop(): Unit = {
    val knownPeers = (membership.active.toSet ++ membership.passive.toSet).filterNot(_.uid == self.uid)
    knownPeers.foreach(sendDisconnectAnnouncement)
    connections.values.foreach(_.close())
    connections.clear()
    connectionToPeer.clear()
    plumtreeAttached.clear()
    plumtreeIncoming.clear()
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
        connectToPeerDetails(peer.channelConnectors, s"${Uid.unwrap(self.uid)}-leave-${Uid.unwrap(peer.uid)}").foreach { latent =>
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
            case _                                => applyTransition(membership.receive(message))
        case Success(Envelope.Dissemination(message)) =>
          val peer = expectedPeer.orElse(connectionToPeer.get(conn))
          peer.flatMap(plumtreeIncoming.get).foreach(_.succeed(message))
        case Failure(ex) =>
          expectedPeer.orElse(connectionToPeer.get(conn)).foreach { peer =>
            val reason = s"incoming connection closed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
            if membership.activeView.contains(peer) then handleDisconnectedPeer(peer, reason)
            else cleanupInactiveConnection(peer, conn, reason)
          }
      }
    }

  private def rememberPeerIdentity(peer: Uid, conn: Connection[Envelope[State]]): Unit =
    rememberConnection(peer, conn)

  private def inferSender(message: HyParViewMessage): Option[Uid] =
    message match
      case HyParViewMessage.Join(newNode)                 => Some(newNode.uid)
      case HyParViewMessage.ForwardJoin(_, _, sender)     => Some(sender)
      case HyParViewMessage.Neighbor(from, _)             => Some(from.uid)
      case HyParViewMessage.NeighborReply(from, _)        => Some(from)
      case HyParViewMessage.Disconnect(_)                 => None
      case HyParViewMessage.Shuffle(_, _, _, sender)      => Some(sender)
      case HyParViewMessage.ShuffleReply(from, _)         => Some(from)

  private def applyTransition(result: Result): Unit = {
    val before = membership
    membership = result.state
    result.actions.foreach {
      case Action.Send(to, message)       => sendMembership(to, message)
      case Action.SendJoin(details, msg)  => sendJoin(details, msg)
    }
    syncViewSideEffects(before, membership)
  }

  private def syncViewSideEffects(before: HyParViewStateMachine, after: HyParViewStateMachine): Unit = {
    val removedActive = before.activePeers -- after.activePeers
    val removedAny = (before.activePeers ++ before.passivePeers).map(_.uid) -- (after.activePeers ++ after.passivePeers).map(_.uid)
    removedAny.foreach { uid =>
      plumtreeAttached.remove(uid)
      plumtreeIncoming.remove(uid)
    }
    removedActive.foreach(peer => log(s"active removed ${Uid.unwrap(peer.uid)}"))
    (after.activePeers -- before.activePeers).foreach { peer =>
      log(s"active added ${Uid.unwrap(peer.uid)}")
      connections.get(peer.uid).foreach { conn =>
        rememberConnection(peer.uid, conn)
        attachPlumtree(peer.uid, conn)
      }
      if !connections.contains(peer.uid) then startConnection(peer)
    }
    if before != after then onViewChanged(before, after)
  }

  private def cleanupInactiveConnection(peer: Uid, conn: Connection[Envelope[State]], reason: String): Unit = {
    val removedStored = connections.get(peer).contains(conn)
    if removedStored then connections.remove(peer): Unit
    connectionToPeer.remove(conn)
    if removedStored then log(s"drop inactive connection peer=${Uid.unwrap(peer)} reason=$reason keptPassive=${membership.passiveView.contains(peer)}")
  }

  private def handleDisconnectedPeer(peer: Uid, reason: String): Unit = {
    val hadMembership = membership.activeView.contains(peer) || membership.passiveView.contains(peer)
    val conn = connections.remove(peer)
    conn.foreach(connectionToPeer.remove)
    plumtreeAttached.remove(peer)
    plumtreeIncoming.remove(peer)
    connecting.remove(peer)
    pendingMembership.remove(peer)
    applyTransition(membership.peerLost(peer))
    if hadMembership || conn.nonEmpty then {
      log(s"disconnect peer=${Uid.unwrap(peer)} reason=$reason")
      onPeerDisconnected(peer)
    }
  }

  private def attachPlumtree(peer: Uid, conn: Connection[Envelope[State]]): Unit =
    if !plumtreeAttached.contains(peer) then
      plumtreeAttached += peer
      val latent = new LatentConnection[ProtocolMessage[State]] {
        override def prepare(receiver: Receive[ProtocolMessage[State]]): Async[Abort, Connection[ProtocolMessage[State]]] =
          Sync {
            val wrapped = new Connection[ProtocolMessage[State]] {
              override def info: ConnectionInfo = conn.info
              override def authenticatedPeerReplicaId: Option[Uid] = Some(peer)
              override def send(message: ProtocolMessage[State]) = conn.send(Envelope.Dissemination(message))
              override def close(): Unit = conn.close()
            }
            plumtreeIncoming.update(peer, receiver.messageHandler(wrapped))
            wrapped
          }
      }
      plumtree.addObjectConnection(latent)

  private def rememberConnection(peer: Uid, conn: Connection[Envelope[State]]): Unit = {
    connectionToPeer.update(conn, peer)
    connections.getOrElseUpdate(peer, conn)
    if membership.activeView.contains(peer) then attachPlumtree(peer, connections(peer))
  }

  private def connectToPeerDetails(details: Set[ChannelConnectDescriptor], label: String): Option[LatentConnection[Envelope[State]]] =
    details.iterator.collectFirst(Function.unlift(detail => resolver.connect(detail, label)))

  private def startConnection(peer: PeerRef): Unit =
    if !connections.contains(peer.uid) && !connecting.contains(peer.uid) then
      connectToPeerDetails(peer.channelConnectors, s"${Uid.unwrap(self.uid)}->${Uid.unwrap(peer.uid)}") match
        case Some(latent) =>
          connecting += peer.uid
          latent.prepare(receive(Some(peer.uid))).runIn(abort) {
            case Success(conn) =>
              connecting -= peer.uid
              rememberConnection(peer.uid, conn)
              if membership.activeView.contains(peer.uid) then attachPlumtree(peer.uid, connections(peer.uid))
              pendingMembership.remove(peer.uid).getOrElse(Vector.empty).foreach(sendMembership(peer, _))
            case Failure(ex) =>
              connecting -= peer.uid
              pendingMembership.remove(peer.uid)
              ex.printStackTrace()
              handleDisconnectedPeer(peer.uid, s"outgoing connect failed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}")
          }
        case None => log(s"cannot connect to ${Uid.unwrap(peer.uid)} because resolver has no route")

  private def sendMembership(peer: PeerRef, message: HyParViewMessage): Unit =
    if peer.uid != self.uid then
      connections.get(peer.uid) match
        case Some(conn) =>
          conn.send(Envelope.Membership(message)).run {
            case Success(_) => ()
            case Failure(ex) =>
              ex.printStackTrace()
              handleDisconnectedPeer(peer.uid, s"membership send failed: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}")
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
              case Success(_) => ()
              case Failure(ex) => ex.printStackTrace()
            }
          case Failure(ex) => ex.printStackTrace()
        }
      case None => log(s"join skipped self=${Uid.unwrap(self.uid)} reason=no route to contact")
}
