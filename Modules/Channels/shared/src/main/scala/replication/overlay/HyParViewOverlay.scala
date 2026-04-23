package replication.overlay

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import de.rmgk.delay.{Async, Callback, Sync}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.{PlumtreeDissemination, ProtocolMessage}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object HyParViewMultiplexed {

  case class PeerRef[Details](uid: Uid, details: Details)

  enum Envelope[State, Details] {
    case Membership(message: HyParViewUnified.HyParViewMessage[Details])
    case Dissemination(message: ProtocolMessage[State])
  }

  def envelopeCodec[State: JsonValueCodec, Details: JsonValueCodec]
      : JsonValueCodec[Envelope[State, Details]] = JsonCodecMaker.make

  def membershipCodec[Details: JsonValueCodec]
      : JsonValueCodec[HyParViewUnified.HyParViewMessage[Details]] = JsonCodecMaker.make
}

object HyParViewUnified {

  /** Configuration for simplified HyParView overlay maintenance. */
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
      val active = math.max(2, math.ceil(math.log(n.toDouble) / math.log(2)).toInt + 1)
      val passive = active * 6

      HyParViewConfig(
        activeViewSize = active,
        passiveViewSize = passive,
        activeRandomWalkLength = active + 1,
        passiveRandomWalkLength = math.max(1, (active + 1) / 2),
        shuffleRandomWalkLength = active,
        shuffleActiveSample = math.min(3, math.max(1, active - 1)),
        shufflePassiveSample = 4
      )
    }
  }

  enum HyParViewMessage[Details] {
    case Join(newNode: HyParViewMultiplexed.PeerRef[Details])
    case ForwardJoin(newNode: HyParViewMultiplexed.PeerRef[Details], ttl: Int, sender: Uid)
    case Neighbor(from: HyParViewMultiplexed.PeerRef[Details], highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(
        origin: HyParViewMultiplexed.PeerRef[Details],
        sample: Set[HyParViewMultiplexed.PeerRef[Details]],
        ttl: Int,
        sender: Uid
    )
    case ShuffleReply(from: Uid, sample: Set[HyParViewMultiplexed.PeerRef[Details]])
  }
}

class HyParViewMultiplexedNode[State, Details](
    val self: HyParViewMultiplexed.PeerRef[Details],
    plumtree: PlumtreeDissemination[State],
    localServer: LatentConnection[HyParViewMultiplexed.Envelope[State, Details]],
    resolver: ConnectionDetailsResolver[Details, HyParViewMultiplexed.Envelope[State, Details]],
    contactNode: Option[Details],
    rnd: Random,
    config: HyParViewUnified.HyParViewConfig = HyParViewUnified.HyParViewConfig.default,
    debug: Boolean = false,
) {
  import HyParViewMultiplexed.*
  import HyParViewUnified.*
  import HyParViewUnified.HyParViewMessage.*

  private val abort = Abort()

  private def log(msg: => String): Unit =
    if debug then println(s"[hyparview ${Uid.unwrap(self.uid)}] $msg")

  private val active: mutable.LinkedHashMap[Uid, PeerRef[Details]]  = mutable.LinkedHashMap.empty
  private val passive: mutable.LinkedHashMap[Uid, PeerRef[Details]] = mutable.LinkedHashMap.empty
  private val connections: mutable.LinkedHashMap[Uid, Connection[Envelope[State, Details]]] = mutable.LinkedHashMap.empty
  private val connectionToPeer: mutable.LinkedHashMap[Connection[Envelope[State, Details]], Uid] = mutable.LinkedHashMap.empty
  private val plumtreeAttached: mutable.Set[Uid] = mutable.LinkedHashSet.empty
  private val plumtreeIncoming: mutable.LinkedHashMap[Uid, Callback[ProtocolMessage[State]]] = mutable.LinkedHashMap.empty

  def activeView: Set[Uid]  = active.keySet.toSet
  def passiveView: Set[Uid] = passive.keySet.toSet

  def startServer(): Unit =
    localServer.prepare(receive(None)).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def join(): Unit =
    contactNode.foreach { contact =>
      resolver.connect(contact, s"${Uid.unwrap(self.uid)}-join").foreach { latent =>
        latent.prepare(receive(None)).runIn(abort) {
          case Success(conn) => conn.send(Envelope.Membership(Join(self))).run(_ => ())
          case Failure(ex)   => ex.printStackTrace()
        }
      }
    }

  def shuffleTick(): Unit =
    if active.nonEmpty then
      val target = randomElement(active.keySet.toSet)
      val sample = Set(self) ++ randomSubset(active.values.toSet, config.shuffleActiveSample) ++ randomSubset(passive.values.toSet, config.shufflePassiveSample)
      sendMembership(target, Shuffle(self, sample, config.shuffleRandomWalkLength, self.uid))

  private def receive(expectedPeer: Option[Uid]): Receive[Envelope[State, Details]] =
    (conn: Connection[Envelope[State, Details]]) => {
      expectedPeer.foreach(peer => rememberConnection(peer, conn))
      {
        case Success(Envelope.Membership(message))    => handleMembership(message, conn)
        case Success(Envelope.Dissemination(message)) =>
          val peer = expectedPeer.orElse(connectionToPeer.get(conn))
          peer.flatMap(plumtreeIncoming.get).foreach(_.succeed(message))
        case Failure(_)                               => ()
      }
    }

  private def attachPlumtree(peer: Uid, conn: Connection[Envelope[State, Details]]): Unit =
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
      log(s"attach plumtree peer=${Uid.unwrap(peer)}")
      plumtree.addObjectConnection(latent)

  private def rememberConnection(peer: Uid, conn: Connection[Envelope[State, Details]]): Unit = {
    connectionToPeer.update(conn, peer)
    connections.getOrElseUpdate(peer, conn)
    if active.contains(peer) then attachPlumtree(peer, connections(peer))
  }

  private def ensureConnection(peer: PeerRef[Details]): Connection[Envelope[State, Details]] =
    connections.getOrElseUpdate(peer.uid, {
      val latent = resolver.connect(peer.details, s"${Uid.unwrap(self.uid)}->${Uid.unwrap(peer.uid)}")
        .getOrElse(throw IllegalStateException(s"cannot resolve connection details $peer"))
      var result: Option[Connection[Envelope[State, Details]]] = None
      latent.prepare(receive(Some(peer.uid))).runIn(abort) {
        case Success(conn) => result = Some(conn)
        case Failure(ex)   => throw ex
      }
      result.getOrElse(throw IllegalStateException(s"failed to connect ${self.uid.show} -> ${peer.uid.show}"))
    })

  private def sendMembership(peer: PeerRef[Details], message: HyParViewMessage[Details]): Unit =
    if peer.uid != self.uid then
      log(s"send ${message.getClass.getSimpleName} -> ${Uid.unwrap(peer.uid)}")
      val conn = ensureConnection(peer)
      conn.send(Envelope.Membership(message)).run {
        case Success(_)  => ()
        case Failure(ex) => ex.printStackTrace()
      }

  private def sendMembership(peerUid: Uid, message: HyParViewMessage[Details]): Unit =
    if peerUid != self.uid then
      active.get(peerUid).orElse(passive.get(peerUid)) match
        case Some(peer) => sendMembership(peer, message)
        case None       => log(s"drop ${message.getClass.getSimpleName} -> unknown ${Uid.unwrap(peerUid)}")

  private def handleMembership(msg: HyParViewMessage[Details], conn: Connection[Envelope[State, Details]]): Unit =
    msg match
      case Join(newNode) =>
        log(s"recv Join from ${Uid.unwrap(newNode.uid)}")
        rememberAdvertisedPeer(newNode)
        rememberConnection(newNode.uid, conn)
        addNodeActiveView(newNode)
        sendMembership(newNode.uid, Neighbor(self, highPriority = true))
        active.keys.filterNot(_ == newNode.uid).foreach { n =>
          sendMembership(n, ForwardJoin(newNode, config.activeRandomWalkLength, self.uid))
        }

      case ForwardJoin(newNode, ttl, sender) =>
        log(s"recv ForwardJoin new=${Uid.unwrap(newNode.uid)} ttl=$ttl sender=${Uid.unwrap(sender)}")
        rememberAdvertisedPeer(newNode)
        rememberConnection(sender, conn)
        if newNode.uid != self.uid then
          if ttl == 0 || active.size <= 1 then
            addNodeActiveView(newNode)
            sendMembership(newNode.uid, Neighbor(self, highPriority = true))
          else
            if ttl == config.passiveRandomWalkLength then addNodePassiveView(newNode)
            val next = active.keySet.filterNot(_ == sender)
            if next.nonEmpty then sendMembership(randomElement(next.toSet), ForwardJoin(newNode, ttl - 1, self.uid))
            else
              addNodeActiveView(newNode)
              sendMembership(newNode.uid, Neighbor(self, highPriority = true))

      case Neighbor(from, highPriority) =>
        log(s"recv Neighbor from=${Uid.unwrap(from.uid)} high=$highPriority")
        rememberAdvertisedPeer(from)
        rememberConnection(from.uid, conn)
        val accepted = highPriority || active.size < config.activeViewSize
        if accepted then addNodeActiveView(from)
        sendMembership(from.uid, NeighborReply(self.uid, accepted))

      case NeighborReply(from, accepted) =>
        log(s"recv NeighborReply from=${Uid.unwrap(from)} accepted=$accepted")
        active.get(from).orElse(passive.get(from)).foreach { peer =>
          if accepted then addNodeActiveView(peer)
          else addNodePassiveView(peer)
        }

      case Disconnect(peer) =>
        log(s"recv Disconnect peer=${Uid.unwrap(peer)}")
        if active.contains(peer) then
          val dropped = active.remove(peer).get
          addNodePassiveView(dropped)
          healActiveView()

      case Shuffle(origin, sample, ttl, sender) =>
        log(s"recv Shuffle origin=${Uid.unwrap(origin.uid)} ttl=$ttl sender=${Uid.unwrap(sender)} sample=${sample.map(p => Uid.unwrap(p.uid))}")
        rememberAdvertisedPeer(origin)
        rememberConnection(sender, conn)
        sample.foreach(rememberAdvertisedPeer)
        sample.foreach(addNodePassiveView)
        if ttl > 0 && active.size > 1 then
          val next = active.keySet.filterNot(_ == sender)
          if next.nonEmpty then sendMembership(randomElement(next.toSet), Shuffle(origin, sample, ttl - 1, self.uid))
          else acceptShuffle(origin, sample)
        else
          acceptShuffle(origin, sample)

      case ShuffleReply(from, sample) =>
        log(s"recv ShuffleReply from=${Uid.unwrap(from)} sample=${sample.map(p => Uid.unwrap(p.uid))}")
        sample.foreach(addNodePassiveView)

  private def acceptShuffle(origin: PeerRef[Details], incomingSample: Set[PeerRef[Details]]): Unit = {
    val replySample = randomSubset(passive.values.toSet, incomingSample.size)
    sendMembership(origin.uid, ShuffleReply(self.uid, replySample))
    incomingSample.foreach(addNodePassiveView)
  }

  private def healActiveView(): Unit = {
    log(s"healActiveView active=${active.keySet.map(Uid.unwrap)} passive=${passive.keySet.map(Uid.unwrap)}")
    var attempts = 0
    while active.size < config.activeViewSize && passive.nonEmpty && attempts < passive.size do
      val candidateId = randomElement(passive.keySet.toSet)
      val candidate   = passive(candidateId)
      passive.remove(candidateId)
      log(s"heal attempt -> ${Uid.unwrap(candidateId)} high=${active.isEmpty}")
      sendMembership(candidate, Neighbor(self, highPriority = active.isEmpty))
      attempts += 1
  }

  private def rememberAdvertisedPeer(peer: PeerRef[Details]): Unit = {
    if peer.uid != self.uid then
      if !active.contains(peer.uid) && !passive.contains(peer.uid) then
        log(s"remember peer ${Uid.unwrap(peer.uid)} as passive")
      passive.getOrElseUpdate(peer.uid, peer): Unit
  }

  private def addNodeActiveView(node: PeerRef[Details]): Unit =
    if node.uid != self.uid && !active.contains(node.uid) then
      if active.size >= config.activeViewSize then dropRandomElementFromActiveView()
      log(s"add active ${Uid.unwrap(node.uid)}")
      active.update(node.uid, node)
      passive.remove(node.uid)
      val conn = ensureConnection(node)
      rememberConnection(node.uid, conn)
      attachPlumtree(node.uid, conn)

  private def addNodePassiveView(node: PeerRef[Details]): Unit =
    if node.uid != self.uid && !active.contains(node.uid) && !passive.contains(node.uid) then
      if passive.size >= config.passiveViewSize then
        passive.remove(randomElement(passive.keySet.toSet)): Unit
      log(s"add passive ${Uid.unwrap(node.uid)}")
      passive.update(node.uid, node)

  private def dropRandomElementFromActiveView(): Unit =
    if active.nonEmpty then
      val n    = randomElement(active.keySet.toSet)
      val peer = active.remove(n).get
      log(s"drop active ${Uid.unwrap(n)}")
      addNodePassiveView(peer)
      sendMembership(n, Disconnect(self.uid))

  private def randomElement[A](set: collection.Set[A]): A =
    set.iterator.drop(rnd.nextInt(set.size)).next()

  private def randomSubset[A](set: Set[A], maxSize: Int): Set[A] =
    rnd.shuffle(set.toList).take(maxSize).toSet
}
