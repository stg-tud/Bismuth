package replication.overlay

import channels.{Abort, Connection, QueuedLocalConnection, Receive}
import rdts.base.Uid
import replication.{PlumtreeDissemination, ProtocolMessage}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object HyParViewOverlay {

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

  enum HyParViewMessage {
    case Join(newNode: Uid)
    case ForwardJoin(newNode: Uid, ttl: Int, sender: Uid)
    case Neighbor(from: Uid, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(origin: Uid, sample: Set[Uid], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[Uid])
  }

  def edgeKey(a: Uid, b: Uid): (Uid, Uid) =
    if Uid.unwrap(a) <= Uid.unwrap(b) then (a, b) else (b, a)
}

/** Simplified HyParView-style overlay for tests/evaluation.
  *
  * Based on HyParView / "HyParView: a membership protocol for reliable gossip-based broadcast"
  * (Leitão, Pereira, Rodrigues, 2007): active/passive views, join random walks, and periodic shuffle.
  *
  * This implementation directly wires active neighbors into the given [[PlumtreeDissemination]] instance.
  */
class HyParViewOverlayNode[State](
    val id: Uid,
    controlNetwork: Map[Uid, QueuedLocalConnection[HyParViewOverlay.HyParViewMessage]],
    dataNetwork: Map[(Uid, Uid), QueuedLocalConnection[ProtocolMessage[State]]],
    plumtree: PlumtreeDissemination[State],
    contactNode: Option[Uid],
    rnd: Random,
    config: HyParViewOverlay.HyParViewConfig = HyParViewOverlay.HyParViewConfig.default,
) {
  import HyParViewOverlay.*
  import HyParViewMessage.*

  private val abort = Abort()

  private val active: mutable.Set[Uid]  = mutable.LinkedHashSet.empty
  private val passive: mutable.Set[Uid] = mutable.LinkedHashSet.empty

  private val outgoingControl: mutable.Map[Uid, Connection[HyParViewMessage]] = mutable.Map.empty
  private val connectedDataPeers: mutable.Set[Uid] = mutable.LinkedHashSet.empty

  def activeView: Set[Uid]  = active.toSet
  def passiveView: Set[Uid] = passive.toSet

  private val receive: Receive[HyParViewMessage] = (_: Connection[HyParViewMessage]) => {
    case Success(msg) => handleMessage(msg)
    case Failure(_)   => ()
  }

  def startServer(): Unit =
    controlNetwork(id).server.prepare(receive).runIn(abort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

  def join(): Unit =
    contactNode.foreach { contact =>
      if contact != id then sendControl(contact, Join(id))
    }

  def shuffleTick(): Unit =
    if active.nonEmpty then
      val target = randomElement(active)
      val sample = Set(id) ++ randomSubset(active.toSet, config.shuffleActiveSample) ++ randomSubset(passive.toSet, config.shufflePassiveSample)
      sendControl(target, Shuffle(id, sample, config.shuffleRandomWalkLength, id))

  private def handleMessage(msg: HyParViewMessage): Unit =
    msg match
      case Join(newNode) =>
        addNodeActiveView(newNode)
        // ensure symmetric active relation with joiner
        sendControl(newNode, Neighbor(id, highPriority = true))
        active.filterNot(_ == newNode).foreach { n =>
          sendControl(n, ForwardJoin(newNode, config.activeRandomWalkLength, id))
        }

      case ForwardJoin(newNode, ttl, sender) =>
        if newNode != id then
          if ttl == 0 || active.size <= 1 then
            addNodeActiveView(newNode)
            sendControl(newNode, Neighbor(id, highPriority = true))
          else
            if ttl == config.passiveRandomWalkLength then addNodePassiveView(newNode)
            val next = active.filterNot(_ == sender)
            if next.nonEmpty then sendControl(randomElement(next), ForwardJoin(newNode, ttl - 1, id))
            else
              addNodeActiveView(newNode)
              sendControl(newNode, Neighbor(id, highPriority = true))

      case Neighbor(from, highPriority) =>
        val accepted = highPriority || active.size < config.activeViewSize
        if accepted then addNodeActiveView(from)
        sendControl(from, NeighborReply(id, accepted))

      case NeighborReply(from, accepted) =>
        if accepted then addNodeActiveView(from)
        else addNodePassiveView(from)

      case Disconnect(peer) =>
        if active.contains(peer) then
          active -= peer
          addNodePassiveView(peer)
          healActiveView()

      case Shuffle(origin, sample, ttl, sender) =>
        if ttl > 0 && active.size > 1 then
          val next = active.filterNot(_ == sender)
          if next.nonEmpty then sendControl(randomElement(next), Shuffle(origin, sample, ttl - 1, id))
          else acceptShuffle(origin, sample)
        else
          acceptShuffle(origin, sample)

      case ShuffleReply(_, sample) =>
        integratePassiveSample(sample)

  private def acceptShuffle(origin: Uid, incomingSample: Set[Uid]): Unit =
    val replySample = randomSubset(passive.toSet, incomingSample.size)
    sendControl(origin, ShuffleReply(id, replySample))
    integratePassiveSample(incomingSample)

  private def healActiveView(): Unit =
    while active.size < config.activeViewSize && passive.nonEmpty do
      val candidate = randomElement(passive)
      passive -= candidate
      sendControl(candidate, Neighbor(id, highPriority = active.isEmpty))

  private def sendControl(peer: Uid, message: HyParViewMessage): Unit =
    if peer != id then
      val conn = outgoingControl.getOrElseUpdate(peer, connectControl(peer))
      conn.send(message).run {
        case Success(_)  => ()
        case Failure(ex) => ex.printStackTrace()
      }

  private def connectControl(peer: Uid): Connection[HyParViewMessage] =
    var connected: Option[Connection[HyParViewMessage]] = None
    controlNetwork(peer).client(Uid.unwrap(id)).prepare(receive).runIn(abort) {
      case Success(conn) => connected = Some(conn)
      case Failure(ex)   => throw ex
    }
    connected.getOrElse(throw IllegalStateException(s"failed to connect ${id.show} -> ${peer.show}"))

  private def ensureDataConnection(peer: Uid): Unit =
    if peer != id && !connectedDataPeers.contains(peer) then
      connectedDataPeers += peer
      val key  = HyParViewOverlay.edgeKey(id, peer)
      val link = dataNetwork(key)
      if Uid.unwrap(id) <= Uid.unwrap(peer) then
        plumtree.addObjectConnection(link.server)
      else
        plumtree.addObjectConnection(link.client(s"${Uid.unwrap(id)}->${Uid.unwrap(peer)}"))

  private def addNodeActiveView(node: Uid): Unit =
    if node != id && !active.contains(node) then
      if active.size >= config.activeViewSize then dropRandomElementFromActiveView()
      active += node
      passive -= node
      outgoingControl.getOrElseUpdate(node, connectControl(node))
      ensureDataConnection(node)

  private def addNodePassiveView(node: Uid): Unit =
    if node != id && !active.contains(node) && !passive.contains(node) then
      if passive.size >= config.passiveViewSize then passive -= randomElement(passive)
      passive += node

  private def dropRandomElementFromActiveView(): Unit =
    if active.nonEmpty then
      val n = randomElement(active)
      active -= n
      addNodePassiveView(n)
      sendControl(n, Disconnect(id))

  private def integratePassiveSample(sample: Set[Uid]): Unit =
    sample.foreach(addNodePassiveView)

  private def randomElement(set: collection.Set[Uid]): Uid =
    set.iterator.drop(rnd.nextInt(set.size)).next()

  private def randomSubset(set: Set[Uid], maxSize: Int): Set[Uid] =
    rnd.shuffle(set.toList).take(maxSize).toSet
}
