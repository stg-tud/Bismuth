package replication.example

import channels.{Abort, Connection, LocalMessageQueue, QueuedLocalConnection, Receive, TestUtil}
import rdts.base.Uid

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class HyParViewQueuedTest extends munit.FunSuite {

  enum HyParViewMessage {
    case Join(newNode: Uid)
    case ForwardJoin(newNode: Uid, ttl: Int, sender: Uid)
    case Neighbor(from: Uid, highPriority: Boolean)
    case NeighborReply(from: Uid, accepted: Boolean)
    case Disconnect(peer: Uid)
    case Shuffle(origin: Uid, sample: Set[Uid], ttl: Int, sender: Uid)
    case ShuffleReply(from: Uid, sample: Set[Uid])

    // Plumtree overlay messages
    case Gossip(messageId: String, payload: String, round: Int, sender: Uid)
    case IHave(messageId: String, round: Int, sender: Uid)
    case Graft(messageId: String, round: Int, sender: Uid)
    case Prune(sender: Uid)
  }
  import HyParViewMessage.*

  class HyParViewNode(
      val id: Uid,
      network: Map[Uid, QueuedLocalConnection[HyParViewMessage]],
      contactNode: Option[Uid],
      rnd: Random,
      activeViewSize: Int = 5,
      passiveViewSize: Int = 30,
      activeRandomWalkLength: Int = 6,
      passiveRandomWalkLength: Int = 3,
      shuffleRandomWalkLength: Int = 5,
      shuffleActiveSample: Int = 3,
      shufflePassiveSample: Int = 4,
  ) {

    private val abort = Abort()

    // HyParView state
    private val active: mutable.Set[Uid]  = mutable.LinkedHashSet.empty
    private val passive: mutable.Set[Uid] = mutable.LinkedHashSet.empty
    private val outgoing: mutable.Map[Uid, Connection[HyParViewMessage]] = mutable.Map.empty

    // Plumtree state (single shared tree, simplified)
    private val eagerPushPeers: mutable.Set[Uid] = mutable.LinkedHashSet.empty
    private val lazyPushPeers: mutable.Set[Uid]  = mutable.LinkedHashSet.empty
    private val receivedMsgs: mutable.Set[String] = mutable.LinkedHashSet.empty
    private val payloadById: mutable.Map[String, String] = mutable.Map.empty
    private var localMsgSeq: Long = 0

    def activeView: Set[Uid]      = active.toSet
    def passiveView: Set[Uid]     = passive.toSet
    def overlayTreeView: Set[Uid] = eagerPushPeers.toSet

    def deliveredCount: Int = receivedMsgs.size
    def hasDelivered(messageId: String): Boolean = receivedMsgs.contains(messageId)

    private val receive: Receive[HyParViewMessage] = (_: Connection[HyParViewMessage]) => {
      case Success(msg) => handleMessage(msg)
      case Failure(_)   => ()
    }

    def startServer(): Unit =
      network(id).server.prepare(receive).runIn(abort)(TestUtil.printErrors(_ => ()))

    def join(): Unit =
      contactNode.foreach { contact =>
        if contact != id then sendTo(contact, Join(id))
      }

    def shuffleTick(): Unit =
      if active.nonEmpty then
        val target = randomElement(active)
        val sample = Set(id) ++ randomSubset(active.toSet, shuffleActiveSample) ++ randomSubset(passive.toSet, shufflePassiveSample)
        sendTo(target, Shuffle(id, sample, shuffleRandomWalkLength, id))

    def plumtreeBroadcast(payload: String): String = {
      localMsgSeq += 1
      val msgId = s"${Uid.unwrap(id)}:$localMsgSeq"
      receivedMsgs += msgId
      payloadById += (msgId -> payload)
      eagerPush(msgId, payload, round = 0, except = id)
      lazyPush(msgId, round = 0, except = id)
      msgId
    }

    private def handleMessage(msg: HyParViewMessage): Unit =
      msg match
        // -------------------- HyParView --------------------
        case Join(newNode) =>
          addNodeActiveView(newNode)
          active.filterNot(_ == newNode).foreach { n =>
            sendTo(n, ForwardJoin(newNode, activeRandomWalkLength, id))
          }

        case ForwardJoin(newNode, ttl, sender) =>
          if newNode != id then
            if ttl == 0 || active.size <= 1 then
              addNodeActiveView(newNode)
              sendTo(newNode, Neighbor(id, highPriority = true))
            else
              if ttl == passiveRandomWalkLength then addNodePassiveView(newNode)
              val next = active.filterNot(_ == sender)
              if next.nonEmpty then sendTo(randomElement(next), ForwardJoin(newNode, ttl - 1, id))
              else
                addNodeActiveView(newNode)
                sendTo(newNode, Neighbor(id, highPriority = true))

        case Neighbor(from, highPriority) =>
          val accepted = highPriority || active.size < activeViewSize
          if accepted then addNodeActiveView(from)
          sendTo(from, NeighborReply(id, accepted))

        case NeighborReply(from, accepted) =>
          if accepted then addNodeActiveView(from)
          else addNodePassiveView(from)

        case Disconnect(peer) =>
          if active.contains(peer) then
            active -= peer
            addNodePassiveView(peer)
            eagerPushPeers -= peer
            lazyPushPeers -= peer
            healActiveView()

        case Shuffle(origin, sample, ttl, sender) =>
          if ttl > 0 && active.size > 1 then
            val next = active.filterNot(_ == sender)
            if next.nonEmpty then sendTo(randomElement(next), Shuffle(origin, sample, ttl - 1, id))
            else acceptShuffle(origin, sample)
          else
            acceptShuffle(origin, sample)

        case ShuffleReply(_, sample) =>
          integratePassiveSample(sample)

        // -------------------- Plumtree --------------------
        case Gossip(messageId, payload, round, sender) =>
          if !receivedMsgs.contains(messageId) then
            receivedMsgs += messageId
            payloadById += (messageId -> payload)
            // first reception: keep sender eager, forward eager+lazy
            eagerPushPeers += sender
            lazyPushPeers -= sender
            eagerPush(messageId, payload, round + 1, except = sender)
            lazyPush(messageId, round + 1, except = sender)
          else
            // duplicate: prune this edge for eager dissemination
            eagerPushPeers -= sender
            lazyPushPeers += sender
            sendTo(sender, Prune(id))

        case IHave(messageId, round, sender) =>
          if !receivedMsgs.contains(messageId) then
            // simplified: immediate graft (no timers)
            sendTo(sender, Graft(messageId, round, id))

        case Graft(messageId, round, sender) =>
          eagerPushPeers += sender
          lazyPushPeers -= sender
          payloadById.get(messageId).foreach { payload =>
            sendTo(sender, Gossip(messageId, payload, round + 1, id))
          }

        case Prune(sender) =>
          eagerPushPeers -= sender
          lazyPushPeers += sender

    private def eagerPush(messageId: String, payload: String, round: Int, except: Uid): Unit =
      eagerPushPeers.filterNot(_ == except).foreach { p =>
        sendTo(p, Gossip(messageId, payload, round, id))
      }

    private def lazyPush(messageId: String, round: Int, except: Uid): Unit =
      lazyPushPeers.filterNot(_ == except).foreach { p =>
        sendTo(p, IHave(messageId, round, id))
      }

    private def acceptShuffle(origin: Uid, incomingSample: Set[Uid]): Unit =
      val replySample = randomSubset(passive.toSet, incomingSample.size)
      sendTo(origin, ShuffleReply(id, replySample))
      integratePassiveSample(incomingSample)

    private def healActiveView(): Unit =
      while active.size < activeViewSize && passive.nonEmpty do
        val candidate = randomElement(passive)
        passive -= candidate
        sendTo(candidate, Neighbor(id, highPriority = active.isEmpty))

    private def sendTo(peer: Uid, message: HyParViewMessage): Unit =
      if peer != id then
        val conn = outgoing.getOrElseUpdate(peer, connectTo(peer))
        conn.send(message).run(TestUtil.printErrors(_ => ()))

    private def connectTo(peer: Uid): Connection[HyParViewMessage] =
      var connected: Option[Connection[HyParViewMessage]] = None
      network(peer).client(Uid.unwrap(id)).prepare(receive).runIn(abort) {
        case Success(conn) => connected = Some(conn)
        case Failure(ex)   => throw ex
      }
      connected.getOrElse(throw IllegalStateException(s"failed to connect ${id.show} -> ${peer.show}"))

    private def addNodeActiveView(node: Uid): Unit =
      if node != id && !active.contains(node) then
        if active.size >= activeViewSize then dropRandomElementFromActiveView()
        active += node
        passive -= node

        // New active links start eager in Plumtree.
        eagerPushPeers += node
        lazyPushPeers -= node

        outgoing.getOrElseUpdate(node, connectTo(node))

    private def addNodePassiveView(node: Uid): Unit =
      if node != id && !active.contains(node) && !passive.contains(node) then
        if passive.size >= passiveViewSize then passive -= randomElement(passive)
        passive += node

    private def dropRandomElementFromActiveView(): Unit =
      if active.nonEmpty then
        val n = randomElement(active)
        active -= n
        addNodePassiveView(n)

        // Removed active links are no longer in Plumtree neighbor sets.
        eagerPushPeers -= n
        lazyPushPeers -= n

        sendTo(n, Disconnect(id))

    private def integratePassiveSample(sample: Set[Uid]): Unit =
      sample.foreach(addNodePassiveView)

    private def randomElement(set: collection.Set[Uid]): Uid =
      set.iterator.drop(rnd.nextInt(set.size)).next()

    private def randomSubset(set: Set[Uid], maxSize: Int): Set[Uid] =
      rnd.shuffle(set.toList).take(maxSize).toSet
  }

  test("hyparview + plumtree queued setup with 1000 nodes") {
    val n      = 100
    val random = Random(42)
    val queue  = LocalMessageQueue[HyParViewMessage]()
    val ids    = Vector.tabulate(n)(i => Uid.predefined(s"n$i"))

    val transports = ids.map(id => id -> QueuedLocalConnection[HyParViewMessage](queue)).toMap

    val nodes = ids.zipWithIndex.map { case (id, idx) =>
      val contact = if idx == 0 then None else Some(Uid.predefined("n0"))
      HyParViewNode(id, transports, contact, random)
    }

    nodes.foreach(_.startServer())
    nodes.foreach(_.join())

    var safety = 0
    while queue.nonEmpty && safety < 300000 do
      queue.deliverAll()
      safety += 1

    (0 until 8).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      var innerSafety = 0
      while queue.nonEmpty && innerSafety < 300000 do
        queue.deliverAll()
        innerSafety += 1
    }

    val connectedEnough = nodes.count(_.activeView.nonEmpty)
    assertEquals(connectedEnough, n)
    assert(nodes.forall(_.activeView.size <= 5))
    assert(nodes.forall(_.passiveView.size <= 30))

    // Plumtree overlay usage: single message from one sender, on top of established HyParView active links.
    val root = nodes.head
    val msgId = root.plumtreeBroadcast("hello-plumtree")

    var broadcastSafety = 0
    while queue.nonEmpty && broadcastSafety < 300000 do
      queue.deliverAll()
      broadcastSafety += 1

    assert(nodes.forall(_.hasDelivered(msgId)))

    val dotPath = Paths.get("target", "hyparview-queued-1000.dot")
    Files.createDirectories(dotPath.getParent)
    Files.writeString(
      dotPath,
      renderDot(nodes, showOverlayTree = true, showActiveSet = false, showPassiveSet = false),
      StandardCharsets.UTF_8
    )
  }

  private def renderDot(
      nodes: Seq[HyParViewNode],
      showOverlayTree: Boolean,
      showActiveSet: Boolean,
      showPassiveSet: Boolean
  ): String = {
    def edgeKey(a: Uid, b: Uid): (String, String) = {
      val aa = Uid.unwrap(a)
      val bb = Uid.unwrap(b)
      if aa <= bb then (aa, bb) else (bb, aa)
    }

    val overlayTreeEdges = mutable.LinkedHashSet.empty[(String, String)]
    val activeEdges      = mutable.LinkedHashSet.empty[(String, String)]
    val passiveEdges     = mutable.LinkedHashSet.empty[(String, String)]

    nodes.foreach { n =>
      if showOverlayTree then
        n.overlayTreeView.foreach { to =>
          if to != n.id then overlayTreeEdges += edgeKey(n.id, to)
        }
      if showActiveSet then
        n.activeView.foreach { to =>
          if to != n.id then activeEdges += edgeKey(n.id, to)
        }
      if showPassiveSet then
        n.passiveView.foreach { to =>
          if to != n.id then passiveEdges += edgeKey(n.id, to)
        }
    }

    // Keep only one style per edge with this precedence: overlay tree > active > passive.
    activeEdges --= overlayTreeEdges
    passiveEdges --= overlayTreeEdges
    passiveEdges --= activeEdges

    val b = new StringBuilder
    b.append("graph HyParViewQueued {\n")
    b.append("  graph [overlap=false, splines=true, nodesep=1.0, ranksep=1.2];\n")

    nodes.foreach { n =>
      val id = Uid.unwrap(n.id)
      b.append(s"  \"$id\";\n")
    }

    overlayTreeEdges.foreach { case (a, c) =>
      b.append(s"  \"$a\" -- \"$c\" [color=forestgreen, penwidth=1.8];\n")
    }

    activeEdges.foreach { case (a, c) =>
      b.append(s"  \"$a\" -- \"$c\";\n")
    }

    passiveEdges.foreach { case (a, c) =>
      b.append(s"  \"$a\" -- \"$c\" [style=dashed];\n")
    }

    b.append("}\n")
    b.result()
  }
}
