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

    private val active: mutable.Set[Uid]  = mutable.LinkedHashSet.empty
    private val passive: mutable.Set[Uid] = mutable.LinkedHashSet.empty

    private val outgoing: mutable.Map[Uid, Connection[HyParViewMessage]] = mutable.Map.empty

    def activeView: Set[Uid]  = active.toSet
    def passiveView: Set[Uid] = passive.toSet

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

    private def handleMessage(msg: HyParViewMessage): Unit =
      msg match
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
        sendTo(n, Disconnect(id))

    private def integratePassiveSample(sample: Set[Uid]): Unit =
      sample.foreach(addNodePassiveView)

    private def randomElement(set: collection.Set[Uid]): Uid =
      set.iterator.drop(rnd.nextInt(set.size)).next()

    private def randomSubset(set: Set[Uid], maxSize: Int): Set[Uid] =
      rnd.shuffle(set.toList).take(maxSize).toSet
  }

  test("hyparview queued setup with 1000 nodes") {
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
    while queue.nonEmpty && safety < 200000 do
      queue.deliverAll()
      safety += 1

    (0 until 8).foreach { _ =>
      nodes.foreach(_.shuffleTick())
      var innerSafety = 0
      while queue.nonEmpty && innerSafety < 200000 do
        queue.deliverAll()
        innerSafety += 1
    }

    val connectedEnough = nodes.count(_.activeView.nonEmpty)
    assertEquals(connectedEnough, n)
    assert(nodes.forall(_.activeView.size <= 5))
    assert(nodes.forall(_.passiveView.size <= 30))

    val dotPath = Paths.get("target", "hyparview-queued-1000.dot")
    Files.createDirectories(dotPath.getParent)
    Files.writeString(dotPath, renderDot(nodes), StandardCharsets.UTF_8)
  }

  private def renderDot(nodes: Seq[HyParViewNode]): String = {
    def edgeKey(a: Uid, b: Uid): (String, String) = {
      val aa = Uid.unwrap(a)
      val bb = Uid.unwrap(b)
      if aa <= bb then (aa, bb) else (bb, aa)
    }

    val activeEdges = mutable.LinkedHashSet.empty[(String, String)]
    val passiveEdges = mutable.LinkedHashSet.empty[(String, String)]

    nodes.foreach { n =>
      n.activeView.foreach { to =>
        if to != n.id then activeEdges += edgeKey(n.id, to)
      }
      n.passiveView.foreach { to =>
        if to != n.id then passiveEdges += edgeKey(n.id, to)
      }
    }

    // If an edge is active, do not also draw it as passive.
    passiveEdges --= activeEdges

    val b = new StringBuilder
    b.append("graph HyParViewQueued {\n")
    b.append("  graph [overlap=false, splines=true, nodesep=1.0, ranksep=1.2];\n")

    nodes.foreach { n =>
      val id = Uid.unwrap(n.id)
      b.append(s"  \"$id\";\n")
    }

    activeEdges.foreach { case (a, c) =>
      b.append(s"  \"$a\" -- \"$c\";\n")
    }

//    passiveEdges.foreach { case (a, c) =>
//      b.append(s"  \"$a\" -- \"$c\" [style=dashed];\n")
//    }

    b.append("}\n")
    b.result()
  }
}
