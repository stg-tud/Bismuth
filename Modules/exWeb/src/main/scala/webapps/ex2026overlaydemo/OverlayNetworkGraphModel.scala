package webapps.ex2026overlaydemo

import rdts.base.Uid
import replication.research.OverlayStatusProtocol

import scala.collection.mutable

object OverlayNetworkGraphModel {

  private val maxReplicatedNodeAgeMillis = 30_000L

  private def ageLabel(lastSeenMillis: Long): String = {
    val ageMillis = math.max(0L, System.currentTimeMillis() - lastSeenMillis)
    if ageMillis < 10_000L then f"${ageMillis / 1000.0}%.3fs ago"
    else s"${ageMillis / 1000L}s ago"
  }

  private def nodeOpacity(lastSeenMillis: Long): Double = {
    val age = math.max(0L, System.currentTimeMillis() - lastSeenMillis).toDouble
    val t   = math.min(1.0, age / maxReplicatedNodeAgeMillis.toDouble)
    1.0 - 0.9 * t
  }

  enum EdgeKind {
    case EagerOverlay, ActiveOverlay, PassiveOverlay
  }

  case class GraphEdge(from: Uid, to: Uid, kind: EdgeKind, opacity: Double)
  case class GraphNode(
      uid: Uid,
      label: String,
      details: String,
      var x: Double,
      var y: Double,
      var vx: Double,
      var vy: Double,
      highlighted: Boolean,
      opacity: Double
  )
  case class LocalViews(active: Set[Uid], passive: Set[Uid], eager: Set[Uid], lastIncomingMessageTimes: Map[Uid, Long])

  private def snapshot(status: OverlayStatusProtocol.Status): Map[Uid, Map[Uid, OverlayStatusProtocol.PeerState]] =
    OverlayStatusProtocol.snapshot(status)

  def renderConnectionInfo(
      status: OverlayStatusProtocol.Status,
      viewerUid: Option[Uid],
      localViews: Option[LocalViews],
  ): String = {
    val localStatus = localViews match
        case Some(views) =>
          val active = views.active.toList.sortBy(Uid.unwrap).map { uid =>
            val suffix = views.lastIncomingMessageTimes.get(uid).map(ts => s" (${ageLabel(ts)})").getOrElse(" (never)")
            Uid.unwrap(uid) + suffix
          }
          val passive = views.passive.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
          val eager   = views.eager.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
          s"local overlay\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${
              if passive.nonEmpty then passive.mkString(", ") else "-"
            }\n  eager: ${if eager.nonEmpty then eager.mkString(", ") else "-"}"
        case None =>
          "local overlay\n  active: -\n  passive: -\n  eager: -"

    val replicated = status.entries.toList.sortBy((uid, _) => Uid.unwrap(uid)).map { (uid, view) =>
      val peers  = snapshot(status).getOrElse(uid, Map.empty)
      val active = peers.collect {
        case (peer, OverlayStatusProtocol.PeerState.Eager | OverlayStatusProtocol.PeerState.Lazy) => peer
      }.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val passive = peers.collect { case (peer, OverlayStatusProtocol.PeerState.Passive) => peer }.toList.sortBy(
        Uid.unwrap
      ).map(Uid.unwrap)
      val eager = peers.collect { case (peer, OverlayStatusProtocol.PeerState.Eager) => peer }.toList.sortBy(
        Uid.unwrap
      ).map(Uid.unwrap)
      val label = if viewerUid.contains(uid) then s"${Uid.unwrap(uid)} (you)" else Uid.unwrap(uid)
      s"$label\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${
          if passive.nonEmpty then passive.mkString(", ") else "-"
        }\n  eager: ${if eager.nonEmpty then eager.mkString(", ") else "-"}\n  last seen: ${ageLabel(view.timestamp)}"
    }

    if replicated.nonEmpty then (localStatus +: replicated).mkString("\n")
    else s"$localStatus\nconnected, but no replicated overlay state yet"
  }

  def buildGraph(
      status: OverlayStatusProtocol.Status,
      viewerUid: Option[Uid],
      localViews: Option[LocalViews],
      positions: mutable.Map[Uid, (Double, Double, Double, Double)],
      width: Double,
      height: Double,
  ): (Vector[GraphNode], Vector[GraphEdge]) = {
    val edges              = mutable.LinkedHashSet.empty[GraphEdge]
    val detailsByNode      = mutable.LinkedHashMap.empty[Uid, String]
    val snapshots          = snapshot(status)
    val timestamps         = status.entries.iterator.map((uid, view) => uid -> view.timestamp).toMap
    val opacityByNode      = mutable.LinkedHashMap.from(timestamps.iterator.map((uid, ts) => uid -> nodeOpacity(ts)))
    val unknownNodeOpacity = 0.1

    snapshots.foreach { (uid, peers) =>
      val activePeers = peers.collect {
        case (peer, OverlayStatusProtocol.PeerState.Eager | OverlayStatusProtocol.PeerState.Lazy) => peer
      }.toSet
      val passivePeers = peers.collect { case (peer, OverlayStatusProtocol.PeerState.Passive) => peer }.toSet
      val eagerPeers   = peers.collect { case (peer, OverlayStatusProtocol.PeerState.Eager) => peer }.toSet
      val opacity      = opacityByNode(uid)
      detailsByNode.update(
        uid,
        s"active=${activePeers.size} passive=${passivePeers.size} eager=${eagerPeers.size} lastSeen=${ageLabel(timestamps(uid))}"
      )
      activePeers.foreach { peerUid =>
        val kind = if eagerPeers.contains(peerUid) then EdgeKind.EagerOverlay else EdgeKind.ActiveOverlay
        edges += GraphEdge(uid, peerUid, kind, math.min(opacity, opacityByNode.getOrElse(peerUid, unknownNodeOpacity)))
        if opacityByNode.contains(peerUid) then
            detailsByNode.getOrElseUpdate(peerUid, "active peer")
            ()
      }
      passivePeers.foreach { peerUid =>
        edges += GraphEdge(
          uid,
          peerUid,
          EdgeKind.PassiveOverlay,
          math.min(opacity, opacityByNode.getOrElse(peerUid, unknownNodeOpacity))
        )
        if opacityByNode.contains(peerUid) then
            detailsByNode.getOrElseUpdate(peerUid, "passive peer")
            ()
      }
    }

    val filteredEdges = edges.iterator.map {
      case GraphEdge(from, to, EdgeKind.PassiveOverlay, opacity) =>
        GraphEdge(from, to, EdgeKind.PassiveOverlay, math.min(opacity, opacityByNode.getOrElse(to, 1.0)))
      case GraphEdge(from, to, kind @ (EdgeKind.EagerOverlay | EdgeKind.ActiveOverlay), opacity) =>
        GraphEdge(
          from,
          to,
          kind,
          math.min(
            opacity,
            math.min(opacityByNode.getOrElse(from, unknownNodeOpacity), opacityByNode.getOrElse(to, unknownNodeOpacity))
          )
        )
    }.toVector

    val uids  = detailsByNode.keySet.toVector.distinct
    val known = uids.toSet
    positions.filterInPlace((uid, _) => known.contains(uid))
    val nodes = uids.zipWithIndex.map { (uid, idx) =>
      val angle          = idx.toDouble / math.max(1, uids.size) * math.Pi * 2
      val initialX       = width / 2 + math.cos(angle) * math.min(width, height) * 0.32
      val initialY       = height / 2 + math.sin(angle) * math.min(width, height) * 0.32
      val (x, y, vx, vy) = positions.getOrElse(uid, (initialX, initialY, 0.0, 0.0))
      GraphNode(
        uid = uid,
        label = if viewerUid.contains(uid) then "you" else Uid.unwrap(uid),
        details = detailsByNode.getOrElse(uid, ""),
        x = x,
        y = y,
        vx = vx,
        vy = vy,
        highlighted = viewerUid.contains(uid),
        opacity = opacityByNode.getOrElse(uid, unknownNodeOpacity),
      )
    }
    (nodes, filteredEdges)
  }

  def tick(
      nodes: Vector[GraphNode],
      edges: Vector[GraphEdge],
      positions: mutable.Map[Uid, (Double, Double, Double, Double)],
      width: Double,
      height: Double
  ): Unit = {
    val byUid   = nodes.map(n => n.uid -> n).toMap
    val centerX = width / 2
    val centerY = height / 2
    val margin  = 50.0

    val eagerNeighborsByNode = mutable.Map.empty[Uid, mutable.LinkedHashSet[Uid]]
    edges.foreach {
      case GraphEdge(from, to, EdgeKind.EagerOverlay, _) =>
        eagerNeighborsByNode.getOrElseUpdate(from, mutable.LinkedHashSet.empty) += to
        eagerNeighborsByNode.getOrElseUpdate(to, mutable.LinkedHashSet.empty) += from
      case _ => ()
    }

    nodes.indices.foreach { i =>
      ((i + 1) until nodes.size).foreach { j =>
        val a          = nodes(i)
        val b          = nodes(j)
        val dx         = b.x - a.x
        val dy         = b.y - a.y
        val distanceSq = math.max(1.0, dx * dx + dy * dy)
        val distance   = math.sqrt(distanceSq)
        val repulsion  = 1800.0 / (distanceSq + 80.0)
        val fxRepel    = dx * repulsion / math.max(1.0, distance)
        val fyRepel    = dy * repulsion / math.max(1.0, distance)
        a.vx -= fxRepel
        a.vy -= fyRepel
        b.vx += fxRepel
        b.vy += fyRepel
      }
    }

    edges.foreach {
      case GraphEdge(from, to, kind @ (EdgeKind.EagerOverlay | EdgeKind.ActiveOverlay), _) =>
        for
            a <- byUid.get(from)
            b <- byUid.get(to)
        do
            val dx                  = b.x - a.x
            val dy                  = b.y - a.y
            val distance            = math.max(1.0, math.sqrt(dx * dx + dy * dy))
            val (desired, strength) = if kind == EdgeKind.EagerOverlay then (135.0, 0.0085) else (290.0, 0.0006)
            val pull                = (distance - desired) * strength
            val fx                  = dx * pull / distance
            val fy                  = dy * pull / distance
            a.vx += fx
            a.vy += fy
            b.vx -= fx
            b.vy -= fy
      case GraphEdge(_, _, EdgeKind.PassiveOverlay, _) => ()
    }

    eagerNeighborsByNode.foreach { (uid, neighbors) =>
      byUid.get(uid).foreach { center =>
        val spokeNodes: Vector[GraphNode] = neighbors.iterator.flatMap(byUid.get).toVector
        val spokeCount                    = spokeNodes.size
        if spokeCount >= 2 then {
          val desiredAngle = math.Pi * 2 / spokeCount.toDouble
          val ordered      = spokeNodes.map { other =>
            val angle = math.atan2(other.y - center.y, other.x - center.x)
            (other, angle)
          }.sortBy(_._2)

          ordered.indices.foreach { idx =>
            val (current, currentAngle) = ordered(idx)
            val (next, nextAngleRaw)    = ordered((idx + 1) % ordered.size)
            val nextAngle               = if idx + 1 < ordered.size then nextAngleRaw else nextAngleRaw + math.Pi * 2
            val gap                     = nextAngle - currentAngle
            if gap < desiredAngle then {
              val deficit        = desiredAngle - gap
              val spreadStrength = deficit * 0.032

              val currentDx       = current.x - center.x
              val currentDy       = current.y - center.y
              val currentDistance = math.max(1.0, math.sqrt(currentDx * currentDx + currentDy * currentDy))
              val currentTx       = -currentDy / currentDistance
              val currentTy       = currentDx / currentDistance

              val nextDx       = next.x - center.x
              val nextDy       = next.y - center.y
              val nextDistance = math.max(1.0, math.sqrt(nextDx * nextDx + nextDy * nextDy))
              val nextTx       = -nextDy / nextDistance
              val nextTy       = nextDx / nextDistance

              current.vx -= currentTx * spreadStrength
              current.vy -= currentTy * spreadStrength
              next.vx += nextTx * spreadStrength
              next.vy += nextTy * spreadStrength
              center.vx += (currentTx - nextTx) * spreadStrength * 0.22
              center.vy += (currentTy - nextTy) * spreadStrength * 0.22
            }
          }
        }
      }
    }

    nodes.foreach { node =>
      node.vx += (centerX - node.x) * 0.0015
      node.vy += (centerY - node.y) * 0.0015
      if node.x < margin then node.vx += (margin - node.x) * 0.03
      if node.x > width - margin then node.vx -= (node.x - (width - margin)) * 0.03
      if node.y < margin then node.vy += (margin - node.y) * 0.03
      if node.y > height - margin then node.vy -= (node.y - (height - margin)) * 0.03
      node.vx *= 0.88
      node.vy *= 0.88
      node.x += node.vx
      node.y += node.vy
      positions.update(node.uid, (node.x, node.y, node.vx, node.vy))
    }
  }
}
