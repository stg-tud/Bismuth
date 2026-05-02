package webapps.ex2026niowebsocket

import rdts.base.Uid
import replication.research.OverlayConnectionDirectory
import replication.research.OverlayConnectionDirectory.LinkState

import scala.collection.mutable

object OverlayNetworkGraphModel {

  enum EdgeKind {
    case EagerOverlay, ActiveOverlay, PassiveOverlay
  }

  case class GraphEdge(from: Uid, to: Uid, kind: EdgeKind)
  case class GraphNode(uid: Uid, label: String, details: String, var x: Double, var y: Double, var vx: Double, var vy: Double, highlighted: Boolean)
  case class LocalViews(active: Set[Uid], passive: Set[Uid], eager: Set[Uid])

  def renderConnectionInfo(
      directory: OverlayConnectionDirectory.Directory,
      viewerUid: Option[Uid],
      localViews: Option[LocalViews],
  ): String = {
    val localStatus = localViews match
      case Some(views) =>
        val active  = views.active.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
        val passive = views.passive.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
        val eager   = views.eager.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
        s"local hyparview\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${if passive.nonEmpty then passive.mkString(", ") else "-"}\n  eager: ${if eager.nonEmpty then eager.mkString(", ") else "-"}"
      case None =>
        "local hyparview\n  active: -\n  passive: -\n  eager: -"

    val replicated = directory.entries.toList.sortBy((uid, _) => Uid.unwrap(uid)).map { (uid, info) =>
      val active  = info.peers.elements.filter(_.state == LinkState.Active).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val passive = info.peers.elements.filter(_.state == LinkState.Passive).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val eager   = info.eagerPeers.elements.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val label   = if viewerUid.contains(uid) then s"${Uid.unwrap(uid)} (you)" else Uid.unwrap(uid)
      s"$label\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${if passive.nonEmpty then passive.mkString(", ") else "-"}\n  eager: ${if eager.nonEmpty then eager.mkString(", ") else "-"}"
    }

    if replicated.nonEmpty then (localStatus +: replicated).mkString("\n")
    else s"$localStatus\nconnected, but no replicated overlay state yet"
  }

  def buildGraph(
      directory: OverlayConnectionDirectory.Directory,
      viewerUid: Option[Uid],
      localViews: Option[LocalViews],
      positions: mutable.Map[Uid, (Double, Double, Double, Double)],
      width: Double,
      height: Double,
  ): (Vector[GraphNode], Vector[GraphEdge]) = {
    val edges         = mutable.LinkedHashSet.empty[GraphEdge]
    val detailsByNode = mutable.LinkedHashMap.empty[Uid, String]

    directory.entries.foreach { (uid, info) =>
      val activeCount  = info.peers.elements.count(_.state == LinkState.Active)
      val passiveCount = info.peers.elements.count(_.state == LinkState.Passive)
      val eagerCount   = info.eagerPeers.elements.size
      detailsByNode.update(uid, s"active=$activeCount passive=$passiveCount eager=$eagerCount")
      info.peers.elements.foreach { peer =>
        val kind = peer.state match
          case LinkState.Active if info.eagerPeers.elements.contains(peer.uid) => EdgeKind.EagerOverlay
          case LinkState.Active                                                => EdgeKind.ActiveOverlay
          case LinkState.Passive                                               => EdgeKind.PassiveOverlay
        edges += GraphEdge(uid, peer.uid, kind)
        detailsByNode.getOrElseUpdate(peer.uid, peer.state match
          case LinkState.Active  => "active peer"
          case LinkState.Passive => "passive peer"
        )
      }
    }

    for
      selfUid <- viewerUid
      views   <- localViews
    do
      val localEager = views.eager intersect views.active
      localEager.foreach { peerUid =>
        val activeEdge = GraphEdge(selfUid, peerUid, EdgeKind.ActiveOverlay)
        edges.remove(activeEdge)
        if !edges.exists(edge => edge.from == selfUid && edge.to == peerUid && edge.kind == EdgeKind.EagerOverlay) then
          edges += GraphEdge(selfUid, peerUid, EdgeKind.EagerOverlay)
        detailsByNode.getOrElseUpdate(selfUid, s"active=${views.active.size} passive=${views.passive.size} eager=${views.eager.size}")
        detailsByNode.getOrElseUpdate(peerUid, "eager peer")
      }

    val eagerPairs = edges.iterator.collect {
      case GraphEdge(from, to, EdgeKind.EagerOverlay) => (from, to)
    }.toSet
    val filteredEdges = edges.iterator.filter {
      case GraphEdge(from, to, EdgeKind.ActiveOverlay) => !eagerPairs.contains((from, to))
      case _                                           => true
    }.toVector

    val uids  = detailsByNode.keySet.toVector.distinct
    val known = uids.toSet
    positions.filterInPlace((uid, _) => known.contains(uid))
    val nodes = uids.zipWithIndex.map { (uid, idx) =>
      val angle    = idx.toDouble / math.max(1, uids.size) * math.Pi * 2
      val initialX = width / 2 + math.cos(angle) * math.min(width, height) * 0.32
      val initialY = height / 2 + math.sin(angle) * math.min(width, height) * 0.32
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
      )
    }
    (nodes, filteredEdges)
  }

  def tick(nodes: Vector[GraphNode], edges: Vector[GraphEdge], positions: mutable.Map[Uid, (Double, Double, Double, Double)], width: Double, height: Double): Unit = {
    val byUid   = nodes.map(n => n.uid -> n).toMap
    val centerX = width / 2
    val centerY = height / 2
    val margin  = 50.0

    nodes.combinations(2).foreach {
      case Seq(a, b) =>
        val dx = b.x - a.x
        val dy = b.y - a.y
        val distanceSq = math.max(1.0, dx * dx + dy * dy)
        val distance = math.sqrt(distanceSq)
        val repulsion = 1800.0 / (distanceSq + 80.0)
        val fxRepel = dx * repulsion / math.max(1.0, distance)
        val fyRepel = dy * repulsion / math.max(1.0, distance)
        a.vx -= fxRepel
        a.vy -= fyRepel
        b.vx += fxRepel
        b.vy += fyRepel
      case _ => ()
    }

    edges.foreach {
      case GraphEdge(from, to, EdgeKind.EagerOverlay | EdgeKind.ActiveOverlay) =>
        for
          a <- byUid.get(from)
          b <- byUid.get(to)
        do
          val dx = b.x - a.x
          val dy = b.y - a.y
          val distance = math.max(1.0, math.sqrt(dx * dx + dy * dy))
          val desired = 170.0
          val pull = (distance - desired) * 0.005
          val fx = dx * pull / distance
          val fy = dy * pull / distance
          a.vx += fx
          a.vy += fy
          b.vx -= fx
          b.vy -= fy
      case GraphEdge(_, _, EdgeKind.PassiveOverlay) => ()
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
