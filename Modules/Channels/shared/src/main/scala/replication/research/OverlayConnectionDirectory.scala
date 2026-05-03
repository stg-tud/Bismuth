package replication.research

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.overlay.HyParViewMultiplexed

/** vibecoded as part of the hyparview experiments */
object OverlayConnectionDirectory {

  enum LinkState {
    case Passive, Active
  }

  given Bottom[LinkState]  = Bottom.provide(LinkState.Passive)
  given Lattice[LinkState] = Lattice.sumLattice
  given Bottom[Long]       = Bottom.provide(0L)
  given Lattice[Long]      = Lattice.fromOrdering[Long]

  case class ViewSnapshot(
      active: Set[Uid],
      passive: Set[Uid],
      eager: Set[Uid],
  ) {
    def normalized: ViewSnapshot = {
      val activeOnly  = active
      val passiveOnly = passive -- activeOnly
      val eagerOnly   = eager intersect activeOnly
      ViewSnapshot(activeOnly, passiveOnly, eagerOnly)
    }
  }

  case class NodeInfo(
      peers: ObserveRemoveMap[Uid, LinkState],
      eagerPeers: ReplicatedSet[Uid],
      lastSeenMillis: Long,
  )
  object NodeInfo {
    given Bottom[NodeInfo]  = Bottom.derived
    given Lattice[NodeInfo] = Lattice.derived
  }

  type Directory = ObserveRemoveMap[Uid, NodeInfo]

  val empty: Directory = ObserveRemoveMap.empty

  def emptyNodeInfo: NodeInfo =
    NodeInfo(ObserveRemoveMap.empty, ReplicatedSet.empty, 0L)

  def snapshot(info: NodeInfo): ViewSnapshot = {
    val active  = info.peers.entries.collect { case (uid, LinkState.Active) => uid }.toSet
    val passive = info.peers.entries.collect { case (uid, LinkState.Passive) => uid }.toSet
    val eager   = info.eagerPeers.elements
    ViewSnapshot(active, passive, eager).normalized
  }

  def snapshot(state: Directory, node: Uid): ViewSnapshot =
    state.get(node).map(snapshot).getOrElse(ViewSnapshot(Set.empty, Set.empty, Set.empty))

  def updateNode(
      state: Directory,
      node: Uid,
      peers: Map[Uid, LinkState],
      eagerPeers: Set[Uid],
      lastSeenMillis: Long,
  )(using LocalUid): Directory = {
    val current = state.get(node).getOrElse(emptyNodeInfo)
    val desired = ViewSnapshot(
      active = peers.collect { case (uid, LinkState.Active) => uid }.toSet,
      passive = peers.collect { case (uid, LinkState.Passive) => uid }.toSet,
      eager = eagerPeers,
    ).normalized

    val peersAfterRemove = {
      val toRemove = current.peers.keySet -- desired.active -- desired.passive
      if toRemove.nonEmpty then current.peers.merge(current.peers.removeAll(toRemove))
      else current.peers
    }
    val withPassive = desired.passive.foldLeft(peersAfterRemove) { (acc, uid) =>
      if acc.get(uid).contains(LinkState.Passive) then acc
      else acc.merge(acc.update(uid, LinkState.Passive))
    }
    val nextPeers = desired.active.foldLeft(withPassive) { (acc, uid) =>
      if acc.get(uid).contains(LinkState.Active) then acc
      else acc.merge(acc.update(uid, LinkState.Active))
    }

    val eagerAfterRemove = {
      val toRemove = current.eagerPeers.elements -- desired.eager
      if toRemove.nonEmpty then current.eagerPeers.merge(current.eagerPeers.removeAll(toRemove))
      else current.eagerPeers
    }
    val nextEager = {
      val toAdd = desired.eager -- eagerAfterRemove.elements
      if toAdd.nonEmpty then eagerAfterRemove.merge(eagerAfterRemove.addAll(toAdd))
      else eagerAfterRemove
    }

    val next = NodeInfo(nextPeers, nextEager, math.max(current.lastSeenMillis, lastSeenMillis))
    if next == current && state.contains(node) then empty
    else state.update(node, next)
  }

  def updateNodeFromOverlay(
      state: Directory,
      node: Uid,
      activePeers: Iterable[HyParViewMultiplexed.PeerRef],
      passivePeers: Iterable[HyParViewMultiplexed.PeerRef],
      eagerPeers: Iterable[Uid],
      lastSeenMillis: Long,
  )(using LocalUid): Directory =
    updateNode(
      state,
      node,
      activePeers.iterator.map(peer => peer.uid -> LinkState.Active).toMap ++
      passivePeers.iterator.map(peer => peer.uid -> LinkState.Passive).toMap,
      eagerPeers.toSet,
      lastSeenMillis,
    )

  def removePeerReferences(
      state: Directory,
      node: Uid,
      peer: Uid,
  )(using LocalUid): Directory = {
    val current        = state.get(node).getOrElse(emptyNodeInfo)
    val remainingPeers = current.peers.entries.filterNot(_._1 == peer).toMap
    updateNode(state, node, remainingPeers, current.eagerPeers.elements - peer, current.lastSeenMillis)
  }

  def removeConnectionBothDirections(
      state: Directory,
      left: Uid,
      right: Uid,
  )(using LocalUid): Directory = {
    val afterLeft  = state.merge(removePeerReferences(state, left, right))
    val afterRight = afterLeft.merge(removePeerReferences(afterLeft, right, left))
    afterRight
  }

  def removeNodeEverywhere(
      state: Directory,
      node: Uid,
  )(using LocalUid): Directory = {
    val withoutRefs = state.entries.foldLeft(state) { case (acc, (owner, _)) =>
      acc.merge(removePeerReferences(acc, owner, node))
    }
    withoutRefs.merge(withoutRefs.remove(node))
  }

  def pruneStaleNodes(
      state: Directory,
      nowMillis: Long,
      maxAgeMillis: Long,
      self: Uid,
  )(using LocalUid): Directory = {
    val staleNodes = state.entries.iterator.collect {
      case (node, info) if node != self && nowMillis - info.lastSeenMillis > maxAgeMillis => node
    }.toList

    staleNodes.foldLeft(empty) { (acc, node) =>
      acc.merge(removeNodeEverywhere(state.merge(acc), node))
    }
  }
}
