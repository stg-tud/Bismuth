package replication.research

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}
import rdts.time.CausalTime
import replication.overlay.HyParViewMultiplexed

/** vibecoded as part of the hyparview experiments */
object OverlayConnectionDirectory {

  enum LinkState {
    case Passive, Active, Eager
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
      lastSeenMillis: Long,
      peers: Map[Uid, LinkState],
  )
  object NodeInfo {
    given Bottom[NodeInfo]  = Bottom.derived
    given Lattice[NodeInfo] = Lattice.derived
  }

  type Directory = ObserveRemoveMap[Uid, LastWriterWins[NodeInfo]]

  val empty: Directory = ObserveRemoveMap.empty

  def emptyNodeInfo: NodeInfo =
    NodeInfo(0L, Map.empty)

  def snapshot(info: NodeInfo): ViewSnapshot = {
    val active  = info.peers.collect { case (uid, LinkState.Active | LinkState.Eager) => uid }.toSet
    val passive = info.peers.collect { case (uid, LinkState.Passive) => uid }.toSet
    val eager   = info.peers.collect { case (uid, LinkState.Eager) => uid }.toSet
    ViewSnapshot(active, passive, eager).normalized
  }

  def snapshot(state: Directory, node: Uid): ViewSnapshot =
    state.get(node).map(_.value).map(snapshot).getOrElse(ViewSnapshot(Set.empty, Set.empty, Set.empty))

  def updateNode(
      state: Directory,
      node: Uid,
      peers: Map[Uid, LinkState],
      eagerPeers: Set[Uid],
      lastSeenMillis: Long,
  )(using LocalUid): Directory = {
    val desired = ViewSnapshot(
      active = peers.collect { case (uid, LinkState.Active | LinkState.Eager) => uid }.toSet,
      passive = peers.collect { case (uid, LinkState.Passive) => uid }.toSet,
      eager = eagerPeers,
    ).normalized

    val nextPeers =
      desired.passive.iterator.map(_ -> LinkState.Passive).toMap ++
      desired.active.iterator.map(uid => uid -> (if desired.eager.contains(uid) then LinkState.Eager else LinkState.Active)).toMap

    val next = NodeInfo(lastSeenMillis, nextPeers)
    if state.get(node).exists(_.value == next) then empty
    else state.update(node, LastWriterWins(CausalTime.now(), next))
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
    val current        = state.get(node).map(_.value).getOrElse(emptyNodeInfo)
    val remainingPeers = current.peers.removed(peer)
    val eagerPeers     = remainingPeers.collect { case (uid, LinkState.Eager) => uid }.toSet
    updateNode(state, node, remainingPeers, eagerPeers, current.lastSeenMillis)
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
      case (node, info) if node != self && nowMillis - info.value.lastSeenMillis > maxAgeMillis => node
    }.toList

    staleNodes.foldLeft(empty) { (acc, node) =>
      acc.merge(removeNodeEverywhere(state.merge(acc), node))
    }
  }
}
