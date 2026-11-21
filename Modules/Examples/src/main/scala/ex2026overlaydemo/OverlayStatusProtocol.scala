package ex2026overlaydemo

import channels.JsoniterCodecs.given
import channels.MessagingIO
import channels.routing.PlumtreeBroadcast.PeerRole
import channels.connection.Peer
import channels.overlay.{FullMeshOverlay, HyParViewStateMachine}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.LastWriterWins

object OverlayStatusProtocol {

  given Bottom[Long]  = Bottom.provide(0L)
  given Lattice[Long] = Lattice.fromOrdering[Long]

  enum PeerState {
    case Passive, Lazy, Eager
  }
  object PeerState {
    given Bottom[PeerState]         = Bottom.provide(PeerState.Passive)
    given Lattice[PeerState]        = Lattice.sumLattice
    given JsonValueCodec[PeerState] = JsonCodecMaker.make
  }

  case class LocalView(
      timestamp: Long,
      peers: Map[Uid, LastWriterWins[PeerState]],
  )
  object LocalView {
    given Bottom[LocalView]         = Bottom.derived
    given Lattice[LocalView]        = Lattice.derived
    given JsonValueCodec[LocalView] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  }

  /** each replica only ever writes its own entry, and entries are never removed */
  type Status = Map[Uid, LocalView]

  val empty: Status = Map.empty

  given JsonValueCodec[Status] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  def localViewOf[State](io: MessagingIO.Plumtree[State], timestamp: Long): LocalView = {
    val (activePeers, passivePeers) = io.overlayController match
        case overlay: HyParViewStateMachine => (overlay.activeView, overlay.passiveView)
        case overlay: FullMeshOverlay       => (overlay.active.keySet, Set.empty[Peer])
        case _                              => (Set.empty[Peer], Set.empty[Peer])

    val peerStates =
        val plumtreeRoles: Map[Uid, PeerState] =
          activePeers.iterator.map { peer =>
            val state = io.state.peerRoles.get(peer) match
                case Some(PeerRole.Lazy)  => PeerState.Lazy
                case Some(PeerRole.Eager) => PeerState.Eager
                case None                 => PeerState.Eager
            peer.uid -> state
          }.toMap

        (passivePeers.iterator.map(_.uid -> PeerState.Passive) ++
        plumtreeRoles.iterator).toList

    val peers = peerStates.map((uid, state) => uid -> LastWriterWins.now(state)).toMap

    LocalView(timestamp, peers)
  }

  def statusDelta[State](current: Status, io: MessagingIO.Plumtree[State], timestamp: Long): Status =
    Map(io.selfPeer.uid -> localViewOf(io, timestamp))

  def snapshot(status: Status): Map[Uid, Map[Uid, PeerState]] =
    status.iterator.map { case (uid, view) =>
      uid -> view.peers.iterator.map { case (peer, role) => peer -> role.value }.toMap
    }.toMap
}
