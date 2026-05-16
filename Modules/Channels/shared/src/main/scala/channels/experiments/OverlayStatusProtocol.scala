package channels.experiments

import channels.BroadcastIO
import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast
import channels.broadcast.PlumtreeBroadcast.{Peer, PeerRole}
import channels.overlay.{FullMeshOverlay, HyParViewStateMachine}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Lattice.syntax.merge
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap}

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
      peers: ObserveRemoveMap[Uid, LastWriterWins[PeerState]],
  )
  object LocalView {
    given Bottom[LocalView]         = Bottom.derived
    given Lattice[LocalView]        = Lattice.derived
    given JsonValueCodec[LocalView] = JsonCodecMaker.make
  }

  type Status = ObserveRemoveMap[Uid, LocalView]

  val empty: Status = ObserveRemoveMap.empty

  given JsonValueCodec[Status] = summon[JsonValueCodec[ObserveRemoveMap[Uid, LocalView]]]

  def localViewOf[State](io: BroadcastIO[State], timestamp: Long)(using LocalUid): LocalView = {
    val (activePeers, passivePeers) = io.overlayController match
        case overlay: HyParViewStateMachine => (overlay.activeView, overlay.passiveView)
        case overlay: FullMeshOverlay       => (overlay.active.keySet, Set.empty[Uid])
        case _                              => (Set.empty[Uid], Set.empty[Uid])

    val peerStates =
      val plumtreeRoles: Map[Uid, PeerState] = io.plumtreeState match
          case pt: PlumtreeBroadcast[?] =>
            activePeers.iterator.map { uid =>
              val state = pt.peerRoles.get(Peer(uid)) match
                  case Some(PeerRole.Lazy)  => PeerState.Lazy
                  case Some(PeerRole.Eager) => PeerState.Eager
                  case None                 => PeerState.Eager
              uid -> state
            }.toMap
          case _ => Map.empty

      (passivePeers.iterator.map(_ -> PeerState.Passive) ++
        plumtreeRoles.iterator).toList

    val peers = peerStates.foldLeft(ObserveRemoveMap.empty[Uid, LastWriterWins[PeerState]]) {
      case (acc, (uid, state)) =>
        acc.merge(acc.update(uid, LastWriterWins.now(state)))
    }

    LocalView(timestamp, peers)
  }

  def statusDelta[State](current: Status, io: BroadcastIO[State], timestamp: Long)(using LocalUid): Status =
    current.update(io.replicaId.uid, localViewOf(io, timestamp))

  def snapshot(status: Status): Map[Uid, Map[Uid, PeerState]] =
    status.entries.iterator.map { case (uid, view) =>
      uid -> view.peers.entries.iterator.map { case (peer, role) => peer -> role.value }.toMap
    }.toMap
}
