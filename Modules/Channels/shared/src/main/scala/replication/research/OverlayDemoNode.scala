package replication.research

import channels.{Abort, ChannelConnectInfo, ChannelResolver, LatentConnection, PeerConnectInfo}
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, LocalUid, Uid}
import replication.BroadcastIO
import replication.PlumtreeBroadcast.Peer
import replication.JsoniterCodecs.codecDemoState
import replication.overlay.HyParViewStateMachine
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.research.OverlayNetworkProtocol.DemoState

import java.util.{Timer, TimerTask}
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayDemoNode(
                       selfDetails: Set[ChannelConnectInfo],
                       listenEnvelope: Option[LatentConnection],
                       envelopeResolver: ChannelResolver,
                       random: Random = Random(0),
                       config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
                       onStateChanged: DemoState => Unit = _ => (),
                       printOverlayEventsToStdout: Boolean = false,
                       runBackgroundTasks: Boolean = true,
                       val localUid: LocalUid = LocalUid.gen(),
) {

  @volatile var state: DemoState = DemoState.empty

  private val selfRef = PeerConnectInfo(localUid.uid, selfDetails)
  private val abort   = Abort()
  private val timer   = Timer(true)
  private var broadcastIO: Option[BroadcastIO[DemoState]] = None

  private val heartbeatIntervalMillis = 10_000L

  private def nowMillis(): Long = System.currentTimeMillis()

  private def emitStateChanged(): Unit =
    onStateChanged(state)

  private def publish(delta: DemoState): Unit =
    if !Bottom.isEmpty(delta) then
        state = state.merge(delta)
        emitStateChanged()
        broadcastIO.foreach(_.applyDelta(delta))

  private def refreshLocalView(replicate: Boolean, updateTimestamp: Boolean): Unit = {
    given LocalUid = localUid
    val lastSeenMillis =
      if updateTimestamp then nowMillis()
      else state.connections.get(localUid.uid).map(_.value.lastSeenMillis).getOrElse(0L)
    val activePeers   = broadcastIO.flatMap(io =>
      io.overlayController match
          case hyp: HyParViewStateMachine => Some(hyp.activePeers)
          case _                         => None
    ).getOrElse(Set.empty)
    val passivePeers  = broadcastIO.flatMap(io =>
      io.overlayController match
          case hyp: HyParViewStateMachine => Some(hyp.passivePeers)
          case _                         => None
    ).getOrElse(Set.empty)
    val eagerView     = broadcastIO.map(io => io.plumtreeState.peerRoles.collect {
      case (Peer(uid), _) => uid
    }.toSet).getOrElse(Set.empty)
    val delta         = OverlayConnectionDirectory.updateNodeFromOverlay(
      state.connections,
      localUid.uid,
      activePeers,
      passivePeers,
      eagerView,
      lastSeenMillis,
    )
    if !Bottom.isEmpty(delta) then
        if replicate then publish(DemoState(delta))
        else {
          state = state.merge(DemoState(delta))
          emitStateChanged()
        }
  }

  private def publishLocalView(): Unit = refreshLocalView(replicate = true, updateTimestamp = true)
  private def pruneStaleReplicatedNodes(): Unit = {
    given LocalUid = localUid
    val delta      = OverlayConnectionDirectory.pruneStaleNodes(
      state.connections,
      nowMillis(),
      30_000L,
      localUid.uid,
    )
    if !Bottom.isEmpty(delta) then publish(DemoState(delta))
  }

  private def newOverlay() = {
    val stateMachine = HyParViewStateMachine.empty(selfRef, config, random.between, _ => true)
    BroadcastIO[DemoState](
      localUid,
      (delta: DemoState) => {
        state = state.merge(delta)
        emitStateChanged()
      },
      overlay = Some(stateMachine),
      resolver = envelopeResolver,
      globalAbort = abort,
    )
  }

  private def startBackgroundTasks(): Unit = {
    timer.schedule(
      new TimerTask {
        override def run(): Unit =
          broadcastIO.foreach(_.repairTick())
      },
      2000L,
      2000L,
    )
    timer.schedule(
      new TimerTask {
        override def run(): Unit = {
          publishLocalView()
          pruneStaleReplicatedNodes()
        }
      },
      heartbeatIntervalMillis,
      heartbeatIntervalMillis,
    )
  }

  def start(seeds: List[ChannelConnectInfo] = Nil): Unit = {
    val node = newOverlay()
    broadcastIO = Some(node)
    listenEnvelope.foreach(node.addBinaryConnection)
    seeds.foreach(detail => node.discover(Set(PeerConnectInfo(localUid.uid, Set(detail)))))
    publishLocalView()
    if runBackgroundTasks then startBackgroundTasks()
  }

  def discoverPeers(peers: Iterable[PeerConnectInfo]): Unit =
    broadcastIO.foreach(_.discover(peers.toSet))

  def selfConnectionDetails: Set[ChannelConnectInfo] = selfDetails

  def shuffleTick(): Unit = broadcastIO.foreach(_.repairTick())

  def repairTick(): Unit = broadcastIO.foreach(_.repairTick())

  def addOverlayConnection(latent: LatentConnection): Unit =
    broadcastIO.foreach(_.addBinaryConnection(latent))

  def activeView: Set[Uid] = broadcastIO.flatMap(io =>
    io.overlayController match
        case hyp: HyParViewStateMachine => Some(hyp.activeView)
        case _                         => None
  ).getOrElse(Set.empty)

  def passiveView: Set[Uid] = broadcastIO.flatMap(io =>
    io.overlayController match
        case hyp: HyParViewStateMachine => Some(hyp.passiveView)
        case _                         => None
  ).getOrElse(Set.empty)

  def eagerView: Set[Uid] = broadcastIO.map(io => io.plumtreeState.peerRoles.collect {
    case (Peer(uid), _) => uid
  }.toSet).getOrElse(Set.empty)

  def lastIncomingMessageTimes: Map[Uid, Long] = Map.empty

  def connectionDirectory: OverlayConnectionDirectory.Directory = state.connections

  def stop(): Unit = {
    given LocalUid = localUid
    val cleanup    = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, localUid.uid)
    if !Bottom.isEmpty(cleanup) then {
      state = state.merge(DemoState(cleanup))
      emitStateChanged()
      broadcastIO.foreach(_.applyDelta(DemoState(cleanup)))
    }
    abort.abort()
    timer.cancel()
  }
}
