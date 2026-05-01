package replication.research

import channels.{Abort, ConnectionDetails, ConnectionDetailsResolver, LatentConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import replication.PlumtreeDissemination
import replication.overlay.HyParViewMultiplexed
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.overlay.HyParViewViewListener
import replication.research.OverlayNetworkProtocol.DemoState

import java.util.{Timer, TimerTask}
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayDemoNode(
    selfDetails: Set[ConnectionDetails],
    listenEnvelope: Option[LatentConnection[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]]],
    envelopeResolver: ConnectionDetailsResolver[Set[ConnectionDetails], HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]],
    random: Random = Random(0),
    config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
    onStateChanged: DemoState => Unit = _ => (),
    printOverlayEventsToStdout: Boolean = false,
)(using JsonValueCodec[DemoState]) {

  val localUid: LocalUid = LocalUid.gen()

  @volatile var state: DemoState = DemoState.empty

  private val selfRef  = HyParViewMultiplexed.PeerRef(localUid.uid, selfDetails)
  private val abort    = Abort()
  private val timer    = Timer(true)
  private var overlay: Option[replication.overlay.HyParViewMultiplexedNode[DemoState, Set[ConnectionDetails]]] = None

  private def emitStateChanged(): Unit = onStateChanged(state)

  private def publish(delta: DemoState): Unit =
    if !Bottom.isEmpty(delta) then
      state = state.merge(delta)
      emitStateChanged()
      plumtree.applyDelta(delta)

  private def publishLocalView(): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.updateNodeFromOverlay(
      state.connections,
      localUid.uid,
      overlay.map(_.activePeers).getOrElse(Set.empty),
      overlay.map(_.passivePeers).getOrElse(Set.empty),
    )
    if !Bottom.isEmpty(delta) then publish(DemoState(ReplicatedSet.empty, delta))
  }

  private def removePeerEverywhere(peer: Uid): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, peer)
    if !Bottom.isEmpty(delta) then publish(DemoState(ReplicatedSet.empty, delta))
  }

  private def overlayLogger: HyParViewViewListener[Set[ConnectionDetails]] =
    if !printOverlayEventsToStdout then HyParViewViewListener.noop
    else
      HyParViewViewListener.printToStdout { details =>
        if details.isEmpty then "(undialable)"
        else details.toList.sortBy(ConnectionDetails.describe).map(ConnectionDetails.describe).mkString(", ")
      }

  private def overlayTracker: HyParViewViewListener[Set[ConnectionDetails]] =
    new HyParViewViewListener[Set[ConnectionDetails]] {
      override def activePeerAdded(peer: HyParViewMultiplexed.PeerRef[Set[ConnectionDetails]]): Unit      = publishLocalView()
      override def activePeerRemoved(peer: HyParViewMultiplexed.PeerRef[Set[ConnectionDetails]]): Unit    = publishLocalView()
      override def passivePeerAdded(peer: HyParViewMultiplexed.PeerRef[Set[ConnectionDetails]]): Unit     = publishLocalView()
      override def passivePeerRemoved(peer: HyParViewMultiplexed.PeerRef[Set[ConnectionDetails]]): Unit   = publishLocalView()
    }

  private val plumtree: PlumtreeDissemination[DemoState] = PlumtreeDissemination(
    localUid,
    delta => {
      state = state.merge(delta)
      emitStateChanged()
    },
    None,
    globalAbort = abort,
  )

  private def newOverlay(seed: Option[ConnectionDetails]) =
    new replication.overlay.HyParViewMultiplexedNode(
      selfRef,
      plumtree,
      listenEnvelope.getOrElse(new LatentConnection[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]] {
        override def prepare(receiver: channels.Receive[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]]) =
          throw UnsupportedOperationException("no local server configured for this overlay node")
      }),
      envelopeResolver,
      seed.map(Set(_)),
      random,
      config,
      viewListener = HyParViewViewListener.combine(overlayTracker, overlayLogger),
      onPeerDisconnected = removePeerEverywhere,
    )

  private def startShuffleTask(): Unit =
    timer.schedule(
      new TimerTask {
        override def run(): Unit = overlay.foreach(_.shuffleTick())
      },
      1000L,
      1000L,
    )

  def start(seeds: List[ConnectionDetails] = Nil): Unit = {
    val node = newOverlay(seeds.headOption)
    overlay = Some(node)
    listenEnvelope.foreach(_ => node.startServer())
    publishLocalView()
    startShuffleTask()
    seeds.headOption.foreach(_ => node.join())
  }

  def selfConnectionDetails: Set[ConnectionDetails] = selfDetails

  def publishAdd(value: String): Unit = {
    given LocalUid = localUid
    publish(DemoState(state.values.add(value), OverlayConnectionDirectory.empty))
  }

  def publishRemove(value: String): Unit =
    publish(DemoState(state.values.remove(value), OverlayConnectionDirectory.empty))

  def shuffleTick(): Unit = overlay.foreach(_.shuffleTick())

  def activeView: Set[Uid] = overlay.map(_.activeView).getOrElse(Set.empty)

  def passiveView: Set[Uid] = overlay.map(_.passiveView).getOrElse(Set.empty)

  def connectionDirectory: OverlayConnectionDirectory.Directory = state.connections

  def stop(): Unit = {
    given LocalUid = localUid
    val cleanup = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, localUid.uid)
    if !Bottom.isEmpty(cleanup) then {
      state = state.merge(DemoState(ReplicatedSet.empty, cleanup))
      emitStateChanged()
      plumtree.applyDelta(DemoState(ReplicatedSet.empty, cleanup))
    }
    overlay.foreach(_.stop(graceful = true))
    abort.abort()
    timer.cancel()
  }
}
