package replication.research

import channels.{Abort, ChannelConnectInfo, ChannelResolver, LatentConnection, PeerConnectInfo}
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import replication.JsoniterCodecs.given
import replication.PlumtreeMessage.Payload
import replication.overlay.HyParViewStateMachine
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.{BroadcastIO, MergingHistory, PlumtreeBroadcast}

import java.util.{Timer, TimerTask}
import scala.util.Random

class OverlayDemoNode(
    selfDetails: Set[ChannelConnectInfo],
    listenEnvelope: Option[LatentConnection],
    envelopeResolver: ChannelResolver,
    random: Random = Random(0),
    config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
    onStateChanged: OverlayStatusProtocol.Status => Unit = _ => (),
    printOverlayEventsToStdout: Boolean = false,
    runBackgroundTasks: Boolean = true,
    val localUid: LocalUid = LocalUid.gen(),
)(using Lattice[Payload[OverlayStatusProtocol.Status]]) {

  @volatile var state: OverlayStatusProtocol.Status = OverlayStatusProtocol.empty

  private val selfRef                                     = PeerConnectInfo(localUid.uid, selfDetails)
  private val abort                                       = Abort()
  private val timer                                       = Timer(true)
  private var broadcastIO: Option[BroadcastIO[OverlayStatusProtocol.Status]] = None

  private def nowMillis(): Long = System.currentTimeMillis()

  private def emitStateChanged(): Unit =
    onStateChanged(state)

  private def publish(delta: OverlayStatusProtocol.Status): Unit =
    if !Bottom.isEmpty(delta) then
        state = state.merge(delta)
        emitStateChanged()
        broadcastIO.foreach(_.applyDelta(delta))

  private def newOverlay() =
    BroadcastIO[OverlayStatusProtocol.Status](
      localUid,
      delta => {
        state = state.merge(delta)
        emitStateChanged()
      },
      overlay = Some(HyParViewStateMachine.empty(selfRef, config, random.between, _ => true)),
      resolver = envelopeResolver,
      globalAbort = abort,
      broadcast =
        Some(PlumtreeBroadcast(localUid.uid, deltaStorage = MergingHistory[OverlayStatusProtocol.Status](blockSize = 1000)))
    )

  private def startBackgroundTasks(): Unit = {
    timer.schedule(
      new TimerTask {
        override def run(): Unit =
          repairTick()
      },
      1000L,
      1000L,
    )
    timer.schedule(
      new TimerTask {
        override def run(): Unit = overlayInfoTick()
      },
      100L,
      100L,
    )
  }

  def start(seeds: List[ChannelConnectInfo] = Nil): Unit = {
    val node = newOverlay()
    broadcastIO = Some(node)
    listenEnvelope.foreach(node.addBinaryConnection)
    seeds.foreach(detail => bootstrapVia(PeerConnectInfo(Uid.gen(), Set(detail))))
    overlayInfoTick()
    if runBackgroundTasks then startBackgroundTasks()
  }

  def bootstrapVia(peer: PeerConnectInfo): Unit =
    broadcastIO.foreach(_.bootstrapVia(peer))

  def discoverPeers(peers: Iterable[PeerConnectInfo]): Unit =
    broadcastIO.foreach(_.discover(peers.toSet))

  def selfConnectionDetails: Set[ChannelConnectInfo] = selfDetails

  def repairTick(): Unit = broadcastIO.foreach(_.repairTick())

  def overlayInfoTick(): Unit = {
    broadcastIO.foreach { io =>
      given LocalUid = localUid
      publish(OverlayStatusProtocol.statusDelta(state, io, nowMillis()))
    }
  }

  def addOverlayConnection(latent: LatentConnection): Unit =
    broadcastIO.foreach(_.addBinaryConnection(latent))

  def activeView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].activeView
  }.getOrElse(Set.empty)

  def passiveView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].passiveView
  }.getOrElse(Set.empty)

  def eagerView: Set[Uid] = broadcastIO.map(_.plumtreeState.eagerPeers).getOrElse(Set.empty)

  def lastIncomingMessageTimes: Map[Uid, Long] = Map.empty

  def stop(): Unit = {
    abort.abort()
    timer.cancel()
  }
}
