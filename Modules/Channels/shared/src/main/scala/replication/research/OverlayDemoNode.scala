package replication.research

import channels.{Abort, ConnectionDetails, ConnectionDetailsResolver, LatentConnection, Receive}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedSet}
import replication.{PlumtreeDissemination, ProtocolMessage}
import replication.overlay.{HyParViewMultiplexed, HyParViewMultiplexedNode, HyParViewUnified}
import replication.research.OverlayNetworkProtocol.{DemoState, WebRtcAnswer, WebRtcOffer}

import java.util.{Timer, TimerTask}
import scala.collection.mutable
import scala.util.Random

/** vibecoded. dont trust 😉 */
class OverlayDemoNode(
    selfDetails: Set[ConnectionDetails],
    membershipDetails: Option[ConnectionDetails],
    listenEnvelope: Option[LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]],
    envelopeResolver: ConnectionDetailsResolver[ConnectionDetails, HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]],
    random: Random = Random(0),
    config: HyParViewUnified.HyParViewConfig = HyParViewUnified.HyParViewConfig.fromEstimatedNetworkSize(10),
    onDirectoryChanged: OverlayConnectionDirectory.Directory[ConnectionDetails] => Unit = _ => (),
)(using JsonValueCodec[DemoState]) {

  val localUid: LocalUid = LocalUid.gen()

  @volatile var state: DemoState = DemoState.empty

  private val selfRef = HyParViewMultiplexed.PeerRef(localUid.uid, membershipDetails.orElse(selfDetails.headOption).getOrElse(
    throw IllegalArgumentException("OverlayDemoNode requires membership details or at least one self connection detail")
  ))
  private var overlay: Option[HyParViewMultiplexedNode[DemoState, ConnectionDetails]] = None
  private val shuffleTimer = Timer(true)
  private val abort        = Abort()

  private def refreshOverlayKnowledge(): Unit =
    onDirectoryChanged(state.connections)

  private def webRtcOffersSummary: List[String] =
    state.webRtcOffers.entries.flatMap { (target, offers) =>
      offers.entries.flatMap { (from, offer) =>
        offer.read.map(v => s"${Uid.unwrap(from)}->${Uid.unwrap(target)}:${v.id.take(12)}")
      }
    }.toList.sorted

  private def webRtcAnswersSummary: List[String] =
    state.webRtcAnswers.entries.flatMap { (target, perPeer) =>
      perPeer.entries.flatMap { (from, answer) =>
        answer.read.map(v => s"${Uid.unwrap(from)}->${Uid.unwrap(target)}:${v.offerId.take(12)}")
      }
    }.toList.sorted

  val plumtree: PlumtreeDissemination[DemoState] = PlumtreeDissemination(
    localUid,
    delta => {
      state = state.merge(delta)
      if !Bottom.isEmpty(delta.webRtcOffers) || !Bottom.isEmpty(delta.webRtcAnswers) then
        println(s"[overlay-demo-node ${Uid.unwrap(localUid.uid)}] merged webrtc delta offers=${webRtcOffersSummary} answers=${webRtcAnswersSummary}")
      refreshOverlayKnowledge()
    },
    None,
    globalAbort = abort,
  )

  private def publishLocalView(
      activePeers: Set[HyParViewMultiplexed.PeerRef[ConnectionDetails]],
      passivePeers: Set[HyParViewMultiplexed.PeerRef[ConnectionDetails]],
  ): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.updateNodeFromOverlay(
      state.connections,
      localUid.uid,
      selfDetails,
      activePeers,
      Set.empty,
    )
    if !Bottom.isEmpty(delta) then {
      val wrapped = DemoState(ReplicatedSet.empty, delta, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
  }

  private def handleDisconnectedPeer(peer: Uid): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.removeConnectionBothDirections(state.connections, localUid.uid, peer)
    if !Bottom.isEmpty(delta) then {
      val wrapped = DemoState(ReplicatedSet.empty, delta, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
  }

  private def newOverlay(contact: Option[ConnectionDetails]): HyParViewMultiplexedNode[DemoState, ConnectionDetails] =
    new HyParViewMultiplexedNode(
      selfRef,
      plumtree,
      listenEnvelope.getOrElse(new LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]] {
        override def prepare(receiver: channels.Receive[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]) =
          throw UnsupportedOperationException("no local server configured for this overlay node")
      }),
      envelopeResolver,
      contact,
      random,
      config,
      onViewChanged = publishLocalView,
      onPeerDisconnected = handleDisconnectedPeer,
    )

  private def startShuffleTask(): Unit =
    shuffleTimer.schedule(
      new TimerTask {
        override def run(): Unit = {
          overlay.foreach(_.shuffleTick())
          refreshOverlayKnowledge()
        }
      },
      1000L,
      1000L,
    )

  def start(seeds: List[ConnectionDetails] = Nil): Unit = {
    val node = newOverlay(seeds.headOption)
    overlay = Some(node)
    listenEnvelope.foreach(_ => node.startServer())
    startShuffleTask()
    publishLocalView(Set.empty, Set.empty)
    seeds.headOption.foreach(_ => node.join())
  }

  private def publishCurrentView(): Unit =
    publishLocalView(
      overlay.map(_.activePeers).getOrElse(Set.empty),
      overlay.map(_.passivePeers).getOrElse(Set.empty),
    )

  def publishAdd(value: String): Unit = {
    given LocalUid = localUid
    val delta   = state.values.add(value)
    val wrapped = DemoState(delta, OverlayConnectionDirectory.empty, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
    state = state.merge(wrapped)
    plumtree.applyDelta(wrapped)
  }

  def publishRemove(value: String): Unit = {
    val delta   = state.values.remove(value)
    val wrapped = DemoState(delta, OverlayConnectionDirectory.empty, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
    state = state.merge(wrapped)
    plumtree.applyDelta(wrapped)
  }

  def publishWebRtcOffer(target: Uid, offer: WebRtcOffer): Unit = {
    given LocalUid = localUid
    val current = state.webRtcOffers.get(target).getOrElse(ObserveRemoveMap.empty[Uid, LastWriterWins[Option[WebRtcOffer]]])
    val register = current.get(localUid.uid).getOrElse(LastWriterWins.empty[Option[WebRtcOffer]]).write(Some(offer))
    val next = current.update(localUid.uid, register)
    val delta = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty, state.webRtcOffers.update(target, next), ObserveRemoveMap.empty)
    state = state.merge(delta)
    println(s"[overlay-demo-node ${Uid.unwrap(localUid.uid)}] publish offer target=${Uid.unwrap(target)} id=${offer.id} offers=${webRtcOffersSummary}")
    plumtree.applyDelta(delta)
  }

  def clearWebRtcOffer(target: Uid, from: Uid): Unit = {
    given LocalUid = localUid
    val current = state.webRtcOffers.get(target).getOrElse(ObserveRemoveMap.empty[Uid, LastWriterWins[Option[WebRtcOffer]]])
    val register = current.get(from).getOrElse(LastWriterWins.empty[Option[WebRtcOffer]]).write(None)
    val next = current.update(from, register)
    val delta = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty, state.webRtcOffers.update(target, next), ObserveRemoveMap.empty)
    state = state.merge(delta)
    plumtree.applyDelta(delta)
  }

  def publishWebRtcAnswer(target: Uid, answer: WebRtcAnswer): Unit = {
    given LocalUid = localUid
    val current = state.webRtcAnswers.get(target).getOrElse(ObserveRemoveMap.empty[Uid, LastWriterWins[Option[WebRtcAnswer]]])
    val register = current.get(localUid.uid).getOrElse(LastWriterWins.empty[Option[WebRtcAnswer]]).write(Some(answer))
    val next = current.update(localUid.uid, register)
    val delta = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty, ObserveRemoveMap.empty, state.webRtcAnswers.update(target, next))
    state = state.merge(delta)
    println(s"[overlay-demo-node ${Uid.unwrap(localUid.uid)}] publish answer target=${Uid.unwrap(target)} offerId=${answer.offerId} answers=${webRtcAnswersSummary}")
    plumtree.applyDelta(delta)
  }

  def clearWebRtcAnswer(target: Uid, from: Uid): Unit = {
    given LocalUid = localUid
    val current = state.webRtcAnswers.get(target).getOrElse(ObserveRemoveMap.empty[Uid, LastWriterWins[Option[WebRtcAnswer]]])
    val register = current.get(from).getOrElse(LastWriterWins.empty[Option[WebRtcAnswer]]).write(None)
    val next = current.update(from, register)
    val delta = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty, ObserveRemoveMap.empty, state.webRtcAnswers.update(target, next))
    state = state.merge(delta)
    plumtree.applyDelta(delta)
  }

  def addDisseminationConnection(latent: LatentConnection[ProtocolMessage[DemoState]]): Unit =
    plumtree.addObjectConnection(latent)

  def addOverlayConnection(latent: LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]): Unit =
    overlay.foreach(_.addIncomingConnection(latent))

  def activeView: Set[Uid] = overlay.map(_.activeView).getOrElse(Set.empty)

  def passiveView: Set[Uid] = overlay.map(_.passiveView).getOrElse(Set.empty)

  def connectionDirectory: OverlayConnectionDirectory.Directory[ConnectionDetails] = state.connections

  def stop(): Unit = {
    given LocalUid = localUid
    val cleanup = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, localUid.uid)
    if !Bottom.isEmpty(cleanup) then {
      val wrapped = DemoState(ReplicatedSet.empty, cleanup, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
    abort.abort()
    shuffleTimer.cancel()
  }
}
