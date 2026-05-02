package replication.research

import channels.{ChannelConnectDescriptor, ChannelResolver, SynchronousLocalConnection}
import munit.FunSuite
import rdts.base.Uid
import replication.research.SignalingServer.Message

class SignalingServerTest extends FunSuite {

  private final class Fixture {
    val serverDetails = ChannelConnectDescriptor.SynchronousLocal("signal")
    val server = SignalingServer(debug = false)

    def resolverFor(id: String): ChannelResolver[Message] = new ChannelResolver[Message] {
      override def canConnect(details: ChannelConnectDescriptor): Boolean = details == serverDetails
      override def connect(details: ChannelConnectDescriptor, label: String) =
        if details == serverDetails then
          val link = SynchronousLocalConnection[Message]()
          server.addIncomingConnection(link.server)
          Some(link.client(s"$id:$label"))
        else None
    }

    def drain(limit: Int = 1000): Unit = ()
  }

  test("client announces descriptors and can query topic and peer info") {
    val fx = Fixture()
    val a = Uid.predefined("a")
    val b = Uid.predefined("b")
    val aDetails = Set(ChannelConnectDescriptor.QueuedLocal("a"), ChannelConnectDescriptor.WebRtc("a"))
    val bDetails = Set(ChannelConnectDescriptor.QueuedLocal("b"))

    var aTopic: Map[Uid, Set[ChannelConnectDescriptor]] = Map.empty
    var bPeer: Map[String, Set[ChannelConnectDescriptor]] = Map.empty

    val clientA = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("a"),
      localUid = a,
      initialAnnouncements = Map("topic-1" -> aDetails),
      onTopicInfo = (_, peers) => aTopic = peers,
    )
    val clientB = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("b"),
      localUid = b,
      initialAnnouncements = Map("topic-1" -> bDetails),
      onPeerInfo = (_, topics) => bPeer = topics,
    )

    clientA.start()
    clientB.start()
    fx.drain()
    fx.drain()

    clientA.lookupTopic("topic-1")
    clientB.lookupPeer(a)
    fx.drain()
    fx.drain()

    assertEquals(aTopic(a), aDetails)
    assertEquals(aTopic(b), bDetails)
    assertEquals(bPeer("topic-1"), aDetails)
  }

  test("server replaces old announcement state when the same uid reconnects") {
    val fx = Fixture()
    val a = Uid.predefined("a")
    val details = Set(ChannelConnectDescriptor.QueuedLocal("a"))

    val first = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("a-1"),
      localUid = a,
      initialAnnouncements = Map("topic-1" -> details),
    )
    val second = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("a-2"),
      localUid = a,
      initialAnnouncements = Map.empty,
    )

    first.start()
    fx.drain()
    assertEquals(fx.server.topicPeers("topic-1"), Map(a -> details))

    second.start()
    fx.drain()
    assertEquals(fx.server.topicPeers("topic-1"), Map.empty)
  }

  test("topic lookup returns at most requested number of random peers") {
    val fx = Fixture()
    val ids = List("a", "b", "c", "d").map(Uid.predefined)

    ids.foreach { uid =>
      SignalingClient(
        server = fx.serverDetails,
        resolver = fx.resolverFor(Uid.unwrap(uid)),
        localUid = uid,
        initialAnnouncements = Map("topic-1" -> Set(ChannelConnectDescriptor.WebRtc(Uid.unwrap(uid)))),
      ).start()
    }

    var lookedUp: Map[Uid, Set[ChannelConnectDescriptor]] = Map.empty
    val observer = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("observer"),
      localUid = Uid.predefined("observer"),
      initialAnnouncements = Map.empty,
      onTopicInfo = (_, peers) => lookedUp = peers,
    )
    observer.start()
    observer.lookupTopic("topic-1", 3)

    fx.drain()
    assertEquals(lookedUp.size, 3)
    assert(lookedUp.keySet.subsetOf(ids.toSet))
  }

  test("server relays webrtc offer and answer through signaling clients") {
    val fx = Fixture()
    val a = Uid.predefined("a")
    val b = Uid.predefined("b")

    var seenOffer: Option[(Uid, SignalingServer.Session)] = None
    var seenAnswer: Option[(Uid, SignalingServer.Session)] = None

    val clientA = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("a"),
      localUid = a,
      initialAnnouncements = Map.empty,
      onAnswer = (from, session) => seenAnswer = Some(from -> session),
    )
    lazy val clientB: SignalingClient = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("b"),
      localUid = b,
      initialAnnouncements = Map.empty,
      onOffer = (from, session) =>
        seenOffer = Some(from -> session)
        clientB.answer(from, SignalingServer.Session("answer", "sdp-answer")),
    )

    clientA.start()
    clientB.start()
    fx.drain()
    fx.drain()

    clientA.offer(b, SignalingServer.Session("offer", "sdp-offer"))
    fx.drain()
    fx.drain()

    assertEquals(seenOffer, Some(a -> SignalingServer.Session("offer", "sdp-offer")))
    assertEquals(seenAnswer, Some(b -> SignalingServer.Session("answer", "sdp-answer")))
  }
}
