package replication

import channels.{Abort, Connection, ConnectionDescriptor, ChannelResolver, LatentConnection, SynchronousLocalConnection}
import de.rmgk.delay.Async
import munit.FunSuite
import rdts.base.Uid
import replication.research.{SignalingClient, SignalingServer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class SignalingServerTest extends FunSuite {

  private def asFuture[A](async: Async[Any, A]): Future[A] = {
    val promise = Promise[A]()
    async.run(promise.complete)
    promise.future
  }

  final private class Fixture {
    val abort         = Abort()
    val serverDetails = ConnectionDescriptor.SynchronousLocal("signal")
    val server        = SignalingServer(debug = false)

    def resolverFor(id: String): ChannelResolver = new ChannelResolver {
      override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
        if details == serverDetails then
            val link = SynchronousLocalConnection()
            server.addIncomingConnection(link.server)
            Some(link.client(s"$id"))
        else None
    }
  }


  test("client announces descriptors and can query topic info") {
    val fx       = Fixture()
    val a        = Uid.predefined("a")
    val b        = Uid.predefined("b")
    val aDetails = Set(ConnectionDescriptor.QueuedLocal("a"), ConnectionDescriptor.WebRtc("a"))
    val bDetails = Set(ConnectionDescriptor.QueuedLocal("b"))

    val clientA = SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("a"), localUid = a, abort = fx.abort)
    val clientB = SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("b"), localUid = b, abort = fx.abort)

    for
      aTopic <- asFuture(clientA.announce("topic-1", aDetails))
      bTopic <- asFuture(clientB.announce("topic-1", bDetails))
    yield {
      assert(aTopic(a) == aDetails, aTopic)
      assert(bTopic(a) == aDetails, bTopic)
      assert(bTopic(b) == bDetails, bTopic)
    }
  }

  test("server replaces old announcement state when the same uid reconnects") {
    val fx      = Fixture()
    val a       = Uid.predefined("a")
    val details = Set(ConnectionDescriptor.QueuedLocal("a"))

    val first  = SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("a-1"), localUid = a, abort = fx.abort)
    val second = SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("a-2"), localUid = a, abort = fx.abort)

    for
      _ <- asFuture(first.announce("topic-1", details))
      _ = assertEquals(fx.server.topicPeers("topic-1"), Map(a -> details))
      _ <- asFuture(second.announce("topic-1", Set.empty))
    yield assertEquals(fx.server.topicPeers("topic-1"), Map(a -> Set.empty))
  }

  test("topic lookup returns at most requested number of random peers") {
    val fx  = Fixture()
    val ids = List("a", "b", "c", "d").map(Uid.predefined)

    val announced = Future.sequence(ids.map { uid =>
      asFuture(SignalingClient(
        server = fx.serverDetails,
        resolver = fx.resolverFor(Uid.unwrap(uid)),
        localUid = uid,
        abort = fx.abort,
      ).announce("topic-1", Set(ConnectionDescriptor.WebRtc(Uid.unwrap(uid)))))
    })

    val observer = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("observer"),
      localUid = Uid.predefined("observer"),
      abort = fx.abort,
    )

    for
      _        <- announced
      lookedUp <- asFuture(observer.announce("topic-1", Set.empty, 3))
    yield {
      assertEquals(lookedUp.size, 3)
      assert(lookedUp.keySet.subsetOf(ids.toSet + Uid.predefined("observer")), lookedUp)
    }
  }


  test("server relays webrtc offer and answer through signaling clients") {
    val fx = Fixture()
    val a  = Uid.predefined("a")
    val b  = Uid.predefined("b")

    val seenOffer = Promise[(Uid, SignalingServer.Session)]()

    val clientA = SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("a"), localUid = a, abort = fx.abort)
    val clientB = SignalingClient(
      server = fx.serverDetails,
      resolver = fx.resolverFor("b"),
      localUid = b,
      abort = fx.abort,
      webrtcAnswerer = Some((from, session) =>
        Async.fromCallback {
          seenOffer.success(from -> session)
          Async.handler.succeed(SignalingServer.Session("answer", "sdp-answer"))
        }
      ),
    )

    for
      _      <- asFuture(clientA.announce("topic-1", Set.empty))
      _      <- asFuture(clientB.announce("topic-1", Set.empty))
      answer <- asFuture(clientA.requestSession(b, SignalingServer.Session("offer", "sdp-offer")))
      offer  <- seenOffer.future
    yield {
      assertEquals(offer, a -> SignalingServer.Session("offer", "sdp-offer"))
      assertEquals(answer, SignalingServer.Session("answer", "sdp-answer"))
    }
  }
}
