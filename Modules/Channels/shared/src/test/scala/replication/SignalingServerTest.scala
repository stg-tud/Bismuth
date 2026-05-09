package replication

import channels.{Abort, ChannelResolver, Connection, ConnectionDescriptor, LatentConnection, SynchronousLocalConnection}
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
    val link          = SynchronousLocalConnection("signal")
    val serverDetails = ConnectionDescriptor.SynchronousLocal("signal")
    val server        = SignalingServer(debug = false)
    server.addIncomingConnection(link.server)

    def resolverFor(id: String): ChannelResolver = new ChannelResolver {
      override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
        if details == serverDetails then Some(link.client(s"$id"))
        else None
    }
  }

  test("server relays webrtc offer and answer through signaling clients") {
    val fx = Fixture()
    val a  = Uid.predefined("a")
    val b  = Uid.predefined("b")

    val seenOffer = Promise[(Uid, SignalingServer.Session)]()

    val clientA =
      SignalingClient(server = fx.serverDetails, resolver = fx.resolverFor("a"), localUid = a, abort = fx.abort)
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
        _      <- asFuture(clientA.start())
        _      <- asFuture(clientB.start())
        answer <- asFuture(clientA.requestSession(b, SignalingServer.Session("offer", "sdp-offer")))
        offer  <- seenOffer.future
    yield {
      assertEquals(offer, a -> SignalingServer.Session("offer", "sdp-offer"))
      assertEquals(answer, SignalingServer.Session("answer", "sdp-answer"))
    }
  }
}
