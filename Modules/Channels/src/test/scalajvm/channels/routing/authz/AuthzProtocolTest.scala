package channels.routing.authz

import channels.routing.PlumtreeMessage
import channels.authz.AuthzCodecs.ardtEventCodec
import channels.routing.Routing.{Effect, Step, Timer}
import channels.authz.AuthzTestSupport.{*, given}
import channels.connection.Peer
import channels.crypto.Ed25519.given
import channels.crypto.{Hash, PrivateIdentity}
import channels.authz.{ArdtEvent, Authorization, AuthzReplica}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import munit.FunSuite
import rdts.base.Uid
import rdts.filters.PermissionTree

import scala.collection.mutable

/** Guarantees of [[AuthzProtocol]], on an in-memory network that delivers messages in order and fires timers when
  * idle. See [[AuthzMessagingIOTest]] for a network of [[channels.MessagingIO]]s.
  */
class AuthzProtocolTest extends FunSuite {

  type Protocol = AuthzProtocol[Set[Int]]
  type State    = Protocol#State

  final class Node(val identity: PrivateIdentity, genesis: Hash) {
    val protocol: Protocol                  = AuthzProtocol[Set[Int]](identity)
    var state: State                        = AuthzProtocol.initial[Set[Int]](identity.getPublic, genesis)
    val peer: Peer                          = Peer.Authenticated(Uid(identity.getPublic.id))
    val delivered: mutable.Buffer[Set[Int]] = mutable.Buffer.empty
    def replica: AuthzReplica[Set[Int]]     = state.store.replica
  }

  final class Network(nodes: Node*) {
    private val inbox  = mutable.Queue.empty[(Node, Node, AuthzMessage)]
    private val timers = mutable.Set.empty[Node]

    /** Messages matching this are lost. */
    var lose: AuthzMessage => Boolean = _ => false

    def run(node: Node)(f: (Protocol, State) => Step[State, AuthzMessage, Set[Int]]): Unit = {
      val step = f(node.protocol, node.state)
      node.state = step.state
      step.events.foreach {
        case Effect.Send(peers, message) =>
          if !lose(message) then
              peers.foreach(p => nodes.find(_.peer == p).foreach(to => inbox.enqueue((node, to, message))))
        case Effect.Deliver(state) => node.delivered += state
        case Effect.StartTimer(_)  => timers += node
      }
    }

    def connect(a: Node, b: Node): Unit = {
      run(a)(_.addPeer(_, b.peer))
      run(b)(_.addPeer(_, a.peer))
    }

    def disconnect(a: Node, b: Node): Unit = {
      run(a)(_.removePeer(_, b.peer))
      run(b)(_.removePeer(_, a.peer))
    }

    /** Deliver all messages, and fire the timers whenever there are none left, until nothing happens anymore. */
    def settle(): Unit =
      while inbox.nonEmpty || timers.nonEmpty do {
        while inbox.nonEmpty do {
          val (from, to, message) = inbox.dequeue()
          run(to)(_.handleMessage(_, from.peer, message))
        }
        val fire = timers.toList
        timers.clear()
        fire.foreach(node => run(node)(_.timerFired(_, Timer.Repair)))
      }
  }

  private def setup(): (Node, Node, Network, ArdtEvent) = {
    val root    = newPrivateIdentity()
    val bob     = newPrivateIdentity()
    val genesis = Authorization.createGenesis(root)
    val a       = Node(root, genesis.hash)
    val b       = Node(bob, genesis.hash)
    val net     = Network(a, b)
    net.run(a)(_.receiveLocally(_, genesis))
    net.connect(a, b)
    net.settle()
    (a, b, net, genesis)
  }

  private def delegateAll(net: Network, from: Node, to: Node, genesis: ArdtEvent): Unit =
    net.run(from)(_.delegate(_, genesis.hash, to.identity.getPublic, PermissionTree.allow, PermissionTree.allow))

  test("a new replica learns the event graph from its peer") {
    val (a, b, _, genesis) = setup()
    assertEquals(b.replica.heads, Set(genesis.hash))
  }

  test("deltas only reach replicas that may read them, delegation makes earlier deltas readable") {
    val (a, b, net, genesis) = setup()

    net.run(a)(_.write(_, Set(1)))
    net.settle()
    assertEquals(a.replica.state, Set(1))
    assertEquals(a.delivered.lastOption, Some(Set(1)))
    assertEquals(b.replica.heads, a.replica.heads, "the event is disseminated")
    assertEquals(b.replica.state, Set.empty[Int], "but bob may not read the delta")

    delegateAll(net, a, b, genesis)
    net.settle()
    assertEquals(b.replica.state, Set(1))

    net.run(a)(_.write(_, Set(2)))
    net.settle()
    assertEquals(b.replica.state, Set(1, 2))
    assertEquals(b.delivered.lastOption, Some(Set(1, 2)))

    net.run(b)(_.write(_, Set(3)))
    net.settle()
    assertEquals(a.replica.state, Set(1, 2, 3))
  }

  test("events and deltas spread through intermediate replicas") {
    val root    = newPrivateIdentity()
    val genesis = Authorization.createGenesis(root)
    val a       = Node(root, genesis.hash)
    val b       = Node(newPrivateIdentity(), genesis.hash)
    val c       = Node(newPrivateIdentity(), genesis.hash)
    val net     = Network(a, b, c)
    net.run(a)(_.receiveLocally(_, genesis))
    net.connect(a, b)
    net.connect(b, c)
    delegateAll(net, a, b, genesis)
    delegateAll(net, a, c, genesis)
    net.settle()

    net.run(a)(_.write(_, Set(5)))
    net.settle()
    assertEquals(c.replica.heads, a.replica.heads)
    assertEquals(c.replica.state, Set(5))
  }

  test("lost events are repaired") {
    val (a, b, net, genesis) = setup()
    delegateAll(net, a, b, genesis)
    net.settle()

    net.lose = {
      case AuthzMessage.Events(PlumtreeMessage.Push(_)) => true
      case _                                            => false
    }
    net.run(a)(_.write(_, Set(1)))
    net.run(a)(_.write(_, Set(2)))
    net.lose = _ => false
    net.run(a)(_.write(_, Set(3)))
    net.settle()

    assertEquals(b.replica.heads, a.replica.heads)
    assertEquals(b.replica.state, Set(1, 2, 3))
  }

  test("deltas that were lost are recovered on reconnect") {
    val (a, b, net, genesis) = setup()
    delegateAll(net, a, b, genesis)
    net.settle()

    net.lose = { case AuthzMessage.Deltas(_) => true; case _ => false }
    net.run(a)(_.write(_, Set(1)))
    net.settle()
    net.lose = _ => false
    assertEquals(b.replica.heads, a.replica.heads, "the event arrived")
    assertEquals(b.replica.state, Set.empty[Int], "but its delta was lost")

    net.disconnect(a, b)
    net.connect(a, b)
    net.settle()
    assertEquals(b.replica.state, Set(1))
  }

  test("revoked capabilities can not be used anymore, and their concurrent uses are dropped") {
    val (a, b, net, genesis) = setup()
    delegateAll(net, a, b, genesis)
    net.settle()
    val (bobsCapability, _) = b.replica.activeCapabilitiesOf(b.identity.getPublic).head

    // bob writes while a concurrently revokes
    net.disconnect(a, b)
    net.run(b)(_.write(_, Set(4)))
    net.run(a)(_.revoke(_, bobsCapability))
    net.connect(a, b)
    net.settle()

    assertEquals(a.replica.heads, b.replica.heads)
    assertEquals(a.replica.state, Set.empty[Int])
    assertEquals(b.replica.state, Set.empty[Int])
    intercept[IllegalArgumentException](net.run(b)(_.write(_, Set(5))))
  }

  test("unauthenticated peers are ignored") {
    val (a, _, _, _) = setup()
    val claimed      = Peer.Claimed(Uid.gen())
    assertEquals(a.protocol.addPeer(a.state, claimed).events, Nil)
    assertEquals(
      a.protocol.handleMessage(a.state, claimed, AuthzMessage.Events(PlumtreeMessage.Graft(Set.empty))).events,
      Nil
    )
  }

  test("message codec roundtrip") {
    import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray as write
    val (_, _, _, genesis) = setup()
    val encoded            = writeToArray(genesis)
    readFromArray[AuthzMessage](write[AuthzMessage](AuthzMessage.Events(PlumtreeMessage.Push(encoded)))) match
        case AuthzMessage.Events(PlumtreeMessage.Push(bytes)) => assertEquals(bytes.toSeq, encoded.toSeq)
        case other                                            => fail(s"unexpected $other")
    val graft = AuthzMessage.Events(PlumtreeMessage.Graft(Set(genesis.hash)))
    assertEquals(readFromArray[AuthzMessage](write[AuthzMessage](graft)), graft)
  }
}
