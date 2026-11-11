package channels.broadcast.authz

import channels.broadcast.Routing.{Effect, Step}
import channels.connection.Peer
import channels.crypto.{Hash, PrivateIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import munit.FunSuite
import rdts.base.Uid
import rdts.filters.PermissionTree
import channels.broadcast.authz.AuthzTestSupport.{*, given}
import channels.JsoniterCodecsJvm.ardtEventCodec

import scala.collection.mutable

/** Drives [[AuthzReplica]]s directly, routing their `Send` effects between each other. */
class AuthzProtocolTest extends FunSuite {

  final class Node(val identity: PrivateIdentity, var replica: AuthzReplica[Set[Int]]) {
    val peer: Peer                       = Peer.Authenticated(Uid(identity.getPublic.id))
    val delivered: mutable.Buffer[Set[Int]] = mutable.Buffer.empty
  }

  final class Network(nodes: Node*) {
    private val inbox = mutable.Queue.empty[(Node, Node, AuthzMessage)]

    /** Apply a transition of `node`, enqueueing its sends and recording its deliveries. */
    def run(node: Node)(f: AuthzReplica[Set[Int]] => Step[AuthzReplica[Set[Int]], AuthzMessage, Set[Int]]): Unit = {
      val step = f(node.replica)
      node.replica = step.state
      step.events.foreach {
        case Effect.Send(peers, message) =>
          peers.foreach(p => nodes.find(_.peer == p).foreach(to => inbox.enqueue((node, to, message))))
        case Effect.Deliver(state)  => node.delivered += state
        case Effect.StartTimer(_)   => ()
      }
    }

    def connect(a: Node, b: Node): Unit = {
      run(a)(_.addPeer(b.peer))
      run(b)(_.addPeer(a.peer))
    }

    /** Drop all queued messages matching `p`, e.g., to simulate a connection failing. */
    def drop(p: AuthzMessage => Boolean): Unit = inbox.filterInPlace((_, _, message) => !p(message))

    def deliverAll(): Unit =
      while inbox.nonEmpty do
          val (from, to, message) = inbox.dequeue()
          run(to)(_.handleMessage(from.peer, message))
  }

  private def setup(): (Node, Node, Network, ArdtEvent) = {
    val root    = newPrivateIdentity()
    val bob     = newPrivateIdentity()
    val genesis = Authorization.createGenesis(root)
    val a       = Node(root, AuthzReplica.initial[Set[Int]](root.getPublic, genesis.hash))
    val b       = Node(bob, AuthzReplica.initial[Set[Int]](bob.getPublic, genesis.hash))
    val net     = Network(a, b)
    net.run(a)(_.receiveLocally(genesis))
    net.connect(a, b)
    net.deliverAll()
    (a, b, net, genesis)
  }

  test("a new replica bootstraps the event graph from its peer") {
    val (a, b, _, genesis) = setup()
    assertEquals(b.replica.heads, Set(genesis.hash))
    assertEquals(b.replica.heads, a.replica.heads)
  }

  test("deltas only reach replicas that may read them, delegation and writes are disseminated") {
    val (a, b, net, genesis) = setup()

    net.run(a)(_.write(a.identity, Set(1)))
    net.deliverAll()
    assertEquals(a.replica.state, Set(1))
    assertEquals(b.replica.heads, a.replica.heads, "the event is disseminated")
    assertEquals(b.replica.state, Set.empty[Int], "but bob may not read the delta")

    net.run(a)(_.delegate(a.identity, genesis.hash, b.identity.getPublic, PermissionTree.allow, PermissionTree.allow))
    net.deliverAll()
    assertEquals(b.replica.state, Set(1), "the delegation makes earlier deltas readable")

    net.run(a)(_.write(a.identity, Set(2)))
    net.deliverAll()
    assertEquals(b.replica.state, Set(1, 2))
    assertEquals(b.delivered.lastOption, Some(Set(1, 2)))

    net.run(b)(_.write(b.identity, Set(3)))
    net.deliverAll()
    assertEquals(a.replica.state, Set(1, 2, 3))
  }

  test("deltas that were lost in transit are requested again on reconnect") {
    val (a, b, net, genesis) = setup()
    net.run(a)(_.delegate(a.identity, genesis.hash, b.identity.getPublic, PermissionTree.allow, PermissionTree.allow))
    net.deliverAll()

    net.run(a)(_.write(a.identity, Set(1)))
    net.drop { case AuthzMessage.Deltas(_) => true; case _ => false }
    net.deliverAll()
    assertEquals(b.replica.heads, a.replica.heads, "the event arrived")
    assertEquals(b.replica.state, Set.empty[Int], "but its delta was lost")
    assertEquals(b.replica.deltasWithoutValue.size, 1)

    net.run(b)(_.removePeer(a.peer))
    net.run(b)(_.addPeer(a.peer))
    net.deliverAll()
    assertEquals(b.replica.state, Set(1))
    assertEquals(b.replica.deltasWithoutValue, Set.empty[Hash])
  }

  test("revoked capabilities can not be used anymore") {
    val (a, b, net, genesis) = setup()
    net.run(a)(_.delegate(a.identity, genesis.hash, b.identity.getPublic, PermissionTree.allow, PermissionTree.allow))
    net.deliverAll()
    val (bobsCapability, _) = b.replica.activeCapabilitiesOf(b.identity.getPublic).head

    net.run(a)(_.revoke(a.identity, bobsCapability))
    net.deliverAll()
    intercept[IllegalArgumentException](net.run(b)(_.write(b.identity, Set(4))))
  }

  test("events with missing predecessors wait for them, and deltas for their events") {
    val (a, b, net, genesis) = setup()
    net.run(a)(_.delegate(a.identity, genesis.hash, b.identity.getPublic, PermissionTree.allow, PermissionTree.allow))
    net.deliverAll()

    // create two writes at a without bob being connected
    val solo = a.replica.removePeer(b.peer).state
    val w1   = solo.write(a.identity, Set(10)).state
    val w2   = w1.write(a.identity, Set(11)).state
    val e1   = w2.eventGraph.events(w1.heads.head)._1
    val e2   = w2.eventGraph.events(w2.heads.head)._1
    val deltas = List(e1, e2).map(e => AuthzMessage.DeltaValue(e.hash, w2.deltaValue(e.hash).get))

    // deltas first, then the second event, then the first
    net.run(b)(_.handleMessage(a.peer, AuthzMessage.Deltas(deltas)))
    net.run(b)(_.handleMessage(a.peer, AuthzMessage.Events(List(writeToArray(e2)))))
    assertEquals(b.replica.state, Set.empty[Int])
    assertEquals(b.replica.missing, Set(e1.hash))
    net.run(b)(_.handleMessage(a.peer, AuthzMessage.Events(List(writeToArray(e1)))))
    assertEquals(b.replica.state, Set(10, 11))
    assertEquals(b.replica.missing, Set.empty)
  }

  test("unauthenticated peers are ignored") {
    val (a, _, _, _) = setup()
    val claimed        = Peer.Claimed(Uid.gen())
    assertEquals(a.replica.addPeer(claimed).events, Nil)
    assertEquals(a.replica.handleMessage(claimed, AuthzMessage.Bootstrap).events, Nil)
  }

  test("message codec roundtrip") {
    import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray as write}
    val (_, _, _, genesis) = setup()
    val message = AuthzMessage.Events(List(writeToArray(genesis)))
    readFromArray[AuthzMessage](write[AuthzMessage](message)) match
        case AuthzMessage.Events(List(bytes)) => assertEquals(bytes.toSeq, writeToArray(genesis).toSeq)
        case other                            => fail(s"unexpected $other")
    assertEquals(readFromArray[AuthzMessage](write[AuthzMessage](AuthzMessage.Bootstrap)), AuthzMessage.Bootstrap)
  }
}
