package channels

import channels.connection.{ChannelResolver, Connection, ConnectionDescriptor, LatentConnection, LocalConnectionRegistry, LocalMessageQueue, PeerConnectInfo, QueuedLocalConnection}
import channels.overlay.HyParViewStateMachine
import channels.overlay.HyParViewStateMachine.{HyParViewConfig, Link}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{ReplicaId, Uid}

import scala.collection.mutable

/** Whole-network HyParView scenarios over the deterministic queued transport.
  *
  * Besides the scenario-specific assertions, every test checks the overlay's central invariants once the network is
  * quiet: the driver holds exactly the connections the overlay knows about, no handshake is left pending, and active
  * edges are symmetric.
  */
class HyParViewNetworkTest extends munit.FunSuite {

  given JsonValueCodec[Set[String]] = JsonCodecMaker.make

  private val smallConfig = HyParViewConfig(
    activeViewSize = 2,
    passiveViewSize = 4,
    activeRandomWalkLength = 3,
    passiveRandomWalkLength = 1,
    shuffleRandomWalkLength = 1,
    shuffleActiveSample = 1,
    shufflePassiveSample = 3,
  )

  final class Node(val id: String, val io: BroadcastIO[Set[String]], val info: PeerConnectInfo) {
    def hv: HyParViewStateMachine = io.overlayController.asInstanceOf[HyParViewStateMachine]
    def uid: Uid                  = info.uid
    override def toString: String = id
  }

  final class Net(config: HyParViewConfig = smallConfig) {
    val queue: LocalMessageQueue                   = LocalMessageQueue()
    private val links                              = mutable.Map.empty[String, QueuedLocalConnection]
    private val registry                           = LocalConnectionRegistry(links)
    private val unreachable                        = mutable.Set.empty[(String, String)]
    val nodes: mutable.LinkedHashMap[String, Node] = mutable.LinkedHashMap.empty

    def name(uid: Uid): String = nodes.valuesIterator.find(_.uid == uid).map(_.id).getOrElse(uid.show)

    def node(id: String): Node = {
      links(id) = QueuedLocalConnection(id, queue)
      val replica  = ReplicaId.predefined(id)
      val info     = PeerConnectInfo(replica.uid, Set(ConnectionDescriptor.QueuedLocal(id)))
      val resolver = new ChannelResolver {
        override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] = details match
            case ConnectionDescriptor.QueuedLocal(target) if unreachable.contains((id, target)) => None
            case _ => registry.connect(details)
      }
      val io = BroadcastIO[Set[String]](
        replica,
        _ => (),
        overlay = Some(HyParViewStateMachine.empty(info, config, (lo, _) => lo, _ => true)),
        resolver = resolver,
      )
      val n = Node(id, io, info)
      nodes(id) = n
      io.addServerConnection(links(id).server)
      n
    }

    /** `from` can no longer dial `to`. */
    def block(from: Node, to: Node): Unit = unreachable += ((from.id, to.id))

    def drain(): Unit = {
      var rounds = 0
      while queue.nonEmpty && rounds < 1000 do
          queue.deliverAll()
          rounds += 1
      assert(rounds < 1000, s"queue did not quiesce, remaining=${queue.size}")
    }

    /** One round of all timers on all nodes (shuffles, handshake timeouts, repair), then drain. */
    def round(): Unit =
        nodes.values.foreach(_.io.fireTimers())
        drain()

    def join(newcomer: Node, contact: Node): Unit =
        newcomer.io.bootstrapVia(contact.info.channelConnectors.head)
        drain()

    def active(n: Node): Set[String]  = n.hv.activeView.map(name)
    def passive(n: Node): Set[String] = n.hv.passiveView.map(name)

    def describe: String =
      nodes.values.map(n =>
        s"${n.id}: active=${active(n).toList.sorted} passive=${passive(n).toList.sorted} links=${n.hv.links}"
      )
        .mkString("\n", "\n", "\n")

    def assertInvariants(): Unit =
      nodes.values.foreach { n =>
        assertEquals(n.io.connectionIds, n.hv.links.keySet, s"${n.id}: driver and overlay disagree${describe}")
        assert(
          !n.hv.links.valuesIterator.exists(_.isInstanceOf[Link.Pending]),
          s"${n.id}: handshake left pending${describe}"
        )
        active(n).foreach { other =>
          assert(active(nodes(other)).contains(n.id), s"${n.id} -> $other is one-sided${describe}")
        }
      }
  }

  test("a line of joins grows into a connected overlay with full active views") {
    val net   = Net()
    val nodes = (0 until 8).map(i => net.node(s"n$i"))
    nodes.sliding(2).foreach { case Seq(a, b) => net.join(b, a) }
    (0 until 10).foreach(_ => net.round())

    nodes.foreach(n => assertEquals(net.active(n).size, smallConfig.activeViewSize, net.describe))
    net.assertInvariants()
  }

  test("a node with a free slot fills it as soon as it learns a free peer") {
    val net = Net()
    val x   = net.node("x")
    val a   = net.node("a")
    net.join(a, x)
    assertEquals(net.active(x), Set("a"))

    val free = net.node("free")
    x.io.discover(Set(free.info))
    net.drain()

    assertEquals(net.active(x), Set("a", "free"), net.describe)
    net.assertInvariants()
  }

  test("refused and unreachable candidates leave no connections behind, and the next candidate is tried") {
    val net                = Net()
    val Seq(x, a, f, g, h) = Seq("x", "a", "f", "g", "h").map(net.node)
    net.join(a, x)
    // f, g, h form a full triangle; x knows f and an unreachable peer
    net.join(g, f)
    net.join(h, f)
    assertEquals(net.active(f).size, 2, net.describe)
    val gone = net.node("gone")
    net.block(x, gone)
    x.io.discover(Set(f.info, gone.info))
    net.drain()

    // f refused (full), gone was dropped; x is still short
    assert(!net.passive(x).contains("gone"), net.describe)
    assertEquals(net.active(x), Set("a"), net.describe)
    net.assertInvariants()

    // a free peer appears
    val free = net.node("free")
    x.io.discover(Set(free.info))
    net.drain()
    assertEquals(net.active(x), Set("a", "free"), net.describe)
    net.assertInvariants()
  }

  test("a failed neighbor is replaced from the passive view") {
    val net   = Net()
    val nodes = (0 until 6).map(i => net.node(s"n$i"))
    nodes.tail.foreach(n => net.join(n, nodes.head))
    (0 until 5).foreach(_ => net.round())
    net.assertInvariants()

    // crash one neighbor of n0: its connections close, it stops reacting
    val crashed = net.nodes(net.active(nodes.head).head)
    nodes.foreach(n => n.io.connectionTo(crashed.uid).foreach(_.close()))
    crashed.io.globalAbort.abort()
    net.nodes.remove(crashed.id)
    net.drain()
    (0 until 5).foreach(_ => net.round())

    net.nodes.values.foreach(n => assert(!net.active(n).contains(crashed.id), net.describe))
    assertEquals(net.active(nodes.head).size, smallConfig.activeViewSize, net.describe)
  }

  test("the shuffle reply reaches the origin over a temporary connection if the walk ends at a non-neighbor") {
    val net                         = Net()
    val Seq(origin, middle, end, q) = Seq("origin", "middle", "end", "q").map(net.node)
    // origin can not dial end, so no direct origin–end edge forms; end can still reach origin
    net.block(origin, end)
    net.join(middle, origin)
    net.join(end, middle)
    net.join(q, end)
    // a ring origin – middle – end – q – origin; end is full, so it does not dial origin on its own
    assertEquals(net.active(origin), Set("middle", "q"), net.describe)
    assertEquals(net.active(middle), Set("origin", "end"), net.describe)
    assertEquals(net.active(end), Set("middle", "q"), net.describe)

    // a peer only end knows about
    val secret = PeerConnectInfo(Uid.predefined("secret"), Set(ConnectionDescriptor.QueuedLocal("secret")))
    end.io.discover(Set(secret))
    net.drain()

    origin.io.fireTimers() // shuffle: origin -> middle (ttl 1) -> end (ttl 0), end replies over a temporary connection
    net.drain()
    assert(origin.hv.known.contains(secret.uid), net.describe)
    net.assertInvariants()
  }
}
