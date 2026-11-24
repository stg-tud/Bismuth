package ex2026policyBroadcast

import channels.MessagingIO
import channels.connection.tls.Tls
import channels.connection.{LocalMessageQueue, Peer, PeerConnectInfo, QueuedLocalConnection}
import channels.overlay.FullMeshOverlay
import channels.crypto.Ed25519.given
import channels.crypto.{IdentityFactory, PrivateIdentity, PublicIdentity}
import PolicyBroadcast.mutate
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{Bottom, Lattice, ReplicaId, Uid}
import rdts.datatypes.GrowOnlyCounter

/** Two counters, `a` may only be written by replica A, and `b` only by replica B. */
case class Counters(a: GrowOnlyCounter = GrowOnlyCounter.zero, b: GrowOnlyCounter = GrowOnlyCounter.zero)
    derives Lattice, Bottom {
  override def toString: String = s"a=${a.value} b=${b.value}"
}

object Counters {
  given JsonValueCodec[Counters] = {
    import channels.JsoniterCodecs.given
    JsonCodecMaker.make
  }

  private def untouched(counter: GrowOnlyCounter) = Bottom[GrowOnlyCounter].isEmpty(counter)

  /** `a` may only read and write counter a, `b` only counter b. Everyone else may read both, but write neither.
    * Only authenticated peers may read anything.
    */
  def policy(a: PublicIdentity, b: PublicIdentity): BroadcastPolicy[Counters] = new BroadcastPolicy[Counters] {
    def mayRead(reader: Peer, delta: Counters): Boolean =
      reader.isAuthenticated && (Uid.unwrap(reader.uid) match
          case a.id => untouched(delta.b)
          case b.id => untouched(delta.a)
          case _    => true
      )

    def mayWrite(author: PublicIdentity, delta: Counters): Boolean =
      author match
          case `a` => untouched(delta.b)
          case `b` => untouched(delta.a)
          case _   => untouched(delta.a) && untouched(delta.b)
  }
}

/** Replicas A and B write their own counter, C and D read both, with [[PolicyBroadcast]] over mutually
  * authenticated TLS on top of local connections. Replicas are identified by their public keys, which TLS
  * authenticates, deltas are signed by their author.
  */
object CounterDemo {

  type Protocol = PolicyBroadcast[Counters]

  final class Replica(val name: String, val identity: PrivateIdentity, policy: BroadcastPolicy[Counters]) {
    val replicaId: ReplicaId = ReplicaId(Uid(identity.getPublic.id))
    val tls: Tls             = Tls(identity)
    val io: MessagingIO[Protocol] =
      new MessagingIO[Protocol](using PolicyBroadcast(identity, policy), PolicyBroadcast.messageCodec[Counters])(
        replicaId,
        PolicyBroadcast.State(Counters()),
        state => println(s"  $name sees $state"),
        // identify as authenticated, so only authenticated connections are used
        MessagingIO.Config(overlay =
          Some(FullMeshOverlay(PeerConnectInfo(Peer.Authenticated(replicaId.uid), Set.empty)))
        )
      )
    def state: Counters = io.state.value

    /** Accept TLS connections. */
    def listen(queue: LocalMessageQueue): QueuedLocalConnection =
        val link = QueuedLocalConnection(name, queue)
        io.addServerConnection(tls.listen(link.server))
        link

    def connect(server: QueuedLocalConnection): Unit = io.addClientConnection(tls.connect(server.client(name)))
  }

  def main(args: Array[String]): Unit = {
    val queue = LocalMessageQueue()

    val idA    = IdentityFactory.createNewIdentity
    val idB    = IdentityFactory.createNewIdentity
    val policy = Counters.policy(idA.getPublic, idB.getPublic)

    val a = Replica("A", idA, policy)
    val b = Replica("B", idB, policy)
    val c = Replica("C", IdentityFactory.createNewIdentity, policy)
    val d = Replica("D", IdentityFactory.createNewIdentity, policy)

    def settle(): Unit = while queue.nonEmpty do { queue.deliverAll(); () }

    def show(): Unit = List(a, b, c, d).foreach(r => println(s"  ${r.name}: ${r.state}"))

    def write(replica: Replica)(mutator: ReplicaId ?=> Counters => Counters): Unit =
        println(s"${replica.name} writes")
        try replica.io.mutate(mutator(using replica.replicaId))
        catch case e: IllegalArgumentException => println(s"  rejected: ${e.getMessage}")
        settle()

    def connect(client: Replica, server: QueuedLocalConnection): Unit =
        client.connect(server)
        settle()

    val linkA = a.listen(queue)
    val linkB = b.listen(queue)
    val linkC = c.listen(queue)

    write(a)(s => Counters(a = s.a.inc()))
    write(b)(s => Counters(b = s.b.inc()))

    println("connecting C to A and B")
    connect(c, linkA)
    connect(c, linkB)
    println("connecting D to C only, D gets the signed writes of A and B through C")
    connect(d, linkC)
    println("connecting B to A, neither may read the other")
    connect(b, linkA)
    show()

    write(a)(s => Counters(a = s.a.add(2)))
    println("no forwarding: D does not get A's new write")
    show()

    println("B tries to write counter a")
    write(b)(s => Counters(a = s.a.inc()))
    println("C tries to write counter b")
    write(c)(s => Counters(b = s.b.inc()))
    show()

    List(a, b, c, d).foreach(_.io.globalAbort.closeRequest = true)
  }
}
