package replication.authz

import channels.broadcast.BroadcastProtocol.{Effect, Timer}
import channels.connection.{ByteBufferMessageBuffer, MessageBuffer, Peer}
import channels.crypto.{PrivateIdentity, PublicIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import munit.FunSuite
import rdts.base.Uid
import rdts.filters.PermissionTree
import replication.JsoniterCodecsJvm.ardtEventCodec
import replication.authz.AuthzTestSupport.{*, given}
import replication.crypto.Hash
import replication.sync.{ConnectionManager, MessageReceiver}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.{Random, Try}

/** Runs the same random operations on the legacy [[Replica]] + [[AntiEntropy]] and on the [[AuthzProtocol]], and
  * checks that every replica ends up with the same state in both after every synchronization.
  *
  * Event hashes differ between the two systems (commitments are salted randomly), so operations never refer to
  * hashes: only the root delegates and revokes, and every other replica has at most one active capability.
  */
class AuthzDifferentialTest extends FunSuite {
  import AuthzDifferentialTest.*

  private val permissions: List[(read: PermissionTree, write: PermissionTree)] = List(
    (PermissionTree.allow, PermissionTree.allow),
    (PermissionTree.allow, PermissionTree.empty),
    (PermissionTree.empty, PermissionTree.empty),
  )

  for seed <- 0 until 30 do
      test(s"legacy and protocol agree on random operations (seed $seed)") {
        val random     = Random(seed)
        val identities = Vector.fill(4)(newPrivateIdentity())
        val genesis    = Authorization.createGenesis(identities(0))
        val systems    = List(Legacy(identities, genesis), Protocol(identities, genesis))
        var nextValue  = 0

        def all[A](op: System => A): List[A] = systems.map(op)

        def assertSame(context: String): Unit = {
          val states = all(_.states)
          assertEquals(states(0), states(1), s"$context: states differ (legacy, protocol)")
        }

        all(_.sync()): Unit
        assertSame("after bootstrap")

        for step <- 0 until 40 do {
          val replica = 1 + random.nextInt(identities.size - 1)
          random.nextInt(10) match
              case 0 | 1 | 2 | 3 =>
                val writer = random.nextInt(identities.size)
                nextValue += 1
                val outcomes = all(s => Try(s.write(writer, nextValue)).isSuccess)
                assertEquals(outcomes(0), outcomes(1), s"step $step: write by $writer succeeded in only one system")
                coverage(if outcomes(0) then (if writer == 0 then "root write" else "delegated write") else "rejected write") += 1
              case 4 | 5 =>
                val (read, write) = permissions(random.nextInt(permissions.size))
                val eligible      = all(_.capabilityOf(0, replica).isEmpty)
                assertEquals(eligible(0), eligible(1), s"step $step: root knows different capabilities of $replica")
                if eligible(0) then all(_.delegate(replica, read, write)): Unit
              case 6 =>
                val eligible = all(_.capabilityOf(0, replica).nonEmpty)
                assertEquals(eligible(0), eligible(1), s"step $step: root knows different capabilities of $replica")
                if eligible(0) then
                    all(_.revoke(replica)): Unit
                    coverage("revocation") += 1
              case _ =>
                val before = all(_.states).head
                all(_.sync()): Unit
                assertSame(s"step $step")
                // a value disappearing from a replica can only be caused by an invalidating revocation
                if before.lazyZip(all(_.states).head).exists((b, a) => !(b subsetOf a)) then
                    coverage("invalidation") += 1
        }

        all(_.sync()): Unit
        assertSame("at the end")
      }

  test("random scenarios cover writes, rejections, revocations and invalidations") {
    List("root write", "delegated write", "rejected write", "revocation", "invalidation").foreach(kind =>
      assert(coverage(kind) > 0, s"no $kind in any scenario: $coverage")
    )
  }
}

object AuthzDifferentialTest {

  /** How often each kind of operation happened, over all scenarios. */
  private val coverage: mutable.Map[String, Int] = mutable.Map.empty.withDefaultValue(0)

  trait System {
    def states: List[Set[Int]]

    /** The active capability of `holder`, as known at `at`. */
    def capabilityOf(at: Int, holder: Int): Option[Hash]

    /** @throws IllegalArgumentException if `writer` may not write */
    def write(writer: Int, value: Int): Unit

    /** Root delegates to `delegatee`. */
    def delegate(delegatee: Int, read: PermissionTree, write: PermissionTree): Unit

    /** Root revokes the capability of `holder`. */
    def revoke(holder: Int): Unit

    /** Deliver all messages and repair until nothing is left to do. */
    def sync(): Unit
  }

  // ------------------------------------------------------------------------------------------------
  // legacy: Replica + AntiEntropy over an in-memory connection manager
  // ------------------------------------------------------------------------------------------------

  final class Legacy(identities: Vector[PrivateIdentity], genesis: ArdtEvent) extends System {
    private val queue = mutable.Queue.empty[() => Unit]
    private val antiEntropies = mutable.Map.empty[PublicIdentity, AntiEntropy]

    private class LocalConnections(self: PublicIdentity) extends ConnectionManager {
      def send(user: PublicIdentity, msg: MessageBuffer): Unit = sendMultiple(user, List(msg))
      def sendMultiple(user: PublicIdentity, messages: Iterable[MessageBuffer]): Unit =
        messages.foreach { message =>
          // copy now, as broadcasts share buffers between receivers
          val bytes = message.convertToArray()
          // the legacy replica throws for rejected input, which only ends the handling of that message
          queue.enqueue(() =>
            Try(antiEntropies(user).receivedMessage(ByteBufferMessageBuffer(ByteBuffer.wrap(bytes)), self)): Unit
          )
        }
      def broadcast(messages: Iterable[MessageBuffer]): Unit = connectedPeers.foreach(sendMultiple(_, messages))
      def listenAddress: Option[(String, Int)]               = None
      def shutdown(): Unit                                   = ()
      def acceptIncomingConnections(): Unit                  = ()
      def connectTo(host: String, port: Int): Unit           = ()
      def disconnect(userId: PublicIdentity): Unit           = ()
      def connectedPeers: Set[PublicIdentity]                = antiEntropies.keySet.toSet - self
    }

    private val noControlPlane: MessageReceiver[ByteBuffer] = (_, _) => ()

    private val replicas: Vector[Replica[Set[Int]]] = identities.map { identity =>
      Replica[Set[Int]](
        genesis.hash,
        identity,
        r => {
          val ae = AntiEntropy(r, _ => LocalConnections(identity.getPublic), _ => noControlPlane)
          antiEntropies(identity.getPublic) = ae
          ae
        },
        _ => ()
      )
    }
    // creates the anti-entropy of every replica
    replicas.foreach(_.listenAddress)
    assert(replicas(0).receiveEvent(writeToArray(genesis)).isRight)
    for a <- identities; b <- identities if a != b do antiEntropies(a.getPublic).connectionEstablished(b.getPublic)

    def states: List[Set[Int]] = replicas.map(_.state).toList

    def capabilityOf(at: Int, holder: Int): Option[Hash] =
      replicas(at).activeCapabilitiesOf(identities(holder).getPublic).headOption.map(_._1)

    def write(writer: Int, value: Int): Unit = replicas(writer).mutateState(_ => Set(value))

    def delegate(delegatee: Int, read: PermissionTree, write: PermissionTree): Unit =
      replicas(0).createDelegation(genesis.hash, identities(delegatee).getPublic, read, write)

    def revoke(holder: Int): Unit = replicas(0).createRevocation(capabilityOf(0, holder).get)

    def sync(): Unit = {
      var rounds = 0
      while queue.nonEmpty && rounds < 100 do {
        while queue.nonEmpty do queue.dequeue()()
        antiEntropies.values.foreach(_.requestMissing())
        rounds += 1
      }
    }
  }

  // ------------------------------------------------------------------------------------------------
  // protocol: AuthzReplica, routing its effects
  // ------------------------------------------------------------------------------------------------

  final class Protocol(identities: Vector[PrivateIdentity], genesis: ArdtEvent) extends System {
    private val peers: Vector[Peer] = identities.map(i => Peer.Authenticated(Uid(i.getPublic.id)))
    private val replicas: Array[AuthzReplica[Set[Int]]] =
      identities.map(i => AuthzReplica.initial[Set[Int]](i.getPublic, genesis.hash)).toArray
    private val queue  = mutable.Queue.empty[(from: Int, to: Int, message: AuthzMessage)]
    private val timers = mutable.Set.empty[Int]

    private def run(node: Int)(
        f: AuthzReplica[Set[Int]] => channels.broadcast.BroadcastProtocol.Step[AuthzReplica[Set[Int]], AuthzMessage, Set[Int]]
    ): Unit = {
      val step = f(replicas(node))
      replicas(node) = step.state
      step.events.foreach {
        case Effect.Send(to, message) => to.foreach(p => queue.enqueue((node, peers.indexOf(p), message)))
        case Effect.Deliver(_)        => ()
        case Effect.StartTimer(_)     => timers += node: Unit
      }
    }

    run(0)(_.receiveLocally(genesis))
    for a <- identities.indices; b <- identities.indices if a != b do run(a)(_.addPeer(peers(b)))

    def states: List[Set[Int]] = replicas.map(_.state).toList

    def capabilityOf(at: Int, holder: Int): Option[Hash] =
      replicas(at).activeCapabilitiesOf(identities(holder).getPublic).headOption.map(_._1)

    def write(writer: Int, value: Int): Unit = run(writer)(_.write(identities(writer), Set(value)))

    def delegate(delegatee: Int, read: PermissionTree, write: PermissionTree): Unit =
      run(0)(_.delegate(identities(0), genesis.hash, identities(delegatee).getPublic, read, write))

    def revoke(holder: Int): Unit = run(0)(_.revoke(identities(0), capabilityOf(0, holder).get))

    def sync(): Unit = {
      var rounds = 0
      while (queue.nonEmpty || timers.nonEmpty) && rounds < 100 do {
        while queue.nonEmpty do
            val (from, to, message) = queue.dequeue()
            run(to)(_.handleMessage(peers(from), message))
        val due = timers.toList
        timers.clear()
        due.foreach(node => run(node)(_.timerFired(Timer.Repair)))
        rounds += 1
      }
    }
  }
}
