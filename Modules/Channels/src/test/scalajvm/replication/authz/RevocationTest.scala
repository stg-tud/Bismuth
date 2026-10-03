package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.channels.PrivateIdentity
import crypto.{Hash, PublicIdentity}
import munit.{FunSuite, Location}
import rdts.filters.PermissionTree
import replication.JsoniterCodecsJvm.ardtEventCodec
import replication.authz.ArdtEvent.Payload.{Capability, Revocation}
import replication.authz.AuthzTestSupport.{*, given}

import java.security.PrivateKey
import scala.collection.mutable

/** Tests how receiving a revocation invalidates the deltas of the revoked capabilities that are not causally before it.
  *
  * Every scenario builds an event graph by hand and lets a replica owned by the root identity receive it. The replica's
  * state is compared with the expected state, and the expected state with [[Authorization.materialize]] over a delta
  * value store holding every delta value that the replica was given.
  *
  * In the graph sketches in the comments, `x <- y` means that `x` is a parent of `y`.
  */
class RevocationTest extends FunSuite {
  import RevocationTest.*

  /** A root-owned replica created by `create` that has received the genesis event */
  final private class Fixture[R <: ReplicaUnderTest](create: Factory[R]) {
    private val rootIdentity: PrivateIdentity = newPrivateIdentity()

    val root: Principal                         = Principal(rootIdentity.getPublic, rootIdentity.identityKey.getPrivate)
    val genesis: ArdtEvent                      = Authorization.createGenesis(rootIdentity)
    val notifications: mutable.Buffer[Set[Int]] = mutable.Buffer.empty

    private var antiEntropy: MockAntiEntropy | Null   = null
    private val everyDelta: DeltaValueStore[Set[Int]] = DeltaValueStore[Set[Int]]()

    val replica: R = create(
      genesis.hash,
      rootIdentity,
      r => {
        val mock = MockAntiEntropy(r)
        antiEntropy = mock
        mock
      },
      state => notifications += state: Unit
    )

    replica.start()
    receiveEvent(genesis)

    def principal(): Principal = {
      val (id, key) = newIdentity()
      Principal(id, key)
    }

    /** A delegation of full permissions to `delegatee` by `delegator` using `capability` */
    def grant(delegatee: Principal, delegator: Principal, capability: ArdtEvent, parents: Node*): ArdtEvent =
      buildEvent(
        Capability(delegatee.id, PermissionTree.allow, PermissionTree.allow),
        delegator.id,
        delegator.key,
        parents.map(hashOf).toSet,
        capability.hash
      )

    /** A delta event of `author` using `capability`, together with its delta value */
    def write(value: Set[Int], author: Principal, capability: ArdtEvent, parents: Node*): Write = {
      val (event, revealed) = buildDeltaEvent(value, author.id, author.key, parents.map(hashOf).toSet, capability.hash)
      Write(event, revealed)
    }

    /** A revocation of `capability` by the root using the genesis capability */
    def revoke(capability: ArdtEvent, parents: Node*): ArdtEvent = revokeBy(root, genesis, capability, parents*)

    def revokeBy(revoker: Principal, authorization: ArdtEvent, capability: ArdtEvent, parents: Node*): ArdtEvent =
      buildEvent(Revocation(capability.hash), revoker.id, revoker.key, parents.map(hashOf).toSet, authorization.hash)

    /** Receives the events in order, each delta event directly followed by its delta value */
    def receive(nodes: Node*)(using Location): Unit = nodes.foreach {
      case event: ArdtEvent => receiveEvent(event)
      case write: Write     =>
        receiveEvent(write)
        receiveValue(write)
    }

    /** Receives only the event, not the delta value of a delta event */
    def receiveEvent(node: Node)(using Location): Unit = {
      val event = node match {
        case event: ArdtEvent => event
        case write: Write     => write.event
      }
      assertEquals(replica.receiveEvent(writeToArray(event)), Right(Some(event.hash)))
    }

    def receiveValue(write: Write): Unit = {
      everyDelta.put(
        replica.graph.events(write.hash)._2,
        readFromArray[Set[Int]](write.revealed.value),
        write.revealed.witness
      )
      replica.receiveDelta(write.hash, write.revealed)
    }

    /** Hands the replica a delta value that it must reject and not store */
    def receiveRejectedValue(write: Write)(using Location): Unit = {
      everyDelta.put(
        replica.graph.events(write.hash)._2,
        readFromArray[Set[Int]](write.revealed.value),
        write.revealed.witness
      )
      intercept[IllegalArgumentException](replica.receiveDelta(write.hash, write.revealed))
      assertEquals(replica.delta(write.hash), None)
    }

    /** The event indices of the deltas that the replica itself created and broadcast, whose values are recorded for the
      * oracle
      */
    def broadcastIndices(): Set[Int] = {
      val broadcast               = antiEntropy.nn.broadcastedDeltas
      def author(eventHash: Hash) = replica.graph.events(eventHash)._1.author
      broadcast.foreach(d =>
        everyDelta.put(
          replica.graph.events(d.eventHash)._2,
          readFromArray[Set[Int]](d.delta.value),
          d.delta.witness
        )
      )
      broadcast.map(d => replica.graph.events(d.eventHash)._2).toSet
    }

    def oracle: Set[Int] = Authorization.materialize(replica.graph, everyDelta)

    def assertState(expected: Set[Int])(using Location): Unit = {
      assertEquals(oracle, expected, "Authorization.materialize (the oracle) disagrees with the expected state")
      assertEquals(replica.state, expected, "the replica's state differs from the expected state")
    }

    def assertInvalidated(revocation: ArdtEvent, expected: Write*)(using Location): Unit =
      assertEquals(
        replica.invalidatedBy(revocation.hash),
        expected.map(w => replica.graph.events(w.hash)._2).toSet,
        "deltasInvalidatedBy returned the wrong deltas"
      )
  }

  private val variants: Seq[(String, Factory[ReplicaUnderTest])] = Seq(
    ("Replica", new PlainReplica(_, _, _, _)),
    ("SnapshotAwareReplica without snapshot", new SnapshotReplica(_, _, _, _)),
  )

  for (variant, create) <- variants do revocationTests(variant, create)

  private def revocationTests(variant: String, create: Factory[ReplicaUnderTest]): Unit = {

    // --- 1. revocation on the current heads ---

    test(s"$variant: a revocation built on all current heads invalidates nothing and leaves the state unchanged") {
      val fx    = Fixture(create)
      val alice = fx.principal()
      val bob   = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val capB  = fx.grant(bob, fx.root, fx.genesis, capA)
      val dA    = fx.write(Set(1), alice, capA, capB)
      val dB    = fx.write(Set(2), bob, capB, capB)
      fx.receive(capA, capB, dA, dB)
      assertEquals(fx.replica.heads, Set(dA.hash, dB.hash))
      fx.assertState(Set(1, 2))
      val notificationsBefore = fx.notifications.toList

      val revocation = fx.revoke(capA, dA, dB)
      fx.receive(revocation)

      fx.assertInvalidated(revocation)
      fx.assertState(Set(1, 2))
      assertEquals(fx.replica.delta(dA.hash), Some(Set(1)))
      assertEquals(fx.notifications.toList, notificationsBefore)
    }

    // --- 2. revocation concurrent to a use ---

    test(s"$variant: a revocation concurrent to a use of the revoked capability invalidates that use") {
      // capA <- dA0 <- dA1
      //         dA0 <- revocation
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA0        = fx.write(Set(1), alice, capA, capA)
      val dA1        = fx.write(Set(2), alice, capA, dA0)
      val revocation = fx.revoke(capA, dA0)
      fx.receive(capA, dA0, dA1)
      fx.assertState(Set(1, 2))

      fx.receive(revocation)

      fx.assertInvalidated(revocation, dA1)
      fx.assertState(Set(1))
      assertEquals(fx.replica.delta(dA1.hash), None)
      assertEquals(fx.replica.delta(dA0.hash), Some(Set(1)))
      assertEquals(fx.notifications.lastOption, Some(Set(1)))
    }

    // --- 3. transitive revocation ---

    test(
      s"$variant: revoking a capability invalidates concurrent uses of capabilities delegated from it, but not of unrelated ones"
    ) {
      // genesis -> capA (alice) -> capB (bob) -> capD (dave), capA -> capE (erin); capC (carol) is unrelated.
      // capA <- capB <- capD <- dB0 <- capC <- {dA, dB, dD, dC, capE <- dE}
      //                               capC <- revocation of capA
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val bob        = fx.principal()
      val carol      = fx.principal()
      val dave       = fx.principal()
      val erin       = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val capB       = fx.grant(bob, alice, capA, capA)
      val capD       = fx.grant(dave, bob, capB, capB)
      val dB0        = fx.write(Set(6), bob, capB, capD)
      val capC       = fx.grant(carol, fx.root, fx.genesis, dB0)
      val dA         = fx.write(Set(1), alice, capA, capC)
      val dB         = fx.write(Set(2), bob, capB, capC)
      val dD         = fx.write(Set(4), dave, capD, capC)
      val dC         = fx.write(Set(3), carol, capC, capC)
      val capE       = fx.grant(erin, alice, capA, capC)
      val dE         = fx.write(Set(5), erin, capE, capE)
      val revocation = fx.revoke(capA, capC)
      fx.receive(capA, capB, capD, dB0, capC, dA, dB, dD, dC, capE, dE)
      fx.assertState(Set(1, 2, 3, 4, 5, 6))

      fx.receive(revocation)

      fx.assertInvalidated(revocation, dA, dB, dD, dE)
      fx.assertState(Set(3, 6))
    }

    // --- 4. regression: use with a lower index than the revocation's earliest parent ---

    test(
      s"$variant: a concurrent use with a lower index than every parent of the revocation is invalidated (old index cutoff)"
    ) {
      // capA <- dA1 <- dA2   (received first, indices 2 and 3)
      // capA <- dR1 <- dR2 <- revocation   (indices 4, 5 and 6)
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA1        = fx.write(Set(1), alice, capA, capA)
      val dA2        = fx.write(Set(2), alice, capA, dA1)
      val dR1        = fx.write(Set(8), fx.root, fx.genesis, capA)
      val dR2        = fx.write(Set(9), fx.root, fx.genesis, dR1)
      val revocation = fx.revoke(capA, dR2)
      fx.receive(capA, dA1, dA2, dR1, dR2, revocation)

      val graph = fx.replica.graph
      assert(graph.events(dA2.hash)._2 < graph.events(dR2.hash)._2)
      assert(graph.concurrent(dA1.hash, revocation.hash))
      fx.assertInvalidated(revocation, dA1, dA2)
      fx.assertState(Set(8, 9))
    }

    // --- 5. uses that are causally before the revocation only through indirect paths ---

    test(s"$variant: a use that is causally before the revocation only through a merge event stays valid") {
      // capA <- dA <- merge <- revocation, capA <- dR <- merge, dA <- dA2 (concurrent to the revocation)
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA         = fx.write(Set(1), alice, capA, capA)
      val dR         = fx.write(Set(8), fx.root, fx.genesis, capA)
      val merge      = fx.write(Set(9), fx.root, fx.genesis, dR, dA)
      val dA2        = fx.write(Set(2), alice, capA, dA)
      val revocation = fx.revoke(capA, merge)
      fx.receive(capA, dA, dR, merge, dA2, revocation)

      fx.assertInvalidated(revocation, dA2)
      fx.assertState(Set(1, 8, 9))
    }

    test(
      s"$variant: a use that is causally before the revocation only through delegation and revocation events stays valid"
    ) {
      // capA <- c1 <- c2 <- c3 <- revocation
      // capA <- dA <- capB <- revocationOfB <- revocation
      // capA <- dLate (concurrent to the revocation)
      val fx            = Fixture(create)
      val alice         = fx.principal()
      val bob           = fx.principal()
      val capA          = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA            = fx.write(Set(1), alice, capA, capA)
      val c1            = fx.write(Set(11), fx.root, fx.genesis, capA)
      val c2            = fx.write(Set(12), fx.root, fx.genesis, c1)
      val c3            = fx.write(Set(13), fx.root, fx.genesis, c2)
      val capB          = fx.grant(bob, fx.root, fx.genesis, dA)
      val revocationOfB = fx.revoke(capB, capB)
      val dLate         = fx.write(Set(2), alice, capA, capA)
      val revocation    = fx.revoke(capA, c3, revocationOfB)
      fx.receive(capA, dA, c1, c2, c3, capB, revocationOfB, dLate, revocation)

      fx.assertInvalidated(revocation, dLate)
      fx.assertState(Set(1, 11, 12, 13))
    }

    // --- 6. sibling events with the same parents ---

    test(s"$variant: a revocation concurrent to all of several sibling uses invalidates all of them") {
      // capA <- {s1, s2, s3, dR, revocation}
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val s1         = fx.write(Set(1), alice, capA, capA)
      val s2         = fx.write(Set(2), alice, capA, capA)
      val s3         = fx.write(Set(3), alice, capA, capA)
      val dR         = fx.write(Set(9), fx.root, fx.genesis, capA)
      val revocation = fx.revoke(capA, capA)
      fx.receive(capA, s1, s2, s3, dR, revocation)

      fx.assertInvalidated(revocation, s1, s2, s3)
      fx.assertState(Set(9))
    }

    test(s"$variant: a revocation concurrent to only some of several sibling uses invalidates exactly those") {
      // capA <- {s1, s2, s3}, {s1, s2} <- revocation
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val s1         = fx.write(Set(1), alice, capA, capA)
      val s2         = fx.write(Set(2), alice, capA, capA)
      val s3         = fx.write(Set(3), alice, capA, capA)
      val revocation = fx.revoke(capA, s1, s2)
      fx.receive(capA, s1, s2, s3, revocation)

      fx.assertInvalidated(revocation, s3)
      fx.assertState(Set(1, 2))
    }

    test(s"$variant: the events of a delta decomposed by mutateState are all invalidated by a concurrent revocation") {
      val fx      = Fixture(create)
      val ownCapR = fx.grant(fx.root, fx.root, fx.genesis, fx.genesis) // the root's own, revocable capability
      fx.receive(ownCapR)

      fx.replica.mutateState(_ => Set(1, 2, 3), ownCapR.hash)
      val decomposed = fx.broadcastIndices()
      assertEquals(decomposed.size, 3)
      fx.assertState(Set(1, 2, 3))

      val revocation = fx.revoke(ownCapR, ownCapR)
      fx.receive(revocation)

      assertEquals(fx.replica.invalidatedBy(revocation.hash), decomposed)
      fx.assertState(Set.empty)
    }

    // --- 7. receive orders ---

    test(s"$variant: a revocation received after all concurrent events invalidates the concurrent uses") {
      // capA <- dA1 <- dA2, dA1 <- revocation, capA <- dR
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA1        = fx.write(Set(1), alice, capA, capA)
      val dA2        = fx.write(Set(2), alice, capA, dA1)
      val dR         = fx.write(Set(9), fx.root, fx.genesis, capA)
      val revocation = fx.revoke(capA, dA1)
      fx.receive(capA, dA1, dA2, dR, revocation)

      fx.assertInvalidated(revocation, dA2)
      fx.assertState(Set(1, 9))
    }

    test(s"$variant: concurrent uses received after the revocation are rejected, other concurrent deltas accepted") {
      // capA <- dA1 <- dA2, dA1 <- revocation, capA <- dR, capA <- capB (bob, delegated by alice) <- dB
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val bob        = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA1        = fx.write(Set(1), alice, capA, capA)
      val dA2        = fx.write(Set(2), alice, capA, dA1)
      val dR         = fx.write(Set(9), fx.root, fx.genesis, capA)
      val capB       = fx.grant(bob, alice, capA, capA)
      val dB         = fx.write(Set(3), bob, capB, capB)
      val revocation = fx.revoke(capA, dA1)
      fx.receive(capA, dA1, revocation)
      fx.assertState(Set(1))

      fx.receiveEvent(dA2)
      fx.receiveRejectedValue(dA2)
      fx.receive(dR)
      fx.receive(capB)
      fx.receiveEvent(dB)
      fx.receiveRejectedValue(dB)

      fx.assertState(Set(1, 9))
    }

    test(s"$variant: delta values arriving after the revocation are accepted only if causally before it") {
      // capA <- dA1 <- dA2, dA1 <- revocation; the events arrive before the revocation, their values after it
      val fx         = Fixture(create)
      val alice      = fx.principal()
      val capA       = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA1        = fx.write(Set(1), alice, capA, capA)
      val dA2        = fx.write(Set(2), alice, capA, dA1)
      val revocation = fx.revoke(capA, dA1)
      fx.receive(capA)
      fx.receiveEvent(dA1)
      fx.receiveEvent(dA2)
      fx.receive(revocation)
      fx.assertState(Set.empty)

      fx.receiveValue(dA1)
      fx.receiveRejectedValue(dA2)

      fx.assertState(Set(1))
    }

    // --- 8. multiple revocations ---

    test(s"$variant: two different capabilities revoked one after the other each invalidate their concurrent uses") {
      // capA <- capB <- {dA, dB, dB2, dR}, dB <- r1 (revokes capA) <- r2 (revokes capB)
      val fx    = Fixture(create)
      val alice = fx.principal()
      val bob   = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val capB  = fx.grant(bob, fx.root, fx.genesis, capA)
      val dA    = fx.write(Set(1), alice, capA, capB)
      val dB    = fx.write(Set(2), bob, capB, capB)
      val dB2   = fx.write(Set(3), bob, capB, capB)
      val dR    = fx.write(Set(9), fx.root, fx.genesis, capB)
      val r1    = fx.revoke(capA, dB)
      val r2    = fx.revoke(capB, r1)
      fx.receive(capA, capB, dA, dB, dB2, dR, r1)
      fx.assertInvalidated(r1, dA)
      fx.assertState(Set(2, 3, 9))

      fx.receive(r2)

      fx.assertInvalidated(r2, dB2)
      fx.assertState(Set(2, 9))
    }

    test(s"$variant: the same capability revoked twice in sequence invalidates only the uses concurrent to the first") {
      // capA <- dA1 <- r1 <- dR <- r2, capA <- dA2
      val fx    = Fixture(create)
      val alice = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA1   = fx.write(Set(1), alice, capA, capA)
      val dA2   = fx.write(Set(2), alice, capA, capA)
      val r1    = fx.revoke(capA, dA1)
      val dR    = fx.write(Set(9), fx.root, fx.genesis, r1)
      val r2    = fx.revoke(capA, dR)
      fx.receive(capA, dA1, dA2, r1, dR, r2)

      fx.assertState(Set(1, 9))
    }

    test(s"$variant: the same capability revoked twice concurrently, both concurrent to a use, invalidates it once") {
      // capA <- {dA, dR, r1}, dR <- r2 (alice revoking her own capability)
      val fx    = Fixture(create)
      val alice = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA    = fx.write(Set(1), alice, capA, capA)
      val dR    = fx.write(Set(9), fx.root, fx.genesis, capA)
      val r1    = fx.revoke(capA, capA)
      val r2    = fx.revokeBy(alice, capA, capA, dR)
      fx.receive(capA, dA, dR, r1)
      fx.assertInvalidated(r1, dA)
      fx.assertState(Set(9))

      fx.receive(r2)

      fx.assertState(Set(9))
    }

    test(
      s"$variant: a use causally before one revocation but concurrent to a second, later received one is invalidated"
    ) {
      // Suspected problem: capA <- dA <- r1, capA <- r2; r1 is received before r2
      val fx    = Fixture(create)
      val alice = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA    = fx.write(Set(1), alice, capA, capA)
      val r1    = fx.revoke(capA, dA)
      val r2    = fx.revoke(capA, capA)
      fx.receive(capA, dA, r1)
      fx.assertState(Set(1))

      fx.receive(r2)

      fx.assertState(Set.empty)
    }

    test(
      s"$variant: a use causally before one revocation but concurrent to a second, earlier received one is invalidated"
    ) {
      // Same graph as the previous test, but r2 is received before r1
      val fx    = Fixture(create)
      val alice = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val dA    = fx.write(Set(1), alice, capA, capA)
      val r1    = fx.revoke(capA, dA)
      val r2    = fx.revoke(capA, capA)
      fx.receive(capA, dA, r2)
      fx.assertInvalidated(r2, dA)
      fx.assertState(Set.empty)

      fx.receive(r1)

      fx.assertState(Set.empty)
    }

    test(
      s"$variant: a use of a delegated capability revoked causally after it is invalidated by a concurrent revocation of the delegating capability"
    ) {
      // Suspected problem, transitive: capA <- capB (bob, delegated by alice) <- dB <- r1 (revokes capB),
      // capB <- r2 (revokes capA, thereby transitively capB); r1 is received before r2
      val fx    = Fixture(create)
      val alice = fx.principal()
      val bob   = fx.principal()
      val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
      val capB  = fx.grant(bob, alice, capA, capA)
      val dB    = fx.write(Set(2), bob, capB, capB)
      val r1    = fx.revoke(capB, dB)
      val r2    = fx.revoke(capA, capB)
      fx.receive(capA, capB, dB, r1)
      fx.assertState(Set(2))

      fx.receive(r2)

      fx.assertState(Set.empty)
    }
  }

  // --- 9. SnapshotAwareReplica with snapshots ---

  private def snapshotFixture(): Fixture[SnapshotReplica] = Fixture(new SnapshotReplica(_, _, _, _))

  test("SnapshotAwareReplica: a snapshot taken before every invalidated delta is kept and the state is correct") {
    // capA <- dR | snapshot | dR <- dA0 <- {dA1, dR2, revocation}
    val fx    = snapshotFixture()
    val alice = fx.principal()
    val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
    val dR    = fx.write(Set(9), fx.root, fx.genesis, capA)
    fx.receive(capA, dR)
    fx.replica.createSnapshot()
    val snapshotVersion = fx.replica.graph.nextEventIndex - 1
    assertEquals(fx.replica.currentSnapshotVersion, snapshotVersion)

    val dA0        = fx.write(Set(2), alice, capA, dR)
    val dA1        = fx.write(Set(1), alice, capA, dA0)
    val dR2        = fx.write(Set(8), fx.root, fx.genesis, dA0)
    val revocation = fx.revoke(capA, dA0)
    fx.receive(dA0, dA1, dR2, revocation)

    fx.assertInvalidated(revocation, dA1)
    assertEquals(fx.replica.currentSnapshotVersion, snapshotVersion, "the snapshot should have been kept")
    fx.assertState(Set(2, 8, 9))
  }

  test("SnapshotAwareReplica: a snapshot containing an invalidated delta is discarded and the state is correct") {
    // capA <- dA, capA <- dR | snapshot | dR <- dR2 <- revocation
    val fx    = snapshotFixture()
    val alice = fx.principal()
    val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
    val dA    = fx.write(Set(1), alice, capA, capA)
    val dR    = fx.write(Set(9), fx.root, fx.genesis, capA)
    fx.receive(capA, dA, dR)
    fx.replica.createSnapshot()
    fx.assertState(Set(1, 9))

    val dR2        = fx.write(Set(8), fx.root, fx.genesis, dR)
    val revocation = fx.revoke(capA, dR2)
    fx.receive(dR2, revocation)

    fx.assertInvalidated(revocation, dA)
    assertEquals(fx.replica.currentSnapshotVersion, -1, "the snapshot should have been discarded")
    fx.assertState(Set(8, 9))
  }

  test("SnapshotAwareReplica: a valid delta value received after the snapshot for an event it covers is kept") {
    // Suspected problem: capA <- dR (value arrives after the snapshot) <- revocation, capA <- dA
    val fx    = snapshotFixture()
    val alice = fx.principal()
    val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
    val dR    = fx.write(Set(9), fx.root, fx.genesis, capA)
    fx.receive(capA)
    fx.receiveEvent(dR)
    fx.replica.createSnapshot()
    val snapshotVersion = fx.replica.graph.nextEventIndex - 1
    fx.receiveValue(dR)
    fx.assertState(Set(9))

    val dA         = fx.write(Set(1), alice, capA, capA)
    val revocation = fx.revoke(capA, dR)
    fx.receive(dA, revocation)

    fx.assertInvalidated(revocation, dA)
    assertEquals(fx.replica.currentSnapshotVersion, snapshotVersion, "the snapshot should have been kept")
    fx.assertState(Set(9))
  }

  test(
    "SnapshotAwareReplica: an invalidated delta value received after the snapshot for an event it covers is removed"
  ) {
    // capA <- dA (value arrives after the snapshot), capA <- dR <- revocation
    val fx    = snapshotFixture()
    val alice = fx.principal()
    val capA  = fx.grant(alice, fx.root, fx.genesis, fx.genesis)
    val dA    = fx.write(Set(1), alice, capA, capA)
    val dR    = fx.write(Set(9), fx.root, fx.genesis, capA)
    fx.receive(capA, dR)
    fx.receiveEvent(dA)
    fx.replica.createSnapshot()
    fx.receiveValue(dA)
    fx.assertState(Set(1, 9))

    val revocation = fx.revoke(capA, dR)
    fx.receive(revocation)

    fx.assertInvalidated(revocation, dA)
    fx.assertState(Set(9))
  }
}

object RevocationTest {
  final case class Principal(id: PublicIdentity, key: PrivateKey)

  /** A delta event together with its delta value */
  final case class Write(event: ArdtEvent, revealed: RevealedValue) {
    def hash: Hash = event.hash
  }

  type Node = ArdtEvent | Write

  def hashOf(node: Node): Hash = node match {
    case event: ArdtEvent => event.hash
    case write: Write     => write.hash
  }

  /** Access to the internals of a replica */
  trait Inspection {
    def graph: ArdtEventGraph[Set[Int]]

    /** The event indices of the deltas the replica invalidated when receiving `revocation`. Since receiving it removes their
      * values, and `deltasInvalidatedBy` only returns deltas whose value is still stored, its result is recorded at
      * that point. For a revocation the replica did not process, e.g. one built on all current heads, this asks
      * `deltasInvalidatedBy` now instead.
      */
    def invalidatedBy(revocation: Hash): Set[Int]
  }

  type ReplicaUnderTest = Replica[Set[Int]] & Inspection

  type Factory[R <: ReplicaUnderTest] = (Hash, PrivateIdentity, Replica[?] => AntiEntropy, Set[Int] => Unit) => R

  final class PlainReplica(
      genesisHash: Hash,
      identity: PrivateIdentity,
      provider: Replica[?] => AntiEntropy,
      onChange: Set[Int] => Unit
  ) extends Replica[Set[Int]](genesisHash, identity, provider, onChange) with Inspection {
    def graph: ArdtEventGraph[Set[Int]] = eventGraph
    private val recordedInvalidations   = mutable.Map.empty[Hash, Set[Int]]

    override protected def deltasInvalidatedBy(revocation: Hash)
        : Iterable[Int] = {
      val invalidated = super.deltasInvalidatedBy(revocation)
      recordedInvalidations.getOrElseUpdate(revocation, invalidated.toSet)
      invalidated
    }

    def invalidatedBy(revocation: Hash): Set[Int] =
      recordedInvalidations.getOrElse(
        revocation,
        super.deltasInvalidatedBy(revocation).toSet
      )
  }

  final class SnapshotReplica(
      genesisHash: Hash,
      identity: PrivateIdentity,
      provider: Replica[?] => AntiEntropy,
      onChange: Set[Int] => Unit
  ) extends SnapshotAwareReplica[Set[Int]](genesisHash, identity, provider, onChange) with Inspection {
    def graph: ArdtEventGraph[Set[Int]] = eventGraph
    private val recordedInvalidations   = mutable.Map.empty[Hash, Set[Int]]

    override protected def deltasInvalidatedBy(revocation: Hash)
        : Iterable[Int] = {
      val invalidated = super.deltasInvalidatedBy(revocation)
      recordedInvalidations.getOrElseUpdate(revocation, invalidated.toSet)
      invalidated
    }

    def invalidatedBy(revocation: Hash): Set[Int] =
      recordedInvalidations.getOrElse(
        revocation,
        super.deltasInvalidatedBy(revocation).toSet
      )

    /** The private `snapshotVersion` of [[SnapshotAwareReplica]], read by reflection; -1 means no snapshot */
    def currentSnapshotVersion: Int = {
      val field = classOf[SnapshotAwareReplica[?]].getDeclaredField("snapshotVersion")
      field.setAccessible(true)
      field.getInt(this)
    }
  }
}
