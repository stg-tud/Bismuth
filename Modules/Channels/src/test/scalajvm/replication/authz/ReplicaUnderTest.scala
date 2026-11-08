package replication.authz

import channels.crypto.{PrivateIdentity, PublicIdentity}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import rdts.filters.PermissionTree
import replication.JsoniterCodecsJvm.ardtEventCodec
import replication.authz.AuthzTestSupport.given
import replication.crypto.Commitment.RevealedValue
import replication.crypto.Hash

import scala.collection.mutable

/** Common interface of the legacy [[Replica]] (and [[SnapshotAwareReplica]]) and the pure [[AuthzReplica]], so the same
  * tests pin down the behaviour of both. Local operations record what they disseminate, remote input is given directly.
  */
trait ReplicaUnderTest {
  def localReplicaId: PublicIdentity
  def state: Set[Int]
  def heads: Set[Hash]
  def graph: ArdtEventGraph[Set[Int]]
  def containsEvent(eventHash: Hash): Boolean
  def delta(eventHash: Hash): Option[Set[Int]]
  def deltaValue(eventHash: Hash): Option[RevealedValue]

  /** @throws IllegalArgumentException if the event is invalid */
  def receiveEvent(encodedEvent: Array[Byte]): Either[Set[Hash], Option[Hash]]

  /** @throws IllegalArgumentException if the delta is not acceptable */
  def receiveDelta(eventHash: Hash, value: RevealedValue): Unit

  def mutateState(mutator: Set[Int] => Set[Int]): Unit
  def mutateState(mutator: Set[Int] => Set[Int], capability: Hash): Unit
  def createDelegation(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree)
      : Unit
  def createRevocation(revokedCapability: Hash): Unit

  /** The events of `deltas` whose values `reader` may read. */
  def readableBy(reader: PublicIdentity, deltas: Iterable[(Hash, RevealedValue)]): Set[Hash]

  def createSnapshot(): Unit

  /** The event index the snapshot was taken at, -1 if there is none. */
  def snapshotVersion: Int

  /** The event indices of the deltas the replica invalidated when receiving `revocation`, or, for a revocation that did
    * not invalidate anything when received, those it would invalidate now.
    */
  def invalidatedBy(revocation: Hash): Set[Int]

  /** The states the application was notified about. */
  val notifications: mutable.Buffer[Set[Int]] = mutable.Buffer.empty

  /** Events and deltas disseminated by local operations, in order. */
  val broadcastedEvents: mutable.Buffer[Array[Byte]]                             = mutable.Buffer.empty
  val broadcastedDeltas: mutable.Buffer[(eventHash: Hash, delta: RevealedValue)] = mutable.Buffer.empty
}

object ReplicaUnderTest {

  type Factory = (genesis: Hash, identity: PrivateIdentity) => ReplicaUnderTest

  /** The implementations under test. All but the snapshot specific tests run against each. */
  val variants: Seq[(String, Factory)] = Seq(
    "Replica"                               -> ((g, i) => Legacy(g, i, snapshotAware = false)),
    "SnapshotAwareReplica without snapshot" -> ((g, i) => Legacy(g, i, snapshotAware = true)),
    "AuthzReplica"                          -> ((g, i) => Pure(g, i)),
  )

  /** Implementations supporting snapshots. */
  val snapshotVariants: Seq[(String, Factory)] = Seq(
    "SnapshotAwareReplica" -> ((g, i) => Legacy(g, i, snapshotAware = true)),
    "AuthzReplica"         -> ((g, i) => Pure(g, i)),
  )

  // ------------------------------------------------------------------------------------------------
  // legacy
  // ------------------------------------------------------------------------------------------------

  /** Records what the legacy replica asks anti-entropy to disseminate. */
  class MockAntiEntropy(replica: Replica[?]) extends AntiEntropy(replica, _ => ???, _ => ???) {
    var recordEvents: Iterable[Array[Byte]] => Unit                             = _ => ()
    var recordDeltas: Iterable[(eventHash: Hash, delta: RevealedValue)] => Unit = _ => ()

    override def start(): Unit                        = ()
    override def listenAddress: Option[(String, Int)] = None

    override def broadcastEvents(events: Iterable[Array[Byte]]): Unit = recordEvents(events)

    override def broadcastDeltasFiltered(deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]): Unit =
      recordDeltas(deltas)
  }

  /** Access to the internals of the legacy replicas. */
  private trait Inspection { self: Replica[Set[Int]] =>
    def graph: ArdtEventGraph[Set[Int]] = eventGraph
    def store: DeltaValueStore[Set[Int]] = deltaValueStore
    val recordedInvalidations: mutable.Map[Hash, Set[Int]] = mutable.Map.empty
    def deltasInvalidatedNow(revocation: Hash): Set[Int]
  }

  final class Legacy(genesis: Hash, identity: PrivateIdentity, snapshotAware: Boolean) extends ReplicaUnderTest {
    private var mock: MockAntiEntropy | Null = null
    private val provider: Replica[?] => AntiEntropy = r => {
      val m = MockAntiEntropy(r)
      m.recordEvents = broadcastedEvents ++= _
      m.recordDeltas = broadcastedDeltas ++= _
      mock = m
      m
    }
    private val onChange: Set[Int] => Unit = notifications += _

    private val replica: Replica[Set[Int]] & Inspection =
      if snapshotAware then
          new SnapshotAwareReplica[Set[Int]](genesis, identity, provider, onChange) with Inspection {
            override protected def deltasInvalidatedBy(revocation: Hash): Iterable[Int] = {
              val invalidated = super.deltasInvalidatedBy(revocation)
              recordedInvalidations.getOrElseUpdate(revocation, invalidated.toSet)
              invalidated
            }
            def deltasInvalidatedNow(revocation: Hash): Set[Int] = super.deltasInvalidatedBy(revocation).toSet
          }
      else
          new Replica[Set[Int]](genesis, identity, provider, onChange) with Inspection {
            override protected def deltasInvalidatedBy(revocation: Hash): Iterable[Int] = {
              val invalidated = super.deltasInvalidatedBy(revocation)
              recordedInvalidations.getOrElseUpdate(revocation, invalidated.toSet)
              invalidated
            }
            def deltasInvalidatedNow(revocation: Hash): Set[Int] = super.deltasInvalidatedBy(revocation).toSet
          }
    replica.start()

    def localReplicaId: PublicIdentity                      = replica.localReplicaId
    def state: Set[Int]                                     = replica.state
    def heads: Set[Hash]                                    = replica.heads
    def graph: ArdtEventGraph[Set[Int]]                     = replica.graph
    def containsEvent(eventHash: Hash): Boolean             = replica.containsEvent(eventHash)
    def delta(eventHash: Hash): Option[Set[Int]]            = replica.delta(eventHash)
    def deltaValue(eventHash: Hash): Option[RevealedValue]  = replica.deltaValue(eventHash)
    def receiveEvent(encoded: Array[Byte]): Either[Set[Hash], Option[Hash]] = replica.receiveEvent(encoded)
    def receiveDelta(eventHash: Hash, value: RevealedValue): Unit          = replica.receiveDelta(eventHash, value)
    def mutateState(mutator: Set[Int] => Set[Int]): Unit                    = replica.mutateState(mutator)
    def mutateState(mutator: Set[Int] => Set[Int], capability: Hash): Unit =
      replica.mutateState(mutator, capability)
    def createDelegation(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree)
        : Unit = replica.createDelegation(usedCapability, delegatee, read, write)
    def createRevocation(revokedCapability: Hash): Unit = replica.createRevocation(revokedCapability)
    def readableBy(reader: PublicIdentity, deltas: Iterable[(Hash, RevealedValue)]): Set[Hash] =
      replica.filterDeltas(reader, deltas.map((h, v) => (eventHash = h, delta = v))).map(_.eventHash).toSet

    def createSnapshot(): Unit = replica match
        case snapshotting: SnapshotAwareReplica[?] => snapshotting.createSnapshot()
        case _                                     => throw UnsupportedOperationException("no snapshots")

    def snapshotVersion: Int = replica match
        case snapshotting: SnapshotAwareReplica[?] =>
          val field = classOf[SnapshotAwareReplica[?]].getDeclaredField("snapshotVersion")
          field.setAccessible(true)
          field.getInt(snapshotting)
        case _ => -1

    def invalidatedBy(revocation: Hash): Set[Int] =
      replica.recordedInvalidations.getOrElse(revocation, replica.deltasInvalidatedNow(revocation))
  }

  // ------------------------------------------------------------------------------------------------
  // pure
  // ------------------------------------------------------------------------------------------------

  final class Pure(genesis: Hash, identity: PrivateIdentity) extends ReplicaUnderTest {
    var replica: AuthzReplica[Set[Int]]                     = AuthzReplica.initial[Set[Int]](identity.getPublic, genesis)
    private val recordedInvalidations: mutable.Map[Hash, Set[Int]] = mutable.Map.empty

    /** Applies a remote input, notifying if the stored deltas, and thus the state, changed. */
    private def remote(next: AuthzReplica[Set[Int]]): Unit = {
      val changed = next.deltas ne replica.deltas
      replica = next
      if changed then notifications += next.state: Unit
    }

    /** Applies a local operation, recording the events (and deltas) it created. */
    private def local(f: AuthzReplica[Set[Int]] => AuthzReplica[Set[Int]]): Unit = {
      val before = replica
      replica = f(before)
      val created = replica.eventGraph.events.toList
        .collect { case (hash, (event, index)) if index >= before.eventGraph.nextEventIndex => (index, hash, event) }
        .sortBy(_._1)
      broadcastedEvents ++= created.map((_, _, event) => writeToArray(event))
      broadcastedDeltas ++= created.flatMap((_, hash, _) =>
        replica.deltaValue(hash).map(value => (eventHash = hash, delta = value))
      )
    }

    def localReplicaId: PublicIdentity                     = replica.self
    def state: Set[Int]                                    = replica.state
    def heads: Set[Hash]                                   = replica.heads
    def graph: ArdtEventGraph[Set[Int]]                    = replica.eventGraph
    def containsEvent(eventHash: Hash): Boolean            = replica.containsEvent(eventHash)
    def delta(eventHash: Hash): Option[Set[Int]]           = replica.delta(eventHash)
    def deltaValue(eventHash: Hash): Option[RevealedValue] = replica.deltaValue(eventHash)

    def receiveEvent(encoded: Array[Byte]): Either[Set[Hash], Option[Hash]] = {
      val before         = replica
      val (next, result) = before.receiveEvent(encoded)
      result.foreach(_.foreach { hash =>
        next.eventGraph.events(hash)._1.payload match
            case ArdtEvent.Payload.Revocation(_) if next.deltas ne before.deltas =>
              recordedInvalidations(hash) = before.deltas.keySet.diff(next.deltas.keySet)
            case _ =>
      })
      remote(next)
      result
    }

    def receiveDelta(eventHash: Hash, value: RevealedValue): Unit = remote(replica.receiveDelta(eventHash, value))

    def mutateState(mutator: Set[Int] => Set[Int]): Unit = local(r => r.write(identity, mutator(r.state)).state)
    def mutateState(mutator: Set[Int] => Set[Int], capability: Hash): Unit =
      local(r => r.write(identity, mutator(r.state), capability).state)
    def createDelegation(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree)
        : Unit = local(_.delegate(identity, usedCapability, delegatee, read, write).state)
    def createRevocation(revokedCapability: Hash): Unit = local(_.revoke(identity, revokedCapability).state)

    def readableBy(reader: PublicIdentity, deltas: Iterable[(Hash, RevealedValue)]): Set[Hash] =
      replica.readableDeltas(reader, deltas.map(_._1)).map(_.event).toSet

    def createSnapshot(): Unit = replica = replica.createSnapshot
    def snapshotVersion: Int   = replica.snapshot.fold(-1)(_.version)

    def invalidatedBy(revocation: Hash): Set[Int] =
      recordedInvalidations.getOrElse(
        revocation,
        replica.eventGraph.deltasInvalidatedBy(revocation).filter(replica.deltas.contains).toSet
      )
  }
}
