package lofi_acl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.travelplanner.TravelPlan
import rdts.base.Uid
import rdts.filters.PermissionTree
import rdts.time.{Dot, Dots}

import java.net.InetAddress
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.security.KeyPair
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicLongArray, AtomicReference}
import scala.util.Random

class PingPongBenchmark(
    val bindHost: String,
    val listenPort: Int,
    val initialPeers: Seq[(String, Int)],
    val trace: Trace,
    val enforceAcl: Boolean
) {
  def runAsLeader(iterations: Int, expectedPeers: Int): Unit = {
    val remainingPongDots = AtomicReference[Dots](Dots.empty)
    val count             = AtomicInteger(0)
    val lastStartTime     = AtomicLong()
    val runtimes          = AtomicLongArray(iterations)

    inline def nextRound(replica: BenchmarkReplica): Unit =
        val delta = trace.deltas(0)(count.get())
        remainingPongDots.set(nextRemoteDots(replica))
        lastStartTime.set(System.nanoTime())
        replica.applyMutation(delta)

    def onReceive(dots: Dots, delta: TravelPlan, replica: BenchmarkReplica): Unit = {
      if remainingPongDots.updateAndGet(_.subtract(dots)).isEmpty then {
        val stopTime = System.nanoTime()
        runtimes.set(count.get(), stopTime - lastStartTime.get())
        println(s"[${count.get() + 1}/$iterations]: ${runtimes.get(count.get())}")
        if count.incrementAndGet() < runtimes.length then {
          Thread.ofVirtual().start { () =>
            Thread.sleep(10) // Other replicas might still be processing messages
            nextRound(replica)
          }: Unit
        } else {
          printResults(runtimes)
          replica.stop()
        }
      }
    }

    val replica = BenchmarkReplica(
      InetAddress.getByName(bindHost),
      trace.ids(0),
      trace.genesis,
      enforceAcl,
      onReceive,
      listenPort
    )
    replica.start()
    replica.sync.delegatePermission(trace.additionalPermissions)

    Thread.sleep(100)
    initialPeers.foreach((host, port) => replica.sync.connect(host, port))
    while replica.sync.connectedPeers.size != expectedPeers
    do {
      println("Waiting")
      Thread.sleep(100)
    }
    println("Waiting one second")
    Thread.sleep(1_000)
    require(replica.sync.connectedPeers.size == expectedPeers)

    println("Starting with benchmark")
    nextRound(replica)
  }

  def runAsFollower(replicaIndex: Int, iterations: Int): Unit = {
    require(replicaIndex > 0 && replicaIndex < trace.ids.length)
    val leaderUid            = Uid(trace.ids(0).getPublic.id)
    val count: AtomicInteger = AtomicInteger(0)
    val deltas               = trace.deltas(replicaIndex)
    val nextPingDot          = AtomicReference(Dot(leaderUid, 0))
    val done                 = Semaphore(0) // Signals completion of benchmark

    def onReceive(dots: Dots, delta: TravelPlan, replica: BenchmarkReplica): Unit =
      if dots.contains(nextPingDot.get()) then {
        replica.applyMutation(deltas(count.getAndIncrement()))
        nextPingDot.set(replica.sync.stateVersion.nextDot(leaderUid))
        if count.get() == iterations then {
          Thread.ofVirtual().start(() =>
              Thread.sleep(100)
              replica.stop()
              done.release()
          ): Unit
        }
      }

    val replica = BenchmarkReplica(
      InetAddress.getByName(bindHost),
      trace.ids(replicaIndex),
      trace.genesis,
      enforceAcl,
      onReceive,
      listenPort
    )
    replica.start()

    Thread.sleep(100)
    initialPeers.foreach((host, port) => replica.sync.connect(host, port))
    done.acquire()
  }

  def runAsRelay(relayId: PrivateIdentity, iterations: Int): Unit = {
    val lastDot = Dot(Uid(trace.ids(0).getPublic.id), iterations - 1)
    val done    = Semaphore(0) // Signals completion of benchmark

    def onReceive(dots: Dots, replica: BenchmarkRelayReplica): Unit =
      if dots.contains(lastDot) then
          Thread.ofVirtual.start { () =>
            Thread.sleep(1_000)
            replica.stop()
            done.release()
          }: Unit

    val replica =
      BenchmarkRelayReplica(InetAddress.getByName(bindHost), relayId, trace.genesis, enforceAcl, listenPort, onReceive)
    replica.start()

    Thread.sleep(100)

    done.acquire()
  }

  def nextRemoteDots(replica: BenchmarkReplica): Dots = {
    val stateVersion = replica.sync.stateVersion
    Dots.from(
      trace.ids
        .map(_.getPublic.id).map(Uid(_))
        .map { uid => stateVersion.nextDot(uid) }
    )
  }

  def printResults(measurements: AtomicLongArray): Unit = {
    for i <- 0 until measurements.length() do
        println(measurements.get(i))
  }
}

object CreateAndSaveTrace {
  private val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  private val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100

  val numReplicas          = 10
  val numDeltasPerReplicas = 11_000

  def main(args: Array[String]): Unit = {
    val relayId = IdentityFactory.createNewIdentity
    val trace   = {
      val trace = TraceGeneration.generateTrace(
        numReplicas,
        numDeltasPerReplicas,
        MIN_ENTRIES_PER_MAP_PER_REPLICA,
        MAX_ENTRIES_PER_MAP_PER_REPLICA
      )(using Random(42))
      trace.copy(
        additionalPermissions =
          trace.additionalPermissions.merge(Acl(read = Map(relayId.getPublic -> PermissionTree.allow)))
      )
    }
    val savedTrace = SavedTrace(
      trace.ids.map(_.identityKey),
      relayId.identityKey,
      trace.genesis,
      trace.additionalPermissions,
      trace.deltas
    )
    val filePath =
      if args.length == 1
      then Paths.get(args(0))
      else Paths.get("ping-pong-trace.json")
    Files.write(filePath, writeToArray(savedTrace), StandardOpenOption.CREATE_NEW): Unit
  }

  def load(path: Path): SavedTrace = readFromStream(Files.newInputStream(path, StandardOpenOption.READ))

  case class SavedTrace(
      identities: Array[KeyPair],
      relayIdentity: KeyPair,
      aclGenesis: BftDelta[Acl],
      additionalPermissions: Acl,
      deltas: Array[Array[TravelPlan]],
  ):
      lazy val relayPrivateIdentity: PrivateIdentity     = IdentityFactory.fromIdentityKey(relayIdentity)
      lazy val privateIdentities: Array[PrivateIdentity] = identities.map(IdentityFactory.fromIdentityKey)

  given JsonValueCodec[KeyPair] = new JsonValueCodec[KeyPair] {
    override def decodeValue(in: JsonReader, default: KeyPair): KeyPair = {
      val bytes = in.readBase64AsBytes(null)
      Ed25519Util.rawPrivateKeyBytesToKeyPair(bytes)
    }

    override def encodeValue(x: KeyPair, out: JsonWriter): Unit =
      out.writeBase64Val(Ed25519Util.privateKeyToRawPrivateKeyBytes(x.getPrivate), true)

    override def nullValue: KeyPair = null
  }

  import lofi_acl.JsoniterCodecs.given
  given JsonValueCodec[SavedTrace] = JsonCodecMaker.make
}
