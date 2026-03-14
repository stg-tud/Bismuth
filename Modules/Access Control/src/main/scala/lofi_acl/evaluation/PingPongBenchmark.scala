package lofi_acl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.travelplanner.TravelPlan
import rdts.base.Uid
import rdts.filters.PermissionTree
import rdts.time.{Dot, Dots}

import java.net.InetAddress
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.security.KeyPair
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicLongArray, AtomicReference}
import scala.util.Random

class PingPongBenchmark(
    val bindHost: String,
    val listenPort: Int,
    val initialPeers: Option[Seq[(String, Int)]],
    val expectedPeers: Int,
    val trace: Trace,
    val enforceAcl: Boolean
) {
  def runAsLeader(replicaIndex: Int, iterations: Int): Unit = {
    val remainingPongDots = AtomicReference[Dots](Dots.empty)
    val count             = AtomicInteger()
    val lastStartTime     = AtomicLong()
    val runtimes          = AtomicLongArray(iterations)

    def onReceive(dots: Dots, delta: TravelPlan, replica: BenchmarkReplica): Unit = {
      if remainingPongDots.updateAndGet(_.subtract(dots)).isEmpty then {
        val stopTime = System.nanoTime()
        runtimes.set(count.get(), stopTime - lastStartTime.get())
        println(s"[${count.get()}/$iterations]: ${runtimes.get(count.get())}")
        if count.incrementAndGet() < runtimes.length then {
          Thread.ofVirtual().start { () =>
            Thread.sleep(10) // Other replicas might still be processing messages
            val delta = trace.deltas(replicaIndex)(count.get())
            remainingPongDots.set(nextRemoteDots(replica))
            lastStartTime.set(System.nanoTime())
            replica.applyMutation(delta)
          }: Unit
        } else {
          printResults(runtimes)
          replica.stop()
        }
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
  }

  def runAsFollower(replicaIndex: Int, leaderIndex: Int, iterations: Int): Unit = {
    val leaderUid            = Uid(trace.ids(leaderIndex).getPublic.id)
    val count: AtomicInteger = AtomicInteger(0)
    val deltas               = trace.deltas(replicaIndex)
    val nextPingDot          = AtomicReference(Dot(leaderUid, 0))

    def onReceive(dots: Dots, delta: TravelPlan, replica: BenchmarkReplica): Unit =
      if dots.contains(nextPingDot.get()) then {
        replica.applyMutation(deltas(count.getAndIncrement()))
        if nextPingDot.updateAndGet(_.advance).time == iterations then {
          Thread.ofVirtual().start(() =>
              Thread.sleep(100)
              replica.stop()
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
  }

  def runAsRelay(relayId: PrivateIdentity): Unit = {
    val replica = BenchmarkRelayReplica(InetAddress.getByName(bindHost), relayId, trace.genesis, enforceAcl, listenPort)
    replica.start()
    initialPeers.foreach(_.foreach((host, port) => replica.sync.connect(host, port)))
  }

  def nextRemoteDots(replica: BenchmarkReplica): Dots = Dots.from(
    replica.sync.stateVersion.internal
      .filterNot(_._1.delegate == replica.identity.getPublic.id)
      .map((uid, ranges) =>
        Dot(uid, ranges.next.getOrElse(0.longValue))
      )
  )

  def printResults(measurements: AtomicLongArray): Unit = {
    for i <- 0 until measurements.length() do
        println(measurements.get(0))
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
    Files.write(Paths.get("ping-pong-trace.json"), writeToArray(savedTrace), StandardOpenOption.CREATE_NEW): Unit
  }

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
