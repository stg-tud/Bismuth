package lofi_acl.evaluation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.Ed25519Util
import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.travelplanner.TravelPlan
import rdts.filters.PermissionTree

import java.net.InetAddress
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.security.KeyPair
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

class PingPongBenchmark(
    val bindHost: String,
    val listenPort: Int,
    val initialPeers: Option[Seq[(String, Int)]],
    val expectedPeers: Int,
    val trace: Trace,
    val enforceAcl: Boolean
) {
  def runAsLeader(replicaIndex: Int, warmupIterations: Int, iterations: Int): Unit = {
    val count: AtomicInteger = AtomicInteger(0)
    val replica              = BenchmarkReplica(
      InetAddress.getByName(bindHost),
      trace.ids(replicaIndex),
      trace.genesis,
      enforceAcl,
      (_, _, _) => ???, // TODO: Add response handler
      listenPort
    )

    // TODO: add harness
  }

  def runAsFollower(replicaIndex: Int, leaderIndex: Int): Unit = {
    val leaderId             = trace.ids(leaderIndex).getPublic
    val count: AtomicInteger = AtomicInteger(0)
    val replica              = BenchmarkReplica(
      InetAddress.getByName(bindHost),
      trace.ids(replicaIndex),
      trace.genesis,
      enforceAcl,
      (_, _, _) => ???, // TODO: Add response handler
      listenPort
    )
  }

  def runAsRelay(relayId: PrivateIdentity): Unit = {
    val replica = BenchmarkRelayReplica(InetAddress.getByName(bindHost), relayId, trace.genesis, enforceAcl, listenPort)
    replica.start()
    initialPeers.foreach(_.foreach((host, port) => replica.sync.connect(host, port)))
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
