package ex2021encfxtodo.sync

import channels.NioTCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle, LegacyKeysetSerialization, RegistryConfiguration}
import rdts.base.LocalUid
import replication.DeltaDissemination

import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, Path}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Random, Try}

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] =
    Try(aead.decrypt(data, associated))
}

class DataManagerConnectionManager[State: {JsonValueCodec}](
    replicaId: LocalUid,
    receiveCallback: State => Unit
) extends ConnectionManager[State] {

  AeadConfig.register()
  private val keysetFilePath: Path = Path.of("demokey.json")
  if !Files.exists(keysetFilePath) then {
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withOutputStream(Files.newOutputStream(keysetFilePath)))
  }

  private val keyset =
    CleartextKeysetHandle.read(JsonKeysetReader.withInputStream(Files.newInputStream(keysetFilePath)))
  private val aead: Aead = keyset.getPrimitive(RegistryConfiguration.get(), classOf[Aead])

  println(LegacyKeysetSerialization.getKeysetInfo(keyset))

  val dataManager: DeltaDissemination[State] =
    DeltaDissemination[State](
      replicaId: LocalUid,
      receiveCallback,
      crypto = Some(AeadTranslation(aead))
    )

  val port = Random.nextInt(10000) + 50000

  val executor: ExecutorService = Executors.newCachedThreadPool((r: Runnable) =>
    Executors.defaultThreadFactory().newThread(r).tap(_.setDaemon(true))
  )
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  val niotcp = new NioTCP {}

  dataManager.addBinaryConnection(niotcp.listen(
    niotcp.defaultServerSocketChannel(new InetSocketAddress("127.0.0.1", port))
  ))

  override val localReplicaId: String = replicaId.toString

  override def stateChanged(newState: State): Unit = {
    dataManager.applyDelta(newState)
  }

  override def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    dataManager.addBinaryConnection(niotcp.connect(niotcp.defaultSocketChannel(new InetSocketAddress(
      uri.getHost,
      uri.getPort
    ))))
  }

  override def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    executor.shutdownNow()
    ()
  }

  override def uri: URI = URI.create(s"tcp://127.0.0.1:$port")

  override def remoteAddresses: Set[String] = Set.empty
}
