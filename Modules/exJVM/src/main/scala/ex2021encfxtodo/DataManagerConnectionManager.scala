package ex2021encfxtodo

import channels.{ConcurrencyHelper, NioTCP}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle, LegacyKeysetSerialization, RegistryConfiguration}
import rdts.base.LocalUid
import replication.BroadcastIO

import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, Path}
import scala.util.{Random, Try}

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] =
    Try(aead.decrypt(data, associated))
}

class DataManagerConnectionManager[State: {JsonValueCodec}](
    replicaId: LocalUid,
    receiveCallback: State => Unit
) {

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

  val dataManager: BroadcastIO[State] =
    BroadcastIO[State](
      replicaId: LocalUid,
      receiveCallback,
      aead = AeadTranslation(aead),
    )

  val port: Int = Random.nextInt(10000) + 50000


  val niotcp: NioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

  dataManager.addBinaryConnection(niotcp.listen(
    niotcp.defaultServerSocketChannel(new InetSocketAddress("127.0.0.1", port))
  ))

  val localReplicaId: String = replicaId.toString

  def stateChanged(newState: State): Unit =
    dataManager.applyDelta(newState)

  def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    dataManager.addBinaryConnection(niotcp.connect(niotcp.defaultSocketChannel(new InetSocketAddress(
      uri.getHost,
      uri.getPort
    ))))
  }

  def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    ()
  }

  def uri: URI = URI.create(s"tcp://127.0.0.1:$port")

  def remoteAddresses: Set[String] = Set.empty
}
