package ex2021encfxtodo

import channels.{Abort, ConnectionDescriptor, NioTCP, NioTcpConnectionDetailsResolver, PeerConnectInfo}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle, RegistryConfiguration}
import rdts.base.LocalUid
import replication.BroadcastIO
import replication.overlay.FullMeshOverlay

import java.nio.file.{Files, Path}
import java.util.concurrent.ExecutorService
import scala.util.Try

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] =
    Try(aead.decrypt(data, associated))
}

class ConnectionManager[State: JsonValueCodec](
    replicaId: LocalUid,
    receiveCallback: State => Unit
) {
  import ConnectionManager.*

  private val aead        = loadOrCreateDemoAead()
  private val globalAbort = Abort()

  private val nio: NioTCP                              = new NioTCP()
  private val nioThread: ExecutorService               = java.util.concurrent.Executors.newSingleThreadExecutor()
  private val nioResolver                              = new NioTcpConnectionDetailsResolver(nio)
  private val (listenDescriptor, listener)             = nioResolver.listen("127.0.0.1", 0)

  val dataManager: BroadcastIO[State] =
    BroadcastIO[State](
      replicaId = replicaId,
      receiveCallback = receiveCallback,
      overlay = Some(FullMeshOverlay(PeerConnectInfo(replicaId.uid, Set(listenDescriptor)))),
      resolver = nioResolver,
      globalAbort = globalAbort,
      aead = AeadTranslation(aead),
    )

  dataManager.addServerConnection(listener)
  nioThread.execute(() => nio.loopSelection(dataManager.globalAbort))

  def stateChanged(newState: State): Unit =
    dataManager.applyDelta(newState)

  def connectionString: String =
    dataManager.selfConnectionDescriptors.headOption.getOrElse(listenDescriptor).toString

  def connectTo(connectionString: String): Unit =
    ConnectionDescriptor.parse(connectionString) match
        case Some(descriptor) =>
          dataManager.bootstrapVia(descriptor)
        case _ =>
          Console.err.println(s"Invalid bootstrap connection string: $connectionString")

  def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    nio.selector.wakeup()
    nioThread.shutdownNow()
    ()
  }

  def remoteAddresses: Set[String] =
    dataManager.overlayController match
        case overlay: FullMeshOverlay => overlay.active.keySet.map(_.delegate)
        case _                                => Set.empty
}

object ConnectionManager {
  private def loadOrCreateDemoAead(): Aead = {
    AeadConfig.register()
    val keysetFilePath: Path = Path.of("demokey.json")
    if !Files.exists(keysetFilePath) then {
      val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
      CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withOutputStream(Files.newOutputStream(keysetFilePath)))
    }

    val keyset =
      CleartextKeysetHandle.read(JsonKeysetReader.withInputStream(Files.newInputStream(keysetFilePath)))
    keyset.getPrimitive(RegistryConfiguration.get(), classOf[Aead])
  }

}
