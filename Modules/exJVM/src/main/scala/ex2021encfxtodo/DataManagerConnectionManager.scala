package ex2021encfxtodo

import channels.{Abort, ChannelConnectInfo, ConcurrencyHelper, NioTCP, NioTcpConnectionDetailsResolver, PeerConnectInfo}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle, RegistryConfiguration}
import rdts.base.LocalUid
import replication.BroadcastIO
import replication.overlay.DirectConnectionOverlay
import replication.research.SignalingClient

import java.nio.file.{Files, Path}
import java.util.concurrent.ExecutorService
import scala.util.Try

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] =
    Try(aead.decrypt(data, associated))
}

class DataManagerConnectionManager[State: JsonValueCodec](
    replicaId: LocalUid,
    receiveCallback: State => Unit
) {
  import DataManagerConnectionManager.*

  private val aead        = loadOrCreateDemoAead()
  private val globalAbort = Abort()

  private val nio: NioTCP                              = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
  private val nioThread: ExecutorService               = java.util.concurrent.Executors.newSingleThreadExecutor()
  private val nioResolver                              = new NioTcpConnectionDetailsResolver(nio)
  private val (listenDetails, listener)                = nioResolver.listen("127.0.0.1", 0)
  private val selfInfo                                 = PeerConnectInfo(replicaId.uid, Set(listenDetails))
  private val signalingNio: NioTCP                     = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
  private val signalingThread: ExecutorService         = java.util.concurrent.Executors.newSingleThreadExecutor()
  private val signalingResolver                        = new NioTcpConnectionDetailsResolver(signalingNio)
  private var signalingClient: Option[SignalingClient] = None

  val dataManager: BroadcastIO[State] =
    BroadcastIO[State](
      replicaId = replicaId,
      receiveCallback = receiveCallback,
      overlay = Some(DirectConnectionOverlay(selfInfo)),
      resolver = nioResolver,
      globalAbort = globalAbort,
      aead = AeadTranslation(aead),
    )

  dataManager.addBinaryConnection(listener)
  nioThread.execute(() => nio.loopSelection(dataManager.globalAbort))
  signalingThread.execute(() => signalingNio.loopSelection(dataManager.globalAbort))

  def stateChanged(newState: State): Unit =
    dataManager.applyDelta(newState)

  def connectToSignalingServer(connectionString: String): Unit =
    parseSignalingServer(connectionString) match
        case Some(server) =>
          println(s"[$replicaId] connecting to signaling server $server, announcing $listenDetails on topic '$Topic'")
          signalingClient.foreach(_.stop())
          var clientRef: Option[SignalingClient] = None
          val client = SignalingClient(
            server = server,
            resolver = signalingResolver,
            localUid = replicaId.uid,
            initialAnnouncements = Map(Topic -> Set(listenDetails)),
            debug = true,
            onRegistered = () => {
              println(s"[$replicaId] signaling registered, looking up topic '$Topic'")
              clientRef.foreach(_.lookupTopic(Topic).run(res => println(s"[$replicaId] lookupTopic sent: $res")))
            },
            onPeerInfo = (uid, topics) =>
              println(s"[$replicaId] signaling peer info for $uid: $topics"),
            onTopicInfo = (topic, peers) => {
              println(s"[$replicaId] signaling topic info for '$topic': $peers")
              dataManager.discover(
                peers.iterator
                  .filterNot(_._1 == replicaId.uid)
                  .map((uid, infos) => PeerConnectInfo(uid, infos))
                  .toSet
              )
            },
            onDisconnected = () => println(s"[$replicaId] signaling disconnected")
          )
          clientRef = Some(client)
          signalingClient = clientRef
          client.start()
        case None =>
          Console.err.println(s"Invalid signaling server connection string: $connectionString")

  def stop(): Unit = {
    signalingClient.foreach(_.stop())
    signalingClient = None
    dataManager.globalAbort.closeRequest = true
    signalingNio.selector.wakeup()
    nio.selector.wakeup()
    signalingThread.shutdownNow()
    nioThread.shutdownNow()
    ()
  }

  def remoteAddresses: Set[String] =
    dataManager.overlayController match
        case overlay: DirectConnectionOverlay => overlay.active.keySet.map(_.delegate)
        case _                                => Set.empty
}

object DataManagerConnectionManager {
  private val Topic = "encfx"

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

  private def parseSignalingServer(connectionString: String): Option[ChannelConnectInfo.Tcp] =
    Try(java.net.URI.create(connectionString)).toOption.collect {
      case uri if Set("ws", "tcp").contains(uri.getScheme) && uri.getHost != null && uri.getPort >= 0 =>
        ChannelConnectInfo.Tcp(uri.getHost, uri.getPort)
    }
}
