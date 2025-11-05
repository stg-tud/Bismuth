package ex2024travel.lofi_acl.sync

import channels.{ArrayMessageBuffer, MessageBuffer}
import crypto.PublicIdentity
import crypto.channels.IdentityFactory
import ex2024travel.lofi_acl.sync.ChannelConnectionManagerTest.{DEBUG, TIMEOUT_MS, buf, unbuf}
import munit.FunSuite

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

object ChannelConnectionManagerTest {
  val DEBUG      = false
  val TIMEOUT_MS = 100

  def buf(str: String): MessageBuffer = ArrayMessageBuffer(str.getBytes)
  extension (tpl: (MessageBuffer, PublicIdentity))
     def unbuf: (String, PublicIdentity) = if tpl == null then null else tpl.copy(_1 = String(tpl._1.asArray))
}

class ChannelConnectionManagerTest extends FunSuite {
  private val idA = IdentityFactory.createNewIdentity
  private val idB = IdentityFactory.createNewIdentity
  // private val idC = IdentityFactory.createNewIdentity
  // private val idD = IdentityFactory.createNewIdentity

  test("Two replicas") {
    val receiverA = QueueAppendingMessageReceiver()
    val receiverB = QueueAppendingMessageReceiver()
    val connManA  =
      ChannelConnectionManager(idA.tlsKeyPem, idA.tlsCertPem, idA.getPublic, receiverA, disableLogging = !DEBUG)
    val connManB =
      ChannelConnectionManager(idB.tlsKeyPem, idB.tlsCertPem, idB.getPublic, receiverB, disableLogging = !DEBUG)
    connManA.acceptIncomingConnections()
    connManB.connectTo("localhost", connManA.listenPort.get) // B -> A

    assertEquals(receiverA.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idB.getPublic)
    assertEquals(receiverB.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idA.getPublic)

    connManA.send(idB.getPublic, buf("Hello"))
    assertEquals(receiverB.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("Hello", idA.getPublic))

    connManB.send(idB.getPublic, buf("World"))

    // Duplicate connections are accepted (A -> B)
    connManB.acceptIncomingConnections()
    connManA.connectTo("localhost", connManB.listenPort.get)
    assertEquals(receiverA.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idB.getPublic)
    assertEquals(receiverB.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idA.getPublic)

    connManA.shutdown()
    connManB.shutdown()
  }

  class QueueAppendingMessageReceiver(_id: PublicIdentity = null) extends MessageReceiver[MessageBuffer] {
    private val localId = Option(_id)

    val messageQueue: LinkedBlockingQueue[(MessageBuffer, PublicIdentity)] =
      LinkedBlockingQueue[(MessageBuffer, PublicIdentity)]()

    val connectionQueue: LinkedBlockingQueue[PublicIdentity] = LinkedBlockingQueue()

    override def receivedMessage(msg: MessageBuffer, fromUser: PublicIdentity): Unit =
      messageQueue.put((msg, fromUser))

    override def connectionEstablished(publicIdentity: PublicIdentity): Unit = {
      if DEBUG then println(s"${localId.map(_.id).getOrElse("Replica")} is now connected to ${publicIdentity.id}")
      connectionQueue.put(publicIdentity)
    }

    override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
      if false then println(s"${localId.map(_.id).getOrElse("Replica")} connectionShutdown to $publicIdentity")
  }
}
