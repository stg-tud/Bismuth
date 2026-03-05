package lofi_acl.sync

import channels.{ArrayMessageBuffer, MessageBuffer}
import crypto.PublicIdentity
import crypto.channels.IdentityFactory
import lofi_acl.sync.ChannelConnectionManagerTest.{DEBUG, TIMEOUT_MS, buf, unbuf}
import munit.FunSuite

import java.util.concurrent.TimeUnit

object ChannelConnectionManagerTest {
  val DEBUG      = true
  val TIMEOUT_MS = 100

  def buf(str: String): MessageBuffer = ArrayMessageBuffer(str.getBytes)
  extension (tpl: (MessageBuffer, PublicIdentity))
      def unbuf: (String, PublicIdentity) = if tpl == null then null else tpl.copy(_1 = String(tpl._1.asArray))
}

class ChannelConnectionManagerTest extends FunSuite {
  private val idA = IdentityFactory.createNewIdentity
  private val idB = IdentityFactory.createNewIdentity
  private val idC = IdentityFactory.createNewIdentity
  // private val idD = IdentityFactory.createNewIdentity

  test("Two replicas") {
    val receiverA = QueueAppendingMessageReceiver()
    val receiverB = QueueAppendingMessageReceiver()
    val connManA  = ChannelConnectionManager(idA, receiverA, disableLogging = !DEBUG)
    val connManB  = ChannelConnectionManager(idB, receiverB, disableLogging = !DEBUG)
    connManA.acceptIncomingConnections()
    connManB.connectTo("localhost", connManA.listenPort.get) // B -> A

    // Duplicate connection attempts (A -> B)
    connManB.acceptIncomingConnections()
    connManA.connectTo("localhost", connManB.listenPort.get)

    assertEquals(receiverA.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idB.getPublic)
    assertEquals(receiverB.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS), idA.getPublic)

    connManA.send(idB.getPublic, buf("Hello"))
    assertEquals(receiverB.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("Hello", idA.getPublic))

    connManB.send(idA.getPublic, buf("World"))
    assertEquals(receiverA.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("World", idB.getPublic))

    connManA.shutdown()
    connManB.shutdown()
  }

  test("Three replicas with duplicates") {
    val receiverA = QueueAppendingMessageReceiver()
    val receiverB = QueueAppendingMessageReceiver()
    val receiverC = QueueAppendingMessageReceiver()

    val connManA = ChannelConnectionManager(idA, receiverA, disableLogging = !DEBUG)
    val connManB = ChannelConnectionManager(idB, receiverB, disableLogging = !DEBUG)
    val connManC = ChannelConnectionManager(idC, receiverC, disableLogging = !DEBUG)

    connManA.acceptIncomingConnections()
    connManB.acceptIncomingConnections()
    connManC.acceptIncomingConnections()

    connManA.connectTo("localhost", connManB.listenPort.get) // A -> B
    connManB.connectTo("localhost", connManA.listenPort.get) // B -> A
    connManC.connectTo("localhost", connManA.listenPort.get) // C -> A
    connManA.connectTo("localhost", connManC.listenPort.get) // A -> C
    connManB.connectTo("localhost", connManC.listenPort.get) // B -> C
    connManC.connectTo("localhost", connManB.listenPort.get) // C -> B

    assertEquals(
      Set(
        receiverA.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS),
        receiverA.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS)
      ),
      Set(idB.getPublic, idC.getPublic)
    )
    assertEquals(
      Set(
        receiverB.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS),
        receiverB.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS)
      ),
      Set(idA.getPublic, idC.getPublic)
    )
    assertEquals(
      Set(
        receiverC.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS),
        receiverC.connectionQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS)
      ),
      Set(idA.getPublic, idB.getPublic)
    )

    connManA.send(idB.getPublic, buf("Hello"))
    connManA.send(idC.getPublic, buf("Hello"))
    assertEquals(receiverB.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("Hello", idA.getPublic))
    assertEquals(receiverC.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("Hello", idA.getPublic))

    connManB.send(idA.getPublic, buf("World"))
    connManB.send(idC.getPublic, buf("World"))
    assertEquals(receiverA.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("World", idB.getPublic))
    assertEquals(receiverC.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("World", idB.getPublic))

    connManC.send(idA.getPublic, buf("!"))
    connManC.send(idB.getPublic, buf("!"))
    assertEquals(receiverA.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("!", idC.getPublic))
    assertEquals(receiverB.messageQueue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS).unbuf, ("!", idC.getPublic))

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
  }
}
