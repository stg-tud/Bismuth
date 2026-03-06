package lofi_acl.sync

import channels.{ArrayMessageBuffer, MessageBuffer}
import crypto.PublicIdentity
import crypto.channels.IdentityFactory
import lofi_acl.sync.ChannelConnectionManagerTest.*
import munit.FunSuite

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

object ChannelConnectionManagerTest {
  val DEBUG      = false
  val TIMEOUT_MS = 100

  def buf(str: String): MessageBuffer = ArrayMessageBuffer(str.getBytes)
  extension (tpl: (MessageBuffer, PublicIdentity))
      def unbuf: (String, PublicIdentity) = if tpl == null then null else tpl.copy(_1 = String(tpl._1.asArray))

  def waitForUnordered[T](queue: LinkedBlockingQueue[T], expected: Set[T]): Set[T] = {
    var accumulated = Set.empty[T]
    while {
      val next = queue.poll(TIMEOUT_MS, TimeUnit.MILLISECONDS)
      if next == null then return accumulated

      accumulated = accumulated + next
      expected != accumulated
    } do {}
    accumulated
  }
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
    connManB.connectTo(connManA.listenAddress.get)

    // Duplicate connection attempts (A -> B)
    connManB.acceptIncomingConnections()
    connManA.connectTo(connManB.listenAddress.get)

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

    connManA.connectTo(connManB.listenAddress.get) // A -> B
    connManB.connectTo(connManA.listenAddress.get) // B -> A
    connManC.connectTo(connManA.listenAddress.get) // C -> A
    connManA.connectTo(connManC.listenAddress.get) // A -> C
    connManB.connectTo(connManC.listenAddress.get) // B -> C
    connManC.connectTo(connManB.listenAddress.get) // C -> B

    assertEquals(
      waitForUnordered(receiverA.connectionQueue, Set(idB.getPublic, idC.getPublic)),
      Set(idB.getPublic, idC.getPublic)
    )
    assertEquals(
      waitForUnordered(receiverB.connectionQueue, Set(idA.getPublic, idC.getPublic)),
      Set(idA.getPublic, idC.getPublic)
    )
    assertEquals(
      waitForUnordered(receiverC.connectionQueue, Set(idA.getPublic, idB.getPublic)),
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
