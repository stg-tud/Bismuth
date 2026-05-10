package channels

import channels.TestUtil.printErrors
import de.rmgk.delay.Callback

import java.util.concurrent.{ExecutorService, Executors, Semaphore, TimeUnit}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait EchoCommunicationTest[CD <: ConnectionDescriptor](
    serverConn: ExecutionContext => LatentConnection[CD],
    clientConn: ExecutionContext => CD => LatentConnection[Connection]
) extends munit.FunSuite {

  def supportsMultipleConnections: Boolean    = true
  def supportsDisconnectDetection: Boolean    = true
  def supportsStableConnectionObject: Boolean = true

  val executor: ExecutorService = Executors.newCachedThreadPool()
  val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)

  override def afterAll(): Unit =
    executor.shutdownNow()

  private def await(sem: Semaphore, permits: Int = 1, clue: => Any): Unit =
    assert(sem.tryAcquire(permits, 5, TimeUnit.SECONDS), clue)

  private def sameRef(a: AnyRef, b: AnyRef): Boolean = a eq b

  private def asReceive(receiver: Connection => Callback[MessageBuffer]): Receive = new Receive {
    override def connectionEstablished(answers: Connection): Callback[MessageBuffer] = receiver(answers)
  }

  private def startServer(receiver: Connection => Callback[MessageBuffer])(using Abort): CD = {
    val started = Semaphore(0)
    var descriptor: CD | Null     = null
    var failure: Throwable | Null = null

    serverConn(ec).prepare(asReceive(receiver)).runIn(summon) {
      case Success(value) =>
        descriptor = value
        started.release()
      case Failure(ex) =>
        failure = ex
        started.release()
    }

    await(started, clue = failure)
    if failure != null then throw failure.nn
    descriptor.nn
  }

  private def connectClient(descriptor: CD, receiver: Connection => Callback[MessageBuffer])(using Abort): Connection = {
    val started = Semaphore(0)
    var connection: Connection | Null = null
    var failure: Throwable | Null     = null

    clientConn(ec)(descriptor).prepare(asReceive(receiver)).runIn(summon) {
      case Success(value) =>
        connection = value
        started.release()
      case Failure(ex) =>
        failure = ex
        started.release()
    }

    await(started, clue = failure)
    if failure != null then throw failure.nn
    connection.nn
  }

  test("sample communication") {
    given abort: Abort = Abort()

    val toSend                 = List("Hi", "ho", "let’s", "go")
    val messageCounter         = Semaphore(0)
    val received               = mutable.ListBuffer.empty[String]

    val descriptor = startServer { conn =>
      printErrors { mb =>
        conn.send(mb).runToFuture(())
      }
    }

    val client = connectClient(descriptor, _ =>
      printErrors { mb =>
        received.synchronized(received += String(mb.asArray))
        messageCounter.release()
      }
    )

    toSend.foreach { msg =>
      client.send(ArrayMessageBuffer(msg.getBytes())).run(printErrors(_ => ()))
    }

    await(messageCounter, toSend.size, received)
    assertEquals(received.synchronized(received.toList).sorted, toSend.sorted)
  }

  test("multiple connections to the server at the same time work") {
    if supportsMultipleConnections then {

    given abort: Abort = Abort()

    val serverConnections = mutable.Set.empty[Connection]
    val clientReceived    = mutable.ListBuffer.empty[String]
    val receivedCounter   = Semaphore(0)

    val descriptor = startServer { conn =>
      serverConnections.synchronized(serverConnections += conn)
      printErrors { mb =>
        conn.send(mb).runToFuture(())
      }
    }

    val clientA = connectClient(descriptor, _ =>
      printErrors { mb =>
        clientReceived.synchronized(clientReceived += String(mb.asArray))
        receivedCounter.release()
      }
    )
    val clientB = connectClient(descriptor, _ =>
      printErrors { mb =>
        clientReceived.synchronized(clientReceived += String(mb.asArray))
        receivedCounter.release()
      }
    )

    ec.execute(() => clientA.send(ArrayMessageBuffer("client-a".getBytes)).run(_ => ()))
    ec.execute(() => clientB.send(ArrayMessageBuffer("client-b".getBytes)).run(_ => ()))

    await(receivedCounter, 2, clientReceived)
    assertEquals(clientReceived.toSet, Set("client-a", "client-b"))
    assertEquals(serverConnections.synchronized(serverConnections.size), 2)
    }
  }

  test("disconnects are detected on the other side") {
    if supportsDisconnectDetection then {
      given abort: Abort = Abort()

      val serverMessages = Semaphore(0)
      val serverFailures = Semaphore(0)
      val failedOnServer = mutable.ListBuffer.empty[Connection]

      val descriptor = startServer { conn =>
        new Callback[MessageBuffer] {
          override def complete(result: Try[MessageBuffer]): Unit = result match {
            case Success(_) =>
              serverMessages.release()
            case Failure(_: NoMoreDataException) =>
              failedOnServer.synchronized(failedOnServer += conn)
              serverFailures.release()
            case Failure(_) =>
              failedOnServer.synchronized(failedOnServer += conn)
              serverFailures.release()
          }
        }
      }

      val client = connectClient(descriptor, _ => printErrors(_ => ()))
      client.send(ArrayMessageBuffer("hello".getBytes)).run(_ => ())

      await(serverMessages, clue = "server did not observe initial message")
      client.close()
      await(serverFailures, clue = "server did not observe disconnect")
      assertEquals(failedOnServer.synchronized(failedOnServer.size), 1)
    }
  }

  test("multiple connections and then disconnects detect the correct connection disconnecting") {
    if supportsDisconnectDetection then {
      given abort: Abort = Abort()

      val firstMessageByConnection = mutable.Map.empty[Connection, String]
      val serverMessages           = Semaphore(0)
      val serverFailures           = Semaphore(0)
      val failedConnections        = mutable.ListBuffer.empty[Connection]

      val descriptor = startServer { conn =>
        new Callback[MessageBuffer] {
          override def complete(result: Try[MessageBuffer]): Unit = result match {
            case Success(mb) =>
              val text = String(mb.asArray)
              firstMessageByConnection.synchronized {
                firstMessageByConnection.getOrElseUpdate(conn, text)
              }
              serverMessages.release()
            case Failure(_: NoMoreDataException) =>
              failedConnections.synchronized(failedConnections += conn)
              serverFailures.release()
            case Failure(_) =>
              failedConnections.synchronized(failedConnections += conn)
              serverFailures.release()
          }
        }
      }

      val clientA = connectClient(descriptor, _ => printErrors(_ => ()))
      val clientB = connectClient(descriptor, _ => printErrors(_ => ()))

      clientA.send(ArrayMessageBuffer("client-a".getBytes)).run(_ => ())
      clientB.send(ArrayMessageBuffer("client-b".getBytes)).run(_ => ())

      await(serverMessages, 2, firstMessageByConnection)
      clientA.close()
      await(serverFailures, clue = "server did not observe disconnect of first client")

      val failed = failedConnections.synchronized(failedConnections.toList)
      assertEquals(failed.size, 1)
      val label = firstMessageByConnection.synchronized(firstMessageByConnection(failed.head))
      assertEquals(label, "client-a")
    }
  }

  test("multiple messages over the same connection have the same connection object on both sides") {
    if supportsStableConnectionObject then {
      given abort: Abort = Abort()

      val serverEstablished = mutable.ListBuffer.empty[Connection]
      val serverSeenOnMsg   = mutable.ListBuffer.empty[Connection]
      val serverMessages    = Semaphore(0)

      val clientEstablished = mutable.ListBuffer.empty[Connection]
      val clientMessages    = Semaphore(0)

      val descriptor = startServer { conn =>
        serverEstablished.synchronized(serverEstablished += conn)
        printErrors { mb =>
          serverSeenOnMsg.synchronized(serverSeenOnMsg += conn)
          serverMessages.release()
          conn.send(mb).runToFuture(())
        }
      }

      val preparedClientConn = connectClient(descriptor, { conn =>
        clientEstablished.synchronized(clientEstablished += conn)
        printErrors { _ =>
          clientMessages.release()
        }
      })

      preparedClientConn.send(ArrayMessageBuffer("one".getBytes)).run(_ => ())
      preparedClientConn.send(ArrayMessageBuffer("two".getBytes)).run(_ => ())

      await(serverMessages, 2, serverSeenOnMsg)
      await(clientMessages, 2, clientEstablished)

      val serverEstablishedList = serverEstablished.synchronized(serverEstablished.toList)
      val serverSeenList        = serverSeenOnMsg.synchronized(serverSeenOnMsg.toList)
      val clientEstablishedList = clientEstablished.synchronized(clientEstablished.toList)

      assertEquals(serverEstablishedList.size, 1)
      assertEquals(clientEstablishedList.size, 1)
      assert(serverSeenList.forall(conn => sameRef(conn.asInstanceOf[AnyRef], serverEstablishedList.head.asInstanceOf[AnyRef])))
      assert(sameRef(preparedClientConn.asInstanceOf[AnyRef], clientEstablishedList.head.asInstanceOf[AnyRef]))
    }
  }
}
