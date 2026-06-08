package channels

import channels.TestUtil.printErrors
import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, ConnectionDescriptor, LatentConnection, MessageBuffer, NoMoreDataException}
import de.rmgk.delay
import de.rmgk.delay.{Async, Callback, Sync}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait EchoCommunicationTest[CD <: ConnectionDescriptor](
    serverConn: (ExecutionContext, Abort) => LatentConnection[CD],
    clientConn: (ExecutionContext, Abort) => CD => LatentConnection[Connection]
) extends munit.FunSuite {

  def supportsMultipleConnections: Boolean                         = true
  def supportsDisconnectDetection: Boolean                         = true
  def supportsStableConnectionObject: Boolean                      = true
  def extraCleanup(cleanups: mutable.ListBuffer[() => Unit]): Unit = ()

  def testExecutionContext: ExecutionContext = TestExecutionContextProvider.create()

  final private class Counter(target: Int) {
    private var current = 0
    private val promise = delay.Promise[Unit]()

    def hit(): Unit = synchronized {
      if current < target then {
        current += 1
        if current > target then throw new IllegalStateException(s"Counter hit $current times, expected $target")
        if current == target then promise.succeed(())
      }
    }

    def async: Async[Any, Unit] = promise.async
  }

  private def startServer(receiver: Connection => Callback[MessageBuffer])(using abort: Abort): Async[Abort, CD] =
    serverConn(testExecutionContext, abort).prepare(receiver(_))

  private def connectClient(descriptor: CD, receiver: Connection => Callback[MessageBuffer])(using
      abort: Abort
  ): Async[Abort, Connection] =
    clientConn(testExecutionContext, abort)(descriptor).prepare(receiver(_))

  private def sendAll(connection: Connection, messages: List[String]): Async[Any, Unit] = messages match {
    case Nil          => Sync(())
    case head :: tail => connection.send(ByteBufferMessageBuffer(head.getBytes)).flatMap(_ => sendAll(connection, tail))
  }

  private def withCleanup[A](body: (Abort, mutable.ListBuffer[Connection]) => Future[A]): Future[A] = {
    given ExecutionContext = testExecutionContext
    val abort              = Abort()
    val clients            = mutable.ListBuffer.empty[Connection]
    val cleanups           = mutable.ListBuffer.empty[() => Unit]
    extraCleanup(cleanups)
    body(abort, clients).transform { result =>
      clients.synchronized(clients.toList).foreach { conn =>
        try conn.close()
        catch case _: Throwable => ()
      }
      cleanups.synchronized(cleanups.toList).foreach { cleanup =>
        try cleanup()
        catch case _: Throwable => ()
      }
      abort.abort()
      result
    }
  }

  test("sample communication") {
    withCleanup { (abort, clients) =>
      given Abort = abort

      val toSend         = List("Hi", "ho", "let’s", "go")
      val messageCounter = Counter(toSend.size)
      val received       = mutable.ListBuffer.empty[String]

      Async[Abort] {
        val descriptor = startServer { conn =>
          printErrors { mb =>
            conn.send(mb).run(printErrors(_ => ()))
          }
        }.bind
        val client = connectClient(
          descriptor,
          _ =>
            printErrors { mb =>
              received.synchronized(received += String(mb.convertToArray()))
              messageCounter.hit()
            }
        ).bind
        clients += client
        sendAll(client, toSend).bind
        messageCounter.async.bind
        assertEquals(received.synchronized(received.toList).sorted, toSend.sorted)
      }.runToFuture(abort)
    }
  }

  test("multiple connections to the server at the same time work") {
    if !supportsMultipleConnections then Future.successful(())
    else
        withCleanup { (abort, clients) =>
          given Abort = abort

          val serverConnections = mutable.Set.empty[Connection]
          val clientReceived    = mutable.ListBuffer.empty[String]
          val receivedCounter   = Counter(2)

          Async[Abort] {
            val descriptor = startServer { conn =>
              serverConnections.synchronized(serverConnections += conn)
              printErrors { mb =>
                conn.send(mb).run(printErrors(_ => ()))
              }
            }.bind
            val clientA = connectClient(
              descriptor,
              _ =>
                printErrors { mb =>
                  clientReceived.synchronized(clientReceived += String(mb.convertToArray()))
                  receivedCounter.hit()
                }
            ).bind
            val clientB = connectClient(
              descriptor,
              _ =>
                printErrors { mb =>
                  clientReceived.synchronized(clientReceived += String(mb.convertToArray()))
                  receivedCounter.hit()
                }
            ).bind
            clients += clientA
            clients += clientB
            clientA.send(ByteBufferMessageBuffer("client-a".getBytes)).bind
            clientB.send(ByteBufferMessageBuffer("client-b".getBytes)).bind
            receivedCounter.async.bind
            assertEquals(clientReceived.toSet, Set("client-a", "client-b"))
            assertEquals(serverConnections.synchronized(serverConnections.size), 2)
          }.runToFuture(abort)
        }
  }

  test("disconnects are detected on the other side") {
    if !supportsDisconnectDetection then Future.successful(())
    else
        withCleanup { (abort, clients) =>
          given Abort = abort

          val serverMessages = Counter(1)
          val serverFailures = Counter(1)
          val failedOnServer = mutable.ListBuffer.empty[Connection]

          Async[Abort] {
            val descriptor = startServer { conn =>
              new Callback[MessageBuffer] {
                override def complete(result: Try[MessageBuffer]): Unit = result match {
                  case Success(_) =>
                    serverMessages.hit()
                  case Failure(_: NoMoreDataException) =>
                    failedOnServer.synchronized(failedOnServer += conn)
                    serverFailures.hit()
                  case Failure(_) =>
                    failedOnServer.synchronized(failedOnServer += conn)
                    serverFailures.hit()
                }
              }
            }.bind
            val client = connectClient(descriptor, _ => printErrors(_ => ())).bind
            clients += client
            client.send(ByteBufferMessageBuffer("hello".getBytes)).bind
            serverMessages.async.bind
            client.close()
            serverFailures.async.bind
            assertEquals(failedOnServer.synchronized(failedOnServer.size), 1)
          }.runToFuture(abort)
        }
  }

  test("multiple connections and then disconnects detect the correct connection disconnecting") {
    if !supportsDisconnectDetection then Future.successful(())
    else
        withCleanup { (abort, clients) =>
          given Abort = abort

          val firstMessageByConnection = mutable.Map.empty[Connection, String]
          val serverMessages           = Counter(2)
          val serverFailures           = Counter(1)
          val failedConnections        = mutable.ListBuffer.empty[Connection]

          Async[Abort] {
            val descriptor = startServer { conn =>
              new Callback[MessageBuffer] {
                override def complete(result: Try[MessageBuffer]): Unit = result match {
                  case Success(mb) =>
                    val text = String(mb.convertToArray())
                    firstMessageByConnection.synchronized {
                      firstMessageByConnection.getOrElseUpdate(conn, text)
                    }
                    serverMessages.hit()
                  case Failure(_: NoMoreDataException) =>
                    failedConnections.synchronized(failedConnections += conn)
                    serverFailures.hit()
                  case Failure(_) =>
                    failedConnections.synchronized(failedConnections += conn)
                    serverFailures.hit()
                }
              }
            }.bind
            val clientA = connectClient(descriptor, _ => printErrors(_ => ())).bind
            val clientB = connectClient(descriptor, _ => printErrors(_ => ())).bind
            clients += clientA
            clients += clientB
            clientA.send(ByteBufferMessageBuffer("client-a".getBytes)).bind
            clientB.send(ByteBufferMessageBuffer("client-b".getBytes)).bind
            serverMessages.async.bind
            clientA.close()
            serverFailures.async.bind
            val failed = failedConnections.synchronized(failedConnections.toList)
            assertEquals(failed.size, 1)
            val label = firstMessageByConnection.synchronized(firstMessageByConnection(failed.head))
            assertEquals(label, "client-a")
          }.runToFuture(abort)
        }
  }

  test("multiple messages over the same connection have the same connection object on both sides") {
    if !supportsStableConnectionObject then Future.successful(())
    else
        withCleanup { (abort, clients) =>
          given Abort = abort

          val serverEstablished = mutable.ListBuffer.empty[Connection]
          val serverSeenOnMsg   = mutable.ListBuffer.empty[Connection]
          val serverMessages    = Counter(2)

          val clientEstablished = mutable.ListBuffer.empty[Connection]
          val clientMessages    = Counter(2)

          Async[Abort] {
            val descriptor = startServer { conn =>
              serverEstablished.synchronized(serverEstablished += conn)
              printErrors { mb =>
                serverSeenOnMsg.synchronized(serverSeenOnMsg += conn)
                serverMessages.hit()
                conn.send(mb).run(printErrors(_ => ()))
              }
            }.bind
            val preparedClientConn = connectClient(
              descriptor,
              { conn =>
                clientEstablished.synchronized(clientEstablished += conn)
                printErrors { _ =>
                  clientMessages.hit()
                }
              }
            ).bind
            clients += preparedClientConn
            preparedClientConn.send(ByteBufferMessageBuffer("one".getBytes)).bind
            preparedClientConn.send(ByteBufferMessageBuffer("two".getBytes)).bind
            serverMessages.async.bind
            clientMessages.async.bind
            val serverEstablishedList = serverEstablished.synchronized(serverEstablished.toList)
            val serverSeenList        = serverSeenOnMsg.synchronized(serverSeenOnMsg.toList)
            val clientEstablishedList = clientEstablished.synchronized(clientEstablished.toList)

            assertEquals(serverEstablishedList.size, 1)
            assertEquals(clientEstablishedList.size, 1)
            assert(serverSeenList.forall(conn =>
              conn.asInstanceOf[AnyRef] eq serverEstablishedList.head.asInstanceOf[AnyRef]
            ))
            assert(preparedClientConn.asInstanceOf[AnyRef] eq clientEstablishedList.head.asInstanceOf[AnyRef])
          }.runToFuture(abort)
        }
  }
}
