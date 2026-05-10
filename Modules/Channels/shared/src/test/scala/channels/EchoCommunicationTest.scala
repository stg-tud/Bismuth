package channels

import channels.TestUtil.printErrors
import de.rmgk.delay
import de.rmgk.delay.{Async, Callback}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait EchoCommunicationTest[CD <: ConnectionDescriptor](
    serverConn: ExecutionContext => LatentConnection[CD],
    clientConn: ExecutionContext => CD => LatentConnection[Connection]
) extends munit.FunSuite {

  def supportsMultipleConnections: Boolean    = true
  def supportsDisconnectDetection: Boolean    = true
  def supportsStableConnectionObject: Boolean = true
  def extraCleanup(cleanups: mutable.ListBuffer[() => Unit]): Unit = ()

  given ec: ExecutionContext = ExecutionContext.global

  private def sameRef(a: AnyRef, b: AnyRef): Boolean = a eq b

  private def asReceive(receiver: Connection => Callback[MessageBuffer]): Receive = new Receive {
    override def connectionEstablished(answers: Connection): Callback[MessageBuffer] = receiver(answers)
  }

  private final class Counter(target: Int) {
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

  private def startServer(receiver: Connection => Callback[MessageBuffer]): Async[Abort, CD] =
    serverConn(ec).prepare(asReceive(receiver))

  private def connectClient(descriptor: CD, receiver: Connection => Callback[MessageBuffer]): Async[Abort, Connection] =
    clientConn(ec)(descriptor).prepare(asReceive(receiver))

  private def withCleanup[A](body: (Abort, mutable.ListBuffer[Connection]) => Future[A]): Future[A] = {
    val abort    = Abort()
    val clients  = mutable.ListBuffer.empty[Connection]
    val cleanups = mutable.ListBuffer.empty[() => Unit]
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
            conn.send(mb).runToFuture(())
          }
        }.bind
        val client = connectClient(descriptor, _ =>
          printErrors { mb =>
            received.synchronized(received += String(mb.asArray))
            messageCounter.hit()
          }
        ).bind
        clients += client
        toSend.foreach(msg => client.send(ArrayMessageBuffer(msg.getBytes())).run(printErrors(_ => ())))
        messageCounter.async.bind
        assertEquals(received.synchronized(received.toList).sorted, toSend.sorted)
      }.runToFuture(abort)
    }
  }

  test("multiple connections to the server at the same time work") {
    if !supportsMultipleConnections then Future.successful(())
    else withCleanup { (abort, clients) =>
      given Abort = abort

      val serverConnections = mutable.Set.empty[Connection]
      val clientReceived    = mutable.ListBuffer.empty[String]
      val receivedCounter   = Counter(2)

      Async[Abort] {
        val descriptor = startServer { conn =>
          serverConnections.synchronized(serverConnections += conn)
          printErrors { mb =>
            conn.send(mb).runToFuture(())
          }
        }.bind
        val clientA = connectClient(descriptor, _ =>
          printErrors { mb =>
            clientReceived.synchronized(clientReceived += String(mb.asArray))
            receivedCounter.hit()
          }
        ).bind
        val clientB = connectClient(descriptor, _ =>
          printErrors { mb =>
            clientReceived.synchronized(clientReceived += String(mb.asArray))
            receivedCounter.hit()
          }
        ).bind
        clients += clientA
        clients += clientB
        clientA.send(ArrayMessageBuffer("client-a".getBytes)).run(_ => ())
        clientB.send(ArrayMessageBuffer("client-b".getBytes)).run(_ => ())
        receivedCounter.async.bind
        assertEquals(clientReceived.toSet, Set("client-a", "client-b"))
        assertEquals(serverConnections.synchronized(serverConnections.size), 2)
      }.runToFuture(abort)
    }
  }

  test("disconnects are detected on the other side") {
    if !supportsDisconnectDetection then Future.successful(())
    else withCleanup { (abort, clients) =>
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
        client.send(ArrayMessageBuffer("hello".getBytes)).run(_ => ())
        serverMessages.async.bind
        client.close()
        serverFailures.async.bind
        assertEquals(failedOnServer.synchronized(failedOnServer.size), 1)
      }.runToFuture(abort)
    }
  }

  test("multiple connections and then disconnects detect the correct connection disconnecting") {
    if !supportsDisconnectDetection then Future.successful(())
    else withCleanup { (abort, clients) =>
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
                val text = String(mb.asArray)
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
        clientA.send(ArrayMessageBuffer("client-a".getBytes)).run(_ => ())
        clientB.send(ArrayMessageBuffer("client-b".getBytes)).run(_ => ())
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
    else withCleanup { (abort, clients) =>
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
            conn.send(mb).runToFuture(())
          }
        }.bind
        val preparedClientConn = connectClient(descriptor, { conn =>
          clientEstablished.synchronized(clientEstablished += conn)
          printErrors { _ =>
            clientMessages.hit()
          }
        }).bind
        clients += preparedClientConn
        preparedClientConn.send(ArrayMessageBuffer("one".getBytes)).run(_ => ())
        preparedClientConn.send(ArrayMessageBuffer("two".getBytes)).run(_ => ())
        serverMessages.async.bind
        clientMessages.async.bind
        val serverEstablishedList = serverEstablished.synchronized(serverEstablished.toList)
        val serverSeenList        = serverSeenOnMsg.synchronized(serverSeenOnMsg.toList)
        val clientEstablishedList = clientEstablished.synchronized(clientEstablished.toList)

        assertEquals(serverEstablishedList.size, 1)
        assertEquals(clientEstablishedList.size, 1)
        assert(serverSeenList.forall(conn => sameRef(conn.asInstanceOf[AnyRef], serverEstablishedList.head.asInstanceOf[AnyRef])))
        assert(sameRef(preparedClientConn.asInstanceOf[AnyRef], clientEstablishedList.head.asInstanceOf[AnyRef]))
      }.runToFuture(abort)
    }
  }
}
