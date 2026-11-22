package channels.connection

import de.rmgk.delay.Async

import java.net.{InetSocketAddress, ServerSocket, Socket, SocketException, SocketTimeoutException}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object TCP {

  inline def syncAttempt[T](inline x: T): Async[Any, T] = Async.fromCallback {
    try Async.handler.succeed(x)
    catch case NonFatal(exception) => Async.handler.fail(exception)
  }

  def handleConnection(
      socket: Socket,
      incoming: Receive,
      executionContext: ExecutionContext,
      dialed: Boolean,
  ): JIOStreamConnection = {
    socket.setTcpNoDelay(true)
    val conn = new JIOStreamConnection(socket.getInputStream, socket.getOutputStream, () => socket.close(), incoming)
    // we speak plain when dialing, accepted connections are established once the remote side chose
    if dialed then conn.establish(WebsocketMultiplexing.Framing.Plain)
    executionContext.execute: () =>
        conn.loopReceive()
    conn
  }

  def defaultSocket(socketAddress: InetSocketAddress): () => Socket =
    () => new Socket(socketAddress.getAddress, socketAddress.getPort)

  def connect(bindsocket: () => Socket, executionContext: ExecutionContext): Connector =
    new Connector {
      override def connect(incoming: Receive): Async[Any, Connection] =
        TCP.syncAttempt {
          TCP.handleConnection(bindsocket(), incoming, executionContext, dialed = true)
        }
    }

  def defaultServerSocket(socketAddress: InetSocketAddress): () => ServerSocket = () => {
    val socket = new ServerSocket

    try
      socket.setReuseAddress(true)
    catch {
      case _: SocketException =>
      // some implementations may not allow SO_REUSEADDR to be set
    }

    socket.bind(socketAddress)
    socket
  }

  def listen(
      bindsocket: () => ServerSocket,
      executionContext: ExecutionContext
  ): Listener =
    new Listener {
      override def listen(incoming: Receive): Async[Abort, ConnectionDescriptor.TcpWebSocket] =
        Async.fromCallback { abort ?=>
          try

              val socket = bindsocket()
              Async.handler.succeed(
                ConnectionDescriptor.TcpWebSocket(socket.getInetAddress.getHostAddress, socket.getLocalPort)
              )

              executionContext.execute { () =>
                try
                    socket.setSoTimeout(1000)
                    while !abort.closeRequest do
                        try
                            val connection = socket.accept()
                            if connection != null
                            then
                                TCP.handleConnection(connection, incoming, executionContext, dialed = false)
                                ()
                        catch case _: SocketTimeoutException => ()
                catch {
                  case exception: SocketException =>
                    Async.handler.fail(exception)
                }
              }

          catch
              case NonFatal(ex) => Async.handler.fail(ex)
        }
    }

}
