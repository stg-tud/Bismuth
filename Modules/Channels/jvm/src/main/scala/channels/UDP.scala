package channels

import de.rmgk.delay.{Async, Callback}

import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketAddress, SocketTimeoutException}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object UDP {
  def listen(
      socketFactory: () => DatagramSocket,
      executionContext: ExecutionContext
  ): LatentConnection[ConnectionDescriptor.Udp] =
    new Listener(socketFactory, executionContext)

  def connect(
      target: SocketAddress,
      socketFactory: () => DatagramSocket,
      executionContext: ExecutionContext
  ): LatentConnection[Connection] =
    new Client(target, socketFactory, executionContext)

  abstract private class Base(socketFactory: () => DatagramSocket, executionContext: ExecutionContext) {
    private def loopReceive(
        datagramSocket: DatagramSocket,
        receiverFor: SocketAddress => (UDPDatagramWrapper, Callback[MessageBuffer]),
        onFailure: Throwable => Unit,
    )(using Abort): Unit = {
      val receiveBuffer = new Array[Byte](1 << 16)

      executionContext.execute { () =>
        try {
          try {
            datagramSocket.setSoTimeout(1000)
            while !summon[Abort].closeRequest do
                val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
                try {
                  datagramSocket.receive(packet)
                  val (_, callback) = receiverFor(packet.getSocketAddress)
                  callback.succeed(ArrayMessageBuffer(packet.getData.slice(
                    packet.getOffset,
                    packet.getOffset + packet.getLength
                  )))
                } catch case _: SocketTimeoutException => ()
          } finally datagramSocket.close()
        } catch {
          case NonFatal(e) => onFailure(e)
        }
      }
    }

    protected def startServerReceiver(datagramSocket: DatagramSocket, receiver: Receive)(using Abort): Unit = {
      val connections = mutable.Map.empty[SocketAddress, (UDPDatagramWrapper, Callback[MessageBuffer])]

      def getOrCreateConnection(sa: SocketAddress): (UDPDatagramWrapper, Callback[MessageBuffer]) =
        connections.synchronized {
          connections.getOrElseUpdate(
            sa, {
              val conn = UDPDatagramWrapper(sa, datagramSocket)
              conn -> receiver.connectionEstablished(conn)
            }
          )
        }

      loopReceive(
        datagramSocket,
        receiverFor = getOrCreateConnection,
        onFailure = e => connections.values.foreach((_, callback) => callback.fail(e))
      )
    }

    protected def startClientReceiver(
        datagramSocket: DatagramSocket,
        connection: UDPDatagramWrapper,
        callback: Callback[MessageBuffer],
    )(using Abort): Unit =
      loopReceive(
        datagramSocket,
        receiverFor = _ => connection -> callback,
        onFailure = callback.fail
      )

  }

  private class Listener(socketFactory: () => DatagramSocket, executionContext: ExecutionContext)
      extends Base(socketFactory, executionContext), LatentConnection[ConnectionDescriptor.Udp] {
    override def prepare(receiver: Receive): Async[Abort, ConnectionDescriptor.Udp] = Async {
      val datagramSocket = socketFactory()
      startServerReceiver(datagramSocket, receiver)
      ConnectionDescriptor.Udp(datagramSocket.getLocalAddress.getHostAddress, datagramSocket.getLocalPort)
    }
  }

  private class Client(target: SocketAddress, socketFactory: () => DatagramSocket, executionContext: ExecutionContext)
      extends Base(socketFactory, executionContext), LatentConnection[Connection] {
    override def prepare(receiver: Receive): Async[Abort, Connection] = Async {
      val datagramSocket = socketFactory()
      val conn           = UDPDatagramWrapper(target, datagramSocket)
      val callback       = receiver.connectionEstablished(conn)
      startClientReceiver(datagramSocket, conn, callback)
      conn
    }
  }
}

class UDPDatagramWrapper(target: SocketAddress, datagramSocket: DatagramSocket) extends Connection {

  override val info: ConnectionInfo =
    (datagramSocket.getLocalSocketAddress, target) match
        case (local: InetSocketAddress, remote: InetSocketAddress) =>
          ConnectionInfo(
            local = Some(ConnectionDescriptor.Udp(local.getHostString, local.getPort)),
            remote = Some(ConnectionDescriptor.Udp(remote.getHostString, remote.getPort)),
            details = Map("type" -> "udp")
          )
        case (local: InetSocketAddress, _) =>
          ConnectionInfo(
            local = Some(ConnectionDescriptor.Udp(local.getHostString, local.getPort)),
            details = Map("type" -> "udp")
          )
        case _ => ConnectionInfo("type" -> "udp")

  def send(message: MessageBuffer): Async[Any, Unit] = Async {
    val outArray   = message.asArray
    val sendPacket = new DatagramPacket(outArray, outArray.length, target)
    datagramSocket.send(sendPacket)
  }

  override def close(): Unit = datagramSocket.close()
}
