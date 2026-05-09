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
  ): LatentConnection[ConnectionDescriptor] =
    new Listener(socketFactory, executionContext)

  def connect(
      target: SocketAddress,
      socketFactory: () => DatagramSocket,
      executionContext: ExecutionContext
  ): LatentConnection[Connection] =
    new Client(target, socketFactory, executionContext)

  abstract private class Base(socketFactory: () => DatagramSocket, executionContext: ExecutionContext) {
    protected def startReceiver(datagramSocket: DatagramSocket, receiver: Receive)(using Abort): Unit = {
      val receiveBuffer = new Array[Byte](1 << 16)
      val connections   = mutable.Map.empty[SocketAddress, (UDPDatagramWrapper, Callback[MessageBuffer])]

      def getOrCreateConnection(sa: SocketAddress): (UDPDatagramWrapper, Callback[MessageBuffer]) =
        connections.synchronized {
          connections.getOrElseUpdate(
            sa, {
              val conn = UDPDatagramWrapper(sa, datagramSocket)
              conn -> receiver.connectionEstablished(conn)
            }
          )
        }

      executionContext.execute { () =>
        try {
          try {
            while !summon[Abort].closeRequest do
                val packet = new DatagramPacket(receiveBuffer, receiveBuffer.length)
                try {
                  datagramSocket.receive(packet)
                  val (_, callback) = getOrCreateConnection(packet.getSocketAddress)
                  callback.succeed(ArrayMessageBuffer(packet.getData.slice(
                    packet.getOffset,
                    packet.getOffset + packet.getLength
                  )))
                } catch case _: SocketTimeoutException => ()
          } finally datagramSocket.close()
        } catch {
          case NonFatal(e) =>
            connections.values.foreach((_, callback) => callback.fail(e))
        }
      }
    }

  }

  private class Listener(socketFactory: () => DatagramSocket, executionContext: ExecutionContext)
      extends Base(socketFactory, executionContext), LatentConnection[ConnectionDescriptor] {
    override def prepare(receiver: Receive): Async[Abort, ConnectionDescriptor] = Async {
      val datagramSocket = socketFactory()
      startReceiver(datagramSocket, receiver)
      ConnectionDescriptor.Udp(datagramSocket.getLocalAddress.getHostAddress, datagramSocket.getLocalPort)
    }
  }

  private class Client(target: SocketAddress, socketFactory: () => DatagramSocket, executionContext: ExecutionContext)
      extends Base(socketFactory, executionContext), LatentConnection[Connection] {
    override def prepare(receiver: Receive): Async[Abort, Connection] = Async {
      val datagramSocket = socketFactory()
      val conn           = UDPDatagramWrapper(target, datagramSocket)
      receiver.connectionEstablished(conn)
      startReceiver(datagramSocket, receiver)
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
