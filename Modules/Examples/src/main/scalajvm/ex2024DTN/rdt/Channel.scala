package ex2024DTN.rdt

import channels.MessagingIO
import channels.broadcast.PlumtreeMessage
import channels.broadcast.PlumtreeMessage.{Graft, IHave, Payload, Prune}
import channels.connection.{Abort, Connection, Connector, MessageBuffer, Peer, Receive}
import channels.experiments.Aead
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import de.rmgk.delay.{Async, Callback, Sync, toAsync}
import ex2024DTN.{MonitoringClientInterface, NoMonitoringClient, RdtMessageType}
import rdts.base.Uid
import rdts.time.Dots

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

enum ClientOperationMode {
  case PushAll
  case RequestLater
}

class ClientContext[T: JsonValueCodec](
    connection: Client,
    executionContext: ExecutionContext,
    operationMode: ClientOperationMode
) extends Connection {
  override def send(message: MessageBuffer): Async[Any, Unit] =
    MessagingIO.decodeEnvelope[T](message, Aead.identity) match
        case Failure(exception)                          => Async.fromCallback(Async.handler.fail(exception))
        case Success(MessagingIO.Envelope.Membership(_)) => Async {}
        case Success(MessagingIO.Envelope.Broadcast(_, Graft(dots))) =>
          // we could send requests into the network. the routing handles them correctly. but they are unnecessary with the cb.succeed() down below.
          // todo: actually there should be no requests being sent anymore then. is that the case?
          operationMode match
              case ClientOperationMode.PushAll      => Sync { () }
              case ClientOperationMode.RequestLater =>
                connection.send(RdtMessageType.Request, Array(), dots).toAsync(using executionContext)
        case Success(MessagingIO.Envelope.Broadcast(_, Payload(dots, data))) =>
          connection.send(
            RdtMessageType.Payload,
            writeToArray[T](data),
            dots
          ).toAsync(using executionContext)
        case Success(MessagingIO.Envelope.Broadcast(_, IHave(_) | Prune)) => Async {}

  override def close(): Unit = connection.close().onComplete {
    case Failure(f)     => f.printStackTrace()
    case Success(value) => ()
  }(using executionContext)
}

class Channel[T: JsonValueCodec](
    host: String,
    port: Int,
    appName: String,
    ec: ExecutionContext,
    monitoringClient: MonitoringClientInterface = NoMonitoringClient,
    operationMode: ClientOperationMode = ClientOperationMode.PushAll
) extends Connector {

  // We use a local dtnid instead of a remote replica ID to signify that the local DTNd is the one providing information.
  // If the local dtnd could be stopped and restarted without loosing data, this id should remain the same for performance reasons, but it will be correct even if it changes.
  val dtnid: Uid      = Uid.gen()
  private val dtnPeer = Peer.Claimed(dtnid)

  override def connect(receiver: Receive): Async[Abort, Connection] =
    Async {
      val client: Client = Client(host, port, appName, monitoringClient).toAsync(using ec).bind
      val conn           = ClientContext[T](client, ec, operationMode)
      val cb             = receiver.connectionEstablished(conn)

      client.registerOnReceive { (message_type: RdtMessageType, payload: Array[Byte], dots: Dots) =>
        message_type match
            case RdtMessageType.Request =>
              cb.succeed(MessagingIO.encodeEnvelope(
                MessagingIO.Envelope.Broadcast(dtnPeer, PlumtreeMessage.Graft(dots)),
                Aead.identity
              ))
            case RdtMessageType.Payload =>
              cb.succeed(MessagingIO.encodeEnvelope(
                MessagingIO.Envelope.Broadcast(
                  dtnPeer,
                  PlumtreeMessage.Payload(
                    dots,
                    readFromArray[T](payload)
                  )
                ),
                Aead.identity
              ))
      }

      // This tells the rdt to send everything it has and new following stuff into the network.
      // It makes any requests unnecessary.
      operationMode match
          case ClientOperationMode.PushAll =>
            cb.succeed(MessagingIO.encodeEnvelope(
              MessagingIO.Envelope.Broadcast(dtnPeer, PlumtreeMessage.Graft(Dots.empty)),
              Aead.identity
            ))
          case ClientOperationMode.RequestLater =>

      conn
    }
}
