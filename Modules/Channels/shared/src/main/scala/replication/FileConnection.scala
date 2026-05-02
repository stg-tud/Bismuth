package replication

import channels.LatentConnection
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, scanJsonValuesFromStream, writeToString}
import de.rmgk.delay.{Async, Callback, Sync}

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Using

class FileConnection[T](path: Path)(using JsonValueCodec[PlumtreeMessage[T]])
    extends LatentConnection[PlumtreeMessage[T]] {

  class InnerConnection(peerFun: => Callback[PlumtreeMessage[T]]) extends channels.Connection[PlumtreeMessage[T]] {
    lazy val peer                                           = peerFun
    def send(message: PlumtreeMessage[T]): Async[Any, Unit] = Async.fromCallback {
      message match
          case PlumtreeMessage.Graft(sender, knows) =>
            Using(Files.newInputStream(path)) { is =>
              scanJsonValuesFromStream[PlumtreeMessage[T]](is) {
                case pm @ PlumtreeMessage.Payload(dots, _, _) if !(dots <= knows) =>
                  peer.succeed(pm)
                  true
                case _ => true
              }
            }
            ()
          case pl: PlumtreeMessage.Payload[T] =>
            val res = writeToString[PlumtreeMessage[T]](pl)
            Files.writeString(path, res + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND)
            ()
          case PlumtreeMessage.IHave(_, _) => ()
          case PlumtreeMessage.Prune(_)    => ()
    }
    def close(): Unit = ()
  }

  def prepare(receiver: channels.Receive[PlumtreeMessage[T]])
      : de.rmgk.delay.Async[channels.Abort, channels.Connection[PlumtreeMessage[T]]] =
    Sync {
      lazy val connection: InnerConnection = InnerConnection(receiver.messageHandler(connection))
      connection
    }
}
