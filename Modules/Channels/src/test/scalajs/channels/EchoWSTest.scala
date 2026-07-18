package channels

import channels.connection.{Abort, ByteBufferMessageBuffer}
import channels.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.Async

import scala.util.{Failure, Success}

class EchoWSTest extends munit.FunSuite {

  test("echo") {

    val outgoing = WebsocketConnect.connect("wss://echo.websocket.org/.ws").prepare { conn =>
      {
        case Success(msg) =>
        case Failure(ex)  => throw IllegalStateException(ex)
      }
    }

    val fut = Async[Abort]:
        val wsc = outgoing.bind
        val bb  = ByteBufferMessageBuffer("hello world".getBytes())
        wsc.send(bb).bind
    .runToFuture(Abort())
    fut
  }

}
