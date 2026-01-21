package channels

import channels.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.{Async, Callback}

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
        wsc.send(ArrayMessageBuffer("hello world".getBytes())).bind
    .runToFuture(Abort())
    fut
  }

}
