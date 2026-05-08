package webapps.ex2026overlaydemo

import channels.{Abort, ArrayMessageBuffer, MessageBuffer}
import channels.webnativewebsockets.WebsocketConnect
import de.rmgk.delay.Async

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

object NodeWebsocketEchoClient {

  @JSExportTopLevel("RunNioWebsocketClient")
  def run(url: String): js.Promise[String] = {
    val abort    = Abort()
    val received = ArrayBuffer.empty[String]
    val done     = Promise[String]()

    val outgoing = WebsocketConnect.connect(url).prepare { conn =>
      {
        case scala.util.Success(message: MessageBuffer) =>
          received += new String(message.asArray, StandardCharsets.UTF_8)
          if received.size >= 2 then {
            conn.close()
            done.trySuccess(received.mkString(",")): Unit
          }
        case scala.util.Failure(err) =>
          done.tryFailure(err): Unit
      }
    }

    val sent: Future[Unit] = Async[Abort] {
      val conn = outgoing.bind
      conn.send(ArrayMessageBuffer("hello-from-node-1".getBytes(StandardCharsets.UTF_8))).bind
      conn.send(ArrayMessageBuffer("hello-from-node-2".getBytes(StandardCharsets.UTF_8))).bind
    }.runToFuture(abort)

    sent.flatMap(_ => done.future).toJSPromise
  }
}
