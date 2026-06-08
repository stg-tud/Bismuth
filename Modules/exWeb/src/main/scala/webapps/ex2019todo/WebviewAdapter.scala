package webapps.ex2019todo

import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, LatentConnection, MessageBuffer, Receive}
import de.rmgk.delay.{Async, Sync}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object WebviewAdapterChannel {

  var receiveCallback: String => String = identity

  @JSExportTopLevel("webview_channel_receive")
  def receive(msg: String): String = receiveCallback(msg)

  object WebviewConnectionContext extends Connection {
    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val b64 = new String(java.util.Base64.getEncoder.encode(message.convertToArray()))
      if !js.isUndefined(scala.scalajs.js.Dynamic.global.webview_channel_send) then
          println("sending message to webview")
          scala.scalajs.js.Dynamic.global.webview_channel_send(b64)
          ()
      else
          println("webview channel send was undefined :(")
      // w.eval(s"""webview_channel_receive('$b64')""")
    }
    override def close(): Unit = ()
  }

  def listen(): LatentConnection[Connection] = new LatentConnection[Connection] {
    def prepare(incomingHandler: Receive): Async[Abort, Connection] = Sync {
      val conn = WebviewConnectionContext
      val cb   = incomingHandler.connectionEstablished(conn)
      receiveCallback = { (msg: String) =>
        val bytes = java.util.Base64.getDecoder.decode(msg)
        cb.succeed(ByteBufferMessageBuffer(bytes))
        ""
      }

      conn
    }
  }
}
