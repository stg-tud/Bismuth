package channels.webrtc

import channels.*
import channels.MesageBufferExtensions.asArrayBuffer
import de.rmgk.delay.{Async, Sync}
import org.scalajs.dom
import org.scalajs.dom.RTCDataChannelState

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

class WebRTCReceiveFailed(message: String)    extends Exception(message)
class WebRTCConnectionFailed(message: String) extends Exception(message)

private def rtcDebug(channel: dom.RTCDataChannel, message: => String): Unit =
  ()

class WebRTCConnection(channel: dom.RTCDataChannel) extends Connection {
  def receive: Prod[MessageBuffer] = Async.fromCallback {
    rtcDebug(channel, "receive() registered handlers")
    channel.onmessage = { (event: dom.MessageEvent) =>
      event.data match {
        case data: ArrayBuffer =>
          Async.handler.succeed(new JsArrayBufferMessageBuffer(data))

        case data: dom.Blob =>
          val reader = new dom.FileReader
          reader.onerror = { event =>
            Async.handler.fail(WebRTCReceiveFailed(s"reading message from blob returned error, event: $event"))
          }
          reader.onload = { (event: dom.Event) =>
            val data = event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
            Async.handler.succeed(new JsArrayBufferMessageBuffer(data))
          }
          reader.readAsArrayBuffer(data)

        case other =>
          throw IllegalStateException(
            s"received some message that is neither an array buffer nor a blob, but throw to\n$other"
          )
      }
    }

    channel.onerror = { (evt: dom.Event) =>
      rtcDebug(channel, s"receive handler saw error event=$evt")
      Async.handler.fail(WebRTCReceiveFailed(s"channel error: $evt"))
    }

    channel.onclose = { (evt: dom.Event) =>
      rtcDebug(channel, s"receive handler saw close event=$evt")
      Async.handler.fail(WebRTCReceiveFailed(s"channel closed: $evt"))
    }

    channel.readyState match
        case RTCDataChannelState.closed =>
          rtcDebug(channel, "receive() found channel already closed")
          Async.handler.fail(WebRTCReceiveFailed("channel already closed"))
        case _ =>

  }
  def send(message: MessageBuffer): Async[Any, Unit] =
    Sync(channel.send(message.asArrayBuffer))

  override def close(): Unit = channel.close()
}

object WebRTCConnection {
  def open(channel: dom.RTCDataChannel): Async[Any, WebRTCConnection] = Async.fromCallback {
    rtcDebug(channel, "open() called")
    channel.readyState match {
      case dom.RTCDataChannelState.connecting =>
        rtcDebug(channel, "open() waiting for onopen/onerror/onclose")
        channel.onopen = { (_: dom.Event) =>
          rtcDebug(channel, "open() succeeded via onopen")
          Async.handler.succeed(new WebRTCConnection(channel))
        }
        channel.onerror = { (evt: dom.Event) =>
          rtcDebug(channel, s"open() failed via onerror event=$evt")
          Async.handler.fail(new WebRTCConnectionFailed(s"channel error before open: $evt"))
        }
        channel.onclose = { (evt: dom.Event) =>
          rtcDebug(channel, s"open() failed via onclose event=$evt")
          Async.handler.fail(new WebRTCConnectionFailed(s"channel closed before open: $evt"))
        }

      case dom.RTCDataChannelState.open =>
        rtcDebug(channel, "open() succeeded immediately")
        Async.handler.succeed(new WebRTCConnection(channel))

      case dom.RTCDataChannelState.closing | dom.RTCDataChannelState.closed =>
        rtcDebug(channel, "open() failed immediately because channel is closing/closed")
        Async.handler.fail(new WebRTCConnectionFailed("channel closed"))
    }
  }

  def openLatent(channel: dom.RTCDataChannel): LatentConnection = new LatentConnection {

    def succeedConnection(incoming: Receive): WebRTCConnection = {
      rtcDebug(channel, "openLatent.succeedConnection() installing message handlers")
      val connector = new WebRTCConnection(channel)
      val handler   = incoming.messageHandler(connector)

      {
        channel.onmessage = { (event: dom.MessageEvent) =>
          event.data match {
            case data: ArrayBuffer =>
              handler.succeed(new JsArrayBufferMessageBuffer(data))

            case data: dom.Blob =>
              val reader = new dom.FileReader
              reader.onerror = { event =>
                handler.fail(WebRTCReceiveFailed(s"reading message from blob returned error, event: $event"))
              }
              reader.onload = { (event: dom.Event) =>
                val data = event.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
                handler.succeed(new JsArrayBufferMessageBuffer(data))
              }
              reader.readAsArrayBuffer(data)

            case other =>
              throw IllegalStateException(
                s"received some message that is neither an array buffer nor a blob, but throw to\n$other"
              )
          }
        }

        channel.onerror = { (evt: dom.Event) =>
          rtcDebug(channel, s"latent receive handler saw error event=$evt")
          handler.fail(WebRTCReceiveFailed(s"channel error: $evt"))
        }

        channel.onclose = { (evt: dom.Event) =>
          rtcDebug(channel, s"latent receive handler saw close event=$evt")
          handler.fail(WebRTCReceiveFailed(s"channel closed: $evt"))
        }

        channel.readyState match
            case RTCDataChannelState.closed =>
              rtcDebug(channel, "latent succeedConnection found channel already closed")
              handler.fail(WebRTCReceiveFailed("channel already closed"))
            case _ =>
      }

      connector
    }

    override def prepare(incomingHandler: Receive): Async[Abort, Connection] =
      Async.fromCallback {
        rtcDebug(channel, "openLatent.prepare() called")

        channel.readyState match {
          case dom.RTCDataChannelState.connecting =>
            rtcDebug(channel, "openLatent.prepare() waiting for onopen/onerror/onclose")
            channel.onopen = { (_: dom.Event) =>
              rtcDebug(channel, "openLatent.prepare() succeeded via onopen")
              Async.handler.succeed(succeedConnection(incomingHandler))
            }
            channel.onerror = { (evt: dom.Event) =>
              rtcDebug(channel, s"openLatent.prepare() failed via onerror event=$evt")
              Async.handler.fail(new WebRTCConnectionFailed(s"channel error before open: $evt"))
            }
            channel.onclose = { (evt: dom.Event) =>
              rtcDebug(channel, s"openLatent.prepare() failed via onclose event=$evt")
              Async.handler.fail(new WebRTCConnectionFailed(s"channel closed before open: $evt"))
            }

          case dom.RTCDataChannelState.open =>
            rtcDebug(channel, "openLatent.prepare() succeeded immediately")
            Async.handler.succeed(succeedConnection(incomingHandler))

          case dom.RTCDataChannelState.closing | dom.RTCDataChannelState.closed =>
            rtcDebug(channel, "openLatent.prepare() failed immediately because channel is closing/closed")
            Async.handler.fail(new WebRTCConnectionFailed("channel closed"))
        }
      }

  }

}
