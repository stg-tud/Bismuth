package ex2026niowebsocket

import channels.*

object NioWebsocketEchoServer {
  def main(args: Array[String]): Unit = {
    val port  = args.headOption.filter(_.nonEmpty).map(_.toInt).getOrElse(0)
    val abort = Abort()
    val nio   = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))

    val resolver            = new NioTcpConnectionDetailsResolver(nio)
    val (details, listener) = resolver.listen(port = port)

    listener.prepare { conn =>
      {
        case scala.util.Success(message) =>
          conn.send(message).run {
            case scala.util.Failure(err) => err.printStackTrace()
            case _                       => ()
          }
        case scala.util.Failure(err) =>
          err.printStackTrace()
      }
    }.runIn(abort) {
      case scala.util.Failure(err) => err.printStackTrace()
      case _                       => ()
    }

    println(s"WS_SERVER_PORT=${details.port}")
    println(s"WS_SERVER_DETAILS=$details")
    nio.loopSelection(abort)
  }
}
