package channels

import channels.{Abort, JSHttpPseudoChannel}

import scala.util.{Failure, Success}

object FetchConnectTest {

  def main(args: Array[String]): Unit = {
    JSHttpPseudoChannel.connect(s"http://localhost:58080/channel").prepare { conn =>
      {
        case Success(msg) => println(msg.convert: String)
        case Failure(ex)  => ex.printStackTrace()
      }
    }.run(using Abort()) {
      case Success(conn) =>
        println(s"established")
        conn.send(ArrayMessageBuffer("Test".getBytes()))
      case Failure(ex) => ex.printStackTrace()
    }

    Thread.sleep(1000)
  }

}
