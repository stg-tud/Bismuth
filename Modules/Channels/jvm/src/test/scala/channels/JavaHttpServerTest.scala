package channels

import channels.JavaHttp.SSEServer
import channels.MessageBuffer.given_Conversion_String_MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.sun.net.httpserver.HttpServer
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.MultiVersionRegister
import replication.DeltaDissemination

import java.net.http.HttpClient
import java.net.{InetSocketAddress, URI}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object JavaHttpServerTest {

  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(InetSocketAddress(58080), 0)

    val conn = SSEServer(handler => server.createContext("/channel", handler))

    conn.prepare(inc => msg => println(msg)).runIn(Abort()):
      case Failure(ex)   => ex.printStackTrace()
      case Success(conn) =>
        println("received connection, replying")
        conn.send("yay!".convert).runIn(Abort())(res => println(res))

    server.start()

  }
}

object JavaHttpServerTest2 {

  given JsonValueCodec[MultiVersionRegister[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(InetSocketAddress(0), 0)

    val serverId = LocalUid.gen()

    val serverDiss = DeltaDissemination[MultiVersionRegister[String]](
      serverId,
      delta => println(s"server received: $delta")
    )

    serverDiss.addBinaryConnection(SSEServer(handler => server.createContext("/channel", handler)))

    serverDiss.applyDelta(MultiVersionRegister.of("Hello! from the server")(using serverId))

    server.start()

    val port = server.getAddress.getPort
    println(port)

    val clientId = LocalUid.gen()

    val clientDiss = DeltaDissemination[MultiVersionRegister[String]](
      clientId,
      delta => println(s"client received: $delta")
    )

    val client = HttpClient.newHttpClient()
    val ec     = ExecutionContext.global
    clientDiss.addBinaryConnection(JavaHttp.SSEClient(client, new URI(s"http://localhost:$port/channel"), ec))

    clientDiss.applyDelta(MultiVersionRegister.of("Hello!")(using clientId))

    println("waiting for shutdown")
    Thread.sleep(1000)
    server.stop(0)
  }
}
