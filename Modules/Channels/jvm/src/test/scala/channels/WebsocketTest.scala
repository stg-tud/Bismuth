package channels

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.util.concurrent.{CompletableFuture, CountDownLatch, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.FutureConverters.*

object WebsocketTest {

  def main(args: Array[String]): Unit = {
    println("[client] Starting WebSocket Test...")

    // Start the server in a background thread
    val serverThread = new Thread(
      () =>
        SimpleWebsocket.main(Array()),
      "websocket-server"
    )
    serverThread.setDaemon(true)
    serverThread.start()

    // Give the server time to start (HttpServer starts faster)
    Thread.sleep(300)

    // Test results collector
    val receivedMessages = scala.collection.mutable.ListBuffer[String]()
    val latch            = new CountDownLatch(1) // We expect close response from server

    // Create WebSocket client
    val client = HttpClient.newHttpClient()

    val listener = new WebSocket.Listener {
      override def onOpen(webSocket: WebSocket): Unit = {
        println("[client] WebSocket opened")
        webSocket.request(1)
      }

      override def onText(webSocket: WebSocket, data: CharSequence, last: Boolean): CompletableFuture[?] = {
        println(s"[client] received text: $data")
        receivedMessages += s"text:$data"
        latch.countDown()
        webSocket.request(1)
        CompletableFuture.completedFuture(null)
      }

      override def onBinary(webSocket: WebSocket, data: ByteBuffer, last: Boolean): CompletableFuture[?] = {
        val bytes = new Array[Byte](data.remaining())
        data.get(bytes)
        println(s"[client] received binary: ${bytes.length} bytes")
        receivedMessages += s"binary:${bytes.length}"
        latch.countDown()
        webSocket.request(1)
        CompletableFuture.completedFuture(null)
      }

      override def onPing(webSocket: WebSocket, message: ByteBuffer): CompletableFuture[?] = {
        println("[client] received ping")
        receivedMessages += "ping"
        latch.countDown()
        webSocket.request(1)
        CompletableFuture.completedFuture(null)
      }

      override def onPong(webSocket: WebSocket, message: ByteBuffer): CompletableFuture[?] = {
        println("[client] received pong")
        receivedMessages += "pong"
        latch.countDown()
        webSocket.request(1)
        CompletableFuture.completedFuture(null)
      }

      override def onClose(webSocket: WebSocket, statusCode: Int, reason: String): CompletableFuture[?] = {
        println(s"[client] WebSocket closed (code=$statusCode, reason=$reason)")
        receivedMessages += s"close:$statusCode:$reason"
        latch.countDown()
        CompletableFuture.completedFuture(null)
      }

      override def onError(webSocket: WebSocket, error: Throwable): Unit = {
        println(s"[client] error: ${error.getMessage}")
        error.printStackTrace()
      }
    }

    // Connect to the server
    println("[client] Connecting to ws://localhost:8080...")
    val wsFuture = client.newWebSocketBuilder()
      .buildAsync(URI.create("ws://localhost:8080"), listener)
      .asScala

    val webSocket = Await.result(wsFuture, 5.seconds)
    println("[client] Connected!")

    // Send a simple text message (opcode 0x1)
    println("[client] Sending text message...")
    webSocket.sendText("Hello, WebSocket!", true).get(5, TimeUnit.SECONDS)
    Thread.sleep(100)

    // Send a binary message (opcode 0x2)
    println("[client] Sending binary message...")
    val binaryData = Array[Byte](0x01, 0x02, 0x03, 0x04, 0x05)
    webSocket.sendBinary(ByteBuffer.wrap(binaryData), true).get(5, TimeUnit.SECONDS)
    Thread.sleep(100)

    // Send a fragmented text message using continuation frames
    println("[client] Sending fragmented text (3 parts)...")
    webSocket.sendText("Part1-", false).get(5, TimeUnit.SECONDS) // First frame, not final
    Thread.sleep(50)
    webSocket.sendText("Part2-", false).get(5, TimeUnit.SECONDS) // Continuation, not final
    Thread.sleep(50)
    webSocket.sendText("Part3-End", true).get(5, TimeUnit.SECONDS) // Continuation, final
    Thread.sleep(100)

    // Send a ping (opcode 0x9) - server should receive it
    println("[client] Sending ping...")
    val pingData = Array[Byte](0x48, 0x69) // "Hi" in bytes
    webSocket.sendPing(ByteBuffer.wrap(pingData)).get(5, TimeUnit.SECONDS)
    Thread.sleep(100)

    // Note: The SimpleWebsocket server doesn't send responses back, so we won't receive
    // text/binary/pong messages back. But the server prints what it receives.

    // Wait a bit for server processing
    Thread.sleep(500)

    // Send close frame (opcode 0x8)
    println("[client] Sending close...")
    webSocket.sendClose(WebSocket.NORMAL_CLOSURE, "Test complete").get(5, TimeUnit.SECONDS)

    // Wait for messages with timeout
    val success = latch.await(10, TimeUnit.SECONDS)

    println("\n[client] --- Test Summary ---")
    println(s"[client] Received messages: ${receivedMessages.mkString(", ")}")
    println(s"[client] Test ${if success then "PASSED" else "TIMED OUT"}")

    // Cleanup
    webSocket.abort()

    // Note: The server runs in an infinite loop, so we can't cleanly shut it down
    // In a real test, you'd want to refactor SimpleWebsocket to be stoppable
    println("\n[client] Test complete. Server is still running (infinite loop).")
    println("[client] Press Ctrl+C to exit.")
  }
}
