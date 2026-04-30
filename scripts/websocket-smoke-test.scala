#!/usr/bin/env scala-cli
//> using scala 3.8.3

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.Path

object WebsocketSmokeTest {

  def main(args: Array[String]): Unit = {
    val root = {
      val cwd = Path.of("").toAbsolutePath.normalize()
      if cwd.getFileName.toString == "scripts" then cwd.getParent else cwd
    }

    runChecked(root.resolve("Modules/Examples Web"), Seq("npm", "install"), "installing Examples Web node dependencies")
    runChecked(root, Seq("sbt", "examplesWeb/fastLinkJS"), "building Examples Web")

    val serverProcess = new java.lang.ProcessBuilder(
      "sbt",
      "examplesJVM/runMain ex2026niowebsocket.NioWebsocketEchoServer 0"
    )
      .directory(root.toFile)
      .redirectErrorStream(true)
      .start()

    try {
      val serverReader = BufferedReader(InputStreamReader(serverProcess.getInputStream))
      val port         = readServerPort(serverReader)
      println(s"[orchestrator] detected server port: $port")

      val modulePath = root.resolve("Modules/Examples Web/target/generated_js/examplesweb-fastopt/main.js").toUri.toString
      val nodeCode =
        s"""
           |const mod = await import(${escapeJs(modulePath)});
           |const result = await mod.RunNioWebsocketClient(${escapeJs(s"ws://127.0.0.1:$port")});
           |console.log('[node-client] echoed=' + result);
           |if (result !== 'hello-from-node-1,hello-from-node-2') {
           |  throw new Error('unexpected echoed payloads: ' + result);
           |}
           |""".stripMargin

      runChecked(root, Seq("node", "--input-type=module", "--eval", nodeCode), "running node websocket client")
      println("[orchestrator] websocket smoke test passed")
    } finally {
      serverProcess.destroy()
      if serverProcess.isAlive then {
        Thread.sleep(250)
        serverProcess.destroyForcibly()
      }
    }
  }

  private def readServerPort(reader: BufferedReader): Int = {
    val deadline = System.nanoTime() + 30L * 1000L * 1000L * 1000L
    while System.nanoTime() < deadline do {
      val line = reader.readLine()
      if line == null then throw IllegalStateException("server terminated before announcing its port")
      println(s"[server] $line")
      val normalized = line.replaceAll("\\u001B\\[[;\\d]*[A-Za-z]", "")
      val marker     = "WS_SERVER_PORT="
      val idx        = normalized.indexOf(marker)
      if idx >= 0 then {
        val digits = normalized.drop(idx + marker.length).dropWhile(!_.isDigit).takeWhile(_.isDigit)
        return digits.toInt
      }
    }
    throw IllegalStateException("timed out waiting for websocket server port")
  }

  private def runChecked(root: Path, command: Seq[String], label: String): Unit = {
    println(s"[orchestrator] $label: ${command.mkString(" ")}")
    val process = new java.lang.ProcessBuilder(command*)
      .directory(root.toFile)
      .inheritIO()
      .start()
    val exit = process.waitFor()
    if exit != 0 then throw IllegalStateException(s"command failed with exit code $exit: ${command.mkString(" ")}")
  }

  private def escapeJs(str: String): String =
    '"' + str.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    } + '"'
}
