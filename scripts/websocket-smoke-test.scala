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
      "examplesJVM/runMain ex2026overlaydemo.OverlayDemo"
    )
      .directory(root.toFile)
      .redirectErrorStream(true)
      .start()

    try {
      val serverReader = BufferedReader(InputStreamReader(serverProcess.getInputStream))
      val seed         = readSeedConnectionString(serverReader)
      println(s"[orchestrator] detected seed connection string: $seed")

      val modulePath = root.resolve("Modules/Examples Web/target/generated_js/examplesweb-fastopt/main.js").toUri.toString
      val nodeCode =
        s"""
           |const mod = await import(${escapeJs(modulePath)});
           |const seed = ${escapeJs(seed)};
           |const [a, b] = await Promise.all([
           |  mod.RunOverlayNetworkObserver(seed, 'observer-a', 'demo-topic'),
           |  mod.RunOverlayNetworkObserver(seed, 'observer-b', 'demo-topic')
           |]);
           |console.log('[node-client-a] ' + a);
           |console.log('[node-client-b] ' + b);
           |if (!a.includes('connected url=') || !b.includes('connected url=')) {
           |  throw new Error('expected both observers to connect using the seed connection string');
           |}
           |if (!a.includes('snapshot=') || !b.includes('snapshot=')) {
           |  throw new Error('expected both observers to learn replicated topology from the overlay');
           |}
           |process.exit(0);
           |""".stripMargin

      runChecked(root, Seq("node", "--input-type=module", "--eval", nodeCode), "running parallel overlay observers")
      println("[orchestrator] websocket smoke test passed")
    } finally {
      serverProcess.destroy()
      if serverProcess.isAlive then {
        Thread.sleep(250)
        serverProcess.destroyForcibly()
      }
    }
  }

  private def readSeedConnectionString(reader: BufferedReader): String = {
    val deadline = System.nanoTime() + 30L * 1000L * 1000L * 1000L
    while System.nanoTime() < deadline do {
      val line = reader.readLine()
      if line == null then throw IllegalStateException("server terminated before announcing its seed connection string")
      println(s"[server] $line")
      val normalized = line.replaceAll("\\u001B\\[[;\\d]*[A-Za-z]", "")
      val marker     = "seed="
      val idx        = normalized.indexOf(marker)
      if idx >= 0 then return normalized.drop(idx + marker.length).trim
    }
    throw IllegalStateException("timed out waiting for overlay seed connection string")
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
