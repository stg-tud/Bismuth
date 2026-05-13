package de.tu_darmstadt.informatik.st.reform

import java.nio.file.Path
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

given ExecutionContext =
  ExecutionContext.fromExecutor(Executors.newSingleThreadScheduledExecutor().nn)

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

object Env {

  def loadEnv(envPath: Path): Map[String, String] = {
    if Files.exists(envPath) && Files.isRegularFile(envPath) then {
      val lines = Files.readAllLines(envPath).asScala
      lines.flatMap { line =>
        val trimmed = line.strip()
        if trimmed.nonEmpty && !trimmed.startsWith("#") then { // Ignore empty lines and comments
          val Array(key, value) = trimmed.split("=", 2).map(_.trim)
          Some((key, value))
        } else None
      }.toMap
    } else {
      throw new IllegalStateException(s".env file not found at path: ${envPath.toAbsolutePath.normalize()}")
    }
  }

  private def findEnvPath(currentPath: Path): Option[Path] = {
    val envGuess     = currentPath.resolve(".env")
    val exampleGuess = currentPath.resolve("env.example")
    if Files.isRegularFile(envGuess)
    then Some(envGuess)
    else if Files.isRegularFile(exampleGuess)
    then Some(exampleGuess)
    else Option(currentPath.getParent).flatMap(findEnvPath)
  }

  lazy val env: Map[String, String] =
    findEnvPath(Path.of("").toAbsolutePath)
      .map(loadEnv)
      .getOrElse(
        throw new IllegalStateException(
          s".env or env.example file not found in directory tree starting at ${Path.of("").toAbsolutePath.nn}",
        ),
      )

  def get(name: String): String = {
    val opt = env.get(name)
    if opt.isEmpty then {
      throw new IllegalStateException(s"Environment variable ${name} must be set. (Did you source .env)?")
    }
    opt.get
  }
}

object Globals {

  val VITE_DATABASE_VERSION: String = Env.get("VITE_DATABASE_VERSION")

  val VITE_PROTOCOL_VERSION: String = Env.get("VITE_PROTOCOL_VERSION")

  val ALWAYS_ONLINE_PEER_DATABASE_PATH: String = Env.get("ALWAYS_ONLINE_PEER_DATABASE_PATH")

  val VITE_ALWAYS_ONLINE_PEER_PATH: String = Env.get("VITE_ALWAYS_ONLINE_PEER_PATH")

  val VITE_ALWAYS_ONLINE_PEER_LISTEN_PORT: Int = Env.get("VITE_ALWAYS_ONLINE_PEER_LISTEN_PORT").toInt

  val JWT_KEY: String = Env.get("JWT_KEY")
}
