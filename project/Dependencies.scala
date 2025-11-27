import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.libraryDependencies

object Dependencies {

  val akka        = libraryDependencies += "com.typesafe.akka"         %% "akka-actor-typed"         % "2.8.8"
  val akkaTestKit = libraryDependencies += "com.typesafe.akka"         %% "akka-actor-testkit-typed" % "2.8.8"  % Test
  val bloomFilter = libraryDependencies += "com.github.alexandrnikitin" % "bloom-filter_2.13"        % "0.13.1"
  val catsParse   = libraryDependencies += "org.typelevel"            %%% "cats-parse"               % "1.1.0"
  val conscrypt   = libraryDependencies += "org.conscrypt"              % "conscrypt-openjdk-uber"   % "2.5.2"
  val decline     = libraryDependencies += "com.monovore"             %%% "decline"                  % "2.5.0"
  val fansi       = libraryDependencies += "com.lihaoyi"              %%% "fansi"                    % "0.5.1"
  val jetcd       = libraryDependencies += "io.etcd"                    % "jetcd-core"               % "0.8.6"
  val monocleCore = libraryDependencies += "dev.optics"               %%% "monocle-core"             % "3.3.0"
  val munit       = libraryDependencies += "org.scalameta"            %%% "munit"                    % "1.2.1"  % Test
  val munitCheck  = libraryDependencies += "org.scalameta"            %%% "munit-scalacheck"         % "1.2.0"  % Test
  val osLib       = libraryDependencies += "com.lihaoyi"              %%% "os-lib"                   % "0.11.6"
  val pprint      = libraryDependencies += "com.lihaoyi"              %%% "pprint"                   % "0.9.5"
  val scalaSwing  = libraryDependencies += "org.scala-lang.modules"    %% "scala-swing"              % "3.0.0"
  val scalaXml    = libraryDependencies += "org.scala-lang.modules"    %% "scala-xml"                % "2.4.0"
  val scalajsDom  = libraryDependencies += "org.scala-js"             %%% "scalajs-dom"              % "2.8.1"
  val slf4jSimple = libraryDependencies += "org.slf4j"                  % "slf4j-simple"             % "2.0.17" % Test
  val slf4jnop    = libraryDependencies += "org.slf4j"                  % "slf4j-nop"                % "2.0.17" % Test
  val slips       = libraryDependencies += "de.rmgk.slips"            %%% "partypack"                % "0.15.0"
  val sttpCore = libraryDependencies += "com.softwaremill.sttp.client4" %%% "core"    % "4.0.13"
  val tink     = libraryDependencies += "com.google.crypto.tink"          % "tink"    % "1.19.0"
  val upickle  = libraryDependencies += "com.lihaoyi"                   %%% "upickle" % "4.4.1"

  val ayza = libraryDependencies ++= List(
    "io.github.hakky54" % "ayza"         % "10.0.2",
    "io.github.hakky54" % "ayza-for-pem" % "10.0.2",
  )

  val bouncyCastle = libraryDependencies ++=
    List(
      // Note, jdk18 means JDK 1.8
      "org.bouncycastle" % "bcprov-jdk18on" % "1.82",
      "org.bouncycastle" % "bcpkix-jdk18on" % "1.82",
    )

  def borer = libraryDependencies ++= Seq(
    "io.bullet" %%% "borer-core"       % "1.16.2",
    "io.bullet" %%% "borer-derivation" % "1.16.2"
  )

  def jetty = {
    val jettyVersion = "12.1.4"
    libraryDependencies ++= Seq(
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
      "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
    )
  }

  def jsoniterScala =
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.38.4",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.38.4" % Provided
    )

  def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "24.0.2-R36"

  def scalajsReact = libraryDependencies ++= Seq(
    "com.github.japgolly.scalajs-react" %%% "core"  % "2.1.3",
    "com.github.japgolly.scalajs-react" %%% "extra" % "2.1.3",
  )

  def scalatags(conf: Configuration = Compile) = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % conf

  val scalatest = libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
    "org.scalatest" %%% s"scalatest-$m" % "3.2.19" % Test
  )

}
