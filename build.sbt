import Settings.{javaOutputVersion, scala3defaults, scala3defaultsExtra}
import org.scalajs.linker.interface.{ESVersion, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode}

lazy val bismuth = project.in(file(".")).settings(scala3defaultsExtra).aggregate(
  channels.js,
  channels.jvm,
  proBench,
  rdts.js,
  rdts.jvm,
  rdts.native,
  reactives.js,
  reactives.jvm,
  reactives.native,
)

lazy val channels = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts)
  .settings(
    Settings.scala3defaultsExtra,
    Dependencies.slips,
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.jsoniterScala,
    SettingsLocal.publishSonatype,
  )
  .jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags(),
  )
  .jvmSettings(
    Test / fork := true,
  )

lazy val proBench = project.in(file("Modules/Protocol Benchmarks"))
  .dependsOn(reactives.jvm, rdts.jvm, channels.jvm, rdts.jvm % "compile->compile;test->test")
  .enablePlugins(JavaAppPackaging)
  .settings(
    scala3defaultsExtra,
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.slips,
    Dependencies.jetcd,
    Dependencies.pprint,
    Dependencies.ycsb,
    Universal / packageName := "probench",
    Universal / name        := "probench",
  )

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaultsExtra,
    SettingsLocal.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaultsExtra,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    SettingsLocal.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jvmSettings(
    libraryDependencies += Dependencies.scalafx % Provided,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    Dependencies.scalatags(Test),
    Settings.jsEnvDom,
  )
