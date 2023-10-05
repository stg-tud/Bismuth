ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.ckuessner"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .disablePlugins(JmhPlugin, AssemblyPlugin)
  .platformsEnablePlugins(JVMPlatform)(JacocoPlugin)
  .settings(
    name := "interval-tree-clocks",
    scalacOptions ++= Seq("-explain", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalatest"     %%% "scalatest"          % "3.2.17"   % "test",
      "org.scalatest"     %%% "scalatest-flatspec" % "3.2.17"   % "test",
      "org.scalatestplus" %%% "scalacheck-1-17"    % "3.2.17.0" % "test",
      "org.scalacheck"    %%% "scalacheck"         % "1.17.0"   % "test"
    )
  )
  .jvmSettings(
    test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-S", "4387684008756621752"),
    test / jacocoReportSettings := JacocoReportSettings(
      "Jacoco Coverage Report",
      None,
      JacocoThresholds(),
      Seq(JacocoReportFormats.ScalaHTML),
      "utf-8"
    )
  )

lazy val benchmarks = (project in file("benchmarks"))
  .enablePlugins(JmhPlugin, AssemblyPlugin)
  .settings(
    name := "benchmarks",
    scalacOptions ++= Seq("-explain", "-feature"),
    assembly                   := (assembly dependsOn (Jmh / compile)).value,
    assembly / mainClass       := Some("org.openjdk.jmh.Main"),
    assembly / assemblyJarName := "benchmarks.jar"
  )
  .dependsOn(root.jvm)
