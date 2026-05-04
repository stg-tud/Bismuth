ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.ckuessner"

ThisBuild / scalaVersion := "3.8.3"

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    name := "interval-tree-clocks",
    scalacOptions ++= Seq("-explain", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"            % "1.2.4" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.2.0" % Test,
    ),
  )
