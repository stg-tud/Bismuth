ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"

lazy val lib = (project in file("lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "daimpl-2025-tabular-rdt-lib",
    libraryDependencies ++=Seq(
      "de.tu-darmstadt.stg" %%% "rdts" % "0.37.0",
      "de.tu-darmstadt.stg" %%% "replication" % "0.37.0"
    )
  )

lazy val app = (project in file("app"))
  .dependsOn(lib)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "daimpl-2025-tabular-rdt-app",
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core" % "2.1.1",
      "com.github.japgolly.scalajs-react" %%% "extra" % "2.1.1",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    ),
    scalaJSUseMainModuleInitializer := true,
    Compile / fastOptJS / crossTarget := baseDirectory.value,
    Compile / fullOptJS / crossTarget := baseDirectory.value
  )

lazy val root = (project in file("."))
  .aggregate(lib, app)
  .settings(
    name := "daimpl-2025-tabular-rdt"
  )
