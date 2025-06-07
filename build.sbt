ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"

lazy val lib = (project in file("lib"))
  .settings(
    name := "daimpl-2025-tabular-rdt-lib",
    libraryDependencies ++=Seq(
      "de.tu-darmstadt.stg" %% "rdts" % "0.37.0",
      "de.tu-darmstadt.stg" %% "replication" % "0.37.0",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )

lazy val app = (project in file("app"))
  .dependsOn(lib)
  .settings(
    name := "daimpl-2025-tabular-rdt-app"
  )

lazy val root = (project in file("."))
  .aggregate(lib, app)
  .settings(
    name := "daimpl-2025-tabular-rdt"
  )
