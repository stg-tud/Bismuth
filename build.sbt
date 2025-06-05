ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "daimpl-2025-tabular-rdt",
    libraryDependencies += "de.tu-darmstadt.stg" %% "rescala" % "0.35.1"
  )
