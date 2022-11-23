ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "interval-tree-clocks",
    idePackagePrefix := Some("de.tu_darmstadt.stg.daimpl"),
    scalacOptions ++= Seq("-explain", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.14" % "test",
      "org.scalatest" %% "scalatest-flatspec" % "3.2.14" % "test",
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test"
    )
  )
