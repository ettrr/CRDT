ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "CRDT"
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.2.19",
)
