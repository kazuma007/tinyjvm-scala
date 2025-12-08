ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "tinyjvm-scala"
  )

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm" % "9.9",
  "org.ow2.asm" % "asm-util" % "9.9",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)
