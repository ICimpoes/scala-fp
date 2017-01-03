name := "scala-fp"

version := "1.0"

scalaVersion := "2.12.1"

val scalaTestVersion = "3.0.0"

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

parallelExecution in Test := false

libraryDependencies += scalaTest