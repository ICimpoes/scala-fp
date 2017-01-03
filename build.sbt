name := "scala-fp"

version := "1.0"

scalaVersion := "2.12.1"

parallelExecution in Test := false

val scalaTestVersion = "3.0.0"

val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies ++= Seq(scalaTest, scalaCheck)