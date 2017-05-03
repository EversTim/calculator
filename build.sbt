import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "nl.sogyo",
      scalaVersion := "2.11.11",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Calculator",
    connectInput in run := true,
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
	scalacOptions += "-feature"
  )
