import sbt._
import sbt.Keys._

object OthellogeneticevolutionBuild extends Build {

  lazy val othellogeneticevolution = Project(
    id = "othello-genetic-evolution",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "othello-genetic-evolution",
      organization := "kb",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies := Seq(
        "com.typesafe.akka" % "akka-actor" % "2.0.1",
        "org.scalatest" %% "scalatest" % "1.8" % "test"
      ),
	    initialCommands in console := "import kb._;import Console._;"
    )
  )


}
