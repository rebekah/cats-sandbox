name := "cats-sandbox"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

// scalac options come from the sbt-tpolecat plugin so need to set any here
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

//for tests
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
