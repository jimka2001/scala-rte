name := "cl-robdd-scala"

version := "0.1"

scalaVersion := "2.12.8"


libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.3"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.jboss" % "jboss-common-core" % "2.5.0.Final"
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"