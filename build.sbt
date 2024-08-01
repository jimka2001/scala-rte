name := "scala-rte"

version := "0.1"

scalaVersion := "2.13.14"

libraryDependencies += "junit" % "junit" % "4.13.2" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.0"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.jboss" % "jboss-common-core" % "2.5.0.Final"
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.1"

//libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-RC1"

libraryDependencies += "org.reflections" % "reflections" % "0.10.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

Test / parallelExecution := false


libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

