name := "cl-robdd-scala"

version := "0.1"

scalaVersion := "2.13.13"

//libraryDependencies += "fr.epita.lrde" %% "clcompat" % "0.1"

libraryDependencies += "junit" % "junit" % "4.13.2" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test"
libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.jboss" % "jboss-common-core" % "2.5.0.Final"
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.1"

//libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-RC1"

libraryDependencies += "org.reflections" % "reflections" % "0.10.2"
//libraryDependencies += "org.reflections" % "reflections" % "0.9.12"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

Test / parallelExecution := false

//import sbt.Keys._
//
//lazy val root = (project in file(".")).
//  enablePlugins(JmhPlugin).
//  settings(
//    name := "scala-jmh-performance-testing",
//    version := "1.0",
//    scalaVersion := "2.13.3",
//    libraryDependencies ++= Seq(
//      "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21",
//      "commons-codec" % "commons-codec" % "1.9",
//      "com.storm-enroute" %% "scalameter" % "0.8.2"
//    ),
//    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
//    parallelExecution in Test := false
// )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
