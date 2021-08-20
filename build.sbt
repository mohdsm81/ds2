

organization := "edu.utah.cs.gauss"

name := "ds2"

version := "0.1.0-beta"

scalaVersion := "2.11.12"

scalaVersion in ThisBuild := "2.11.12"

mainClass in Compile := Some("edu.utah.cs.gauss.ds2.core.frontend.akka.FrontEnd")

//test in assembly := {}

//scaladoc
scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

// testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
//fork in Test := true // trying to make ensime debugging support to work.
//Compile / run / fork := true
//Test / run / fork := true
//Test / fork := true

// Dependencies
// libraryDependencies += "net.liftweb" % "lift-json_2.11" % "3.0-M6"

// Scala.meta
libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.24"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"

// graph for scala
libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.11" % "1.9.4"

//json (net.liftweb.json)
libraryDependencies += "com.assembla.scala-incubator" % "graph-json_2.11" % "1.9.2"

// dot (graph4s to dot)
libraryDependencies += "com.assembla.scala-incubator" % "graph-dot_2.11" % "1.10.0"

libraryDependencies += "org.ow2.asm" % "asm-all" % "5.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5"

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.12.4"

// Use this neat library to parse command line options, best library I used
libraryDependencies += "commons-cli" % "commons-cli" % "1.3.1"

// Bokeh (charting from scala, binding from python's bokeh library)
libraryDependencies += "io.continuum.bokeh" %% "bokeh" % "0.6"

// scala-pickling (boiler-plate-free pickling)
libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.11.0-M2"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.3.12",
  "com.typesafe.akka" %% "akka-remote" % "2.3.12")

