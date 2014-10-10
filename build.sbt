import AssemblyKeys._
assemblySettings

/** Project */
name := "wolf"

version := "1.0-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-optimize", "-Ydelambdafy:inline", "-Yclosure-elim", "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-feature", "-deprecation", "-Xelide-below", "INFO")

parallelExecution in Test := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

test in assembly := {}

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.2"  % "compile",
  "commons-lang" %  "commons-lang" % "2.6" % "compile",
  "edu.emory.mathcs" % "parallelcolt" % "0.9.4" % "compile",
  "edu.emory.mathcs" % "jplasma" % "1.2" % "compile",
  "net.sourceforge.jtransforms" % "jtransforms" % "2.4.0" % "compile",
  "org.scalanlp" %% "breeze" % "0.9" % "compile",
  "org.scalanlp" %% "breeze-natives" % "0.9" % "compile",
  "junit" % "junit" % "4.8.2"  % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
