import AssemblyKeys._
assemblySettings

/** Project */
name := "wolf"

version := "1.0-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.11.2"

/** See https://github.com/rwl/ParallelColt/issues/6 */
mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("edu", "emory", "mathcs", "utils", xs @ _*) => MergeStrategy.first
    case other => old(other)
  }
}

scalacOptions ++= Seq("-optimize", "-Ydelambdafy:inline", "-Yclosure-elim", "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-feature", "-deprecation", "-Xelide-below", "INFO")

parallelExecution in Test := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

test in assembly := {}

parallelExecution in Test := false

resolvers += "PSL third party dependencies" at "https://scm.umiacs.umd.edu/maven/lccd/content/repositories/psl-thirdparty/"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.2"  % "compile",
  "commons-lang" %  "commons-lang" % "2.6" % "compile",
  "edu.emory.mathcs" % "parallelcolt" % "0.9.4" % "compile",
  "edu.emory.mathcs" % "jplasma" % "1.2" % "compile",
  "net.sourceforge.jtransforms" % "jtransforms" % "2.4.0" % "compile",
  "org.scalanlp" %% "breeze" % "0.9" % "compile",
  "org.scalanlp" %% "breeze-natives" % "0.9" % "compile",
  "org.slf4j" % "slf4j-api" % "1.7.7" % "compile",
  "org.slf4j" % "slf4j-simple" % "1.7.7" % "compile",
  "junit" % "junit" % "4.8.2"  % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
