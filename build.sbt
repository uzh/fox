import AssemblyKeys._
assemblySettings

/** Project */
name := "fox"

version := "1.0.0"

organization := "com.signalcollect"

scalaVersion := "2.11.4"

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

/** 
 * See https://github.com/rwl/ParallelColt/issues/6 and 
 * https://github.com/sbt/sbt-assembly/issues/123
 */
mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("edu", "emory", "mathcs", "utils", xs @ _*) => MergeStrategy.first
    case PathList(ps @ _*) if ps.last == ".DS_Store" => MergeStrategy.discard
    case other => old(other)
  }
}

scalacOptions ++= Seq("-optimize", "-Ydelambdafy:inline", "-Yclosure-elim", "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-feature", "-deprecation", "-Xelide-below", "INFO")

assembleArtifact in packageScala := true

parallelExecution in Test := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

jarName in assembly := "fox.jar"

test in assembly := {}

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value  % "compile",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile",
  "com.signalcollect" %% "signal-collect" % "3.0.0" % "compile",
  "commons-lang" %  "commons-lang" % "2.6" % "compile",
  "edu.emory.mathcs" % "parallelcolt" % "0.9.4" % "compile",
  "edu.emory.mathcs" % "jplasma" % "1.2" % "compile",
  "net.sourceforge.jtransforms" % "jtransforms" % "2.4.0" % "compile",
  "org.scalanlp" %% "breeze" % "0.10" % "compile",
  "org.scalanlp" %% "breeze-natives" % "0.10" % "compile",
  "org.slf4j" % "slf4j-api" % "1.7.9" % "compile",
  "org.slf4j" % "slf4j-simple" % "1.7.9" % "compile",
  "junit" % "junit" % "4.12"  % "test",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test"
)

resolvers += "Ifi Public" at "https://maven.ifi.uzh.ch/maven2/content/groups/public/"

resolvers += "PSL third party dependencies" at "https://scm.umiacs.umd.edu/maven/lccd/content/repositories/psl-thirdparty/"

seq(bintraySettings:_*)

pomExtra := (
 <url>https://github.com/uzh/fox</url>
 <scm>
   <url>git@github.com:uzh/fox.git</url>
   <connection>scm:git:git@github.com:uzh/fox.git</connection>
 </scm>
 <developers>
   <developer>
     <id>saramagliacane</id>
     <name>Sara Magliacane</name>
     <url>https://github.com/saramagliacane</url>
   </developer>
   <developer>
     <id>pstutz</id>
     <name>Philip Stutz</name>
     <url>https://github.com/pstutz</url>
   </developer>
 </developers>)
