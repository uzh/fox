import sbt._
import Keys._

object GraphsBuild extends Build {
  lazy val scCore = ProjectRef(file("../signal-collect"), id = "signal-collect")
  val fox = Project(id = "fox",
    base = file(".")) dependsOn (scCore)
}
