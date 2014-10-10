import sbt._
import Keys._

object GraphsBuild extends Build {
  lazy val scCore = ProjectRef(file("../signal-collect"), id = "signal-collect")
  val wolf = Project(id = "wolf",
    base = file(".")) dependsOn (scCore)
}
