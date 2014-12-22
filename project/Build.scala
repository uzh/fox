import sbt._
import Keys._

object FoxBuild extends Build {
  val fox = Project(id = "fox", base = file("."))
}
