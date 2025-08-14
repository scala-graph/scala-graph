ThisBuild / organization         := "org.scala-graph"
ThisBuild / organizationName     := "Graph for Scala"
ThisBuild / organizationHomepage := Some(url("http://scala-graph.org"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/scala-graph/scala-graph"),
    "scm:git:git@github.com:scala-graph/scala-graph.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "peter",
    name = "Peter Empen",
    email = "",
    url = url("http://scala-graph.org")
  )
)

ThisBuild / licenses := List(
  "Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / homepage := Some(url("http://scala-graph.org"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle    := true

ThisBuild / publishTo := {
  val sonatype = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at sonatype)
  else localStaging.value
}
