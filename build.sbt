import sbt._
import Keys._

lazy val all = Project(
  id = "Graph-all",
  base = file("."),
  settings = Seq(
    name := "Graph for Scala",
    version := Version.highest,
    publishTo := None
  ),
  aggregate = Seq(core, constrained, dot, json)
)

lazy val core = Project(
  id = "Graph-core",
  base = file("core"),
  settings = defaultSettings ++ Seq(
    name := "Graph Core",
    version := Version.core,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "optional;provided"
  )
)

lazy val constrained = Project(
  id = "Graph-constrained",
  base = file("constrained"),
  settings = defaultSettings ++ Seq(
    name := "Graph Constrained",
    version := Version.constrained
  )
) dependsOn (core % "compile->compile;test->test")

lazy val dot = Project(
  id = "Graph-dot",
  base = file("dot"),
  settings = defaultSettings ++ Seq(
    name := "Graph DOT",
    version := Version.dot
  )
) dependsOn core

lazy val json = Project(
  id = "Graph-json",
  base = file("json"),
  settings = defaultSettings ++ Seq(
    name := "Graph JSON",
    version := Version.json,
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.0.1"
  )
) dependsOn core

lazy val misc = Project(
  id = "Graph-misc",
  base = file("misc"),
  settings = defaultSettings ++ Seq(
    name := "Graph Miscellaneous",
    version := Version.misc
  )
) dependsOn core

resolvers in ThisBuild ++= Seq(
  "NetBeans" at "http://bits.netbeans.org/nexus/content/groups/netbeans/",
  "gephi-thirdparty" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

lazy val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := Version.compiler_2_12,
  crossScalaVersions := Seq(scalaVersion.value, Version.compiler_2_11),
  organization := "org.scala-graph",
  parallelExecution in Test := false,
  scalacOptions in(Compile, doc) ++=
    Opts.doc.title(name.value) ++
      Opts.doc.version(version.value),
  // prevents sbteclipse from including java source directories
  unmanagedSourceDirectories in Compile := (scalaSource in Compile) (Seq(_)).value,
  unmanagedSourceDirectories in Test := (scalaSource in Test) (Seq(_)).value,
  scalacOptions in(Compile, doc) ++= List("-diagrams", "-implicits"),
  scalacOptions in(Compile, doc) ++= (baseDirectory map { d =>
    Seq("-doc-root-content", d / "rootdoc.txt" getPath)
  }).value,
  autoAPIMappings := true,
  testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.12" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5" % "test",
    "org.gephi" % "gephi-toolkit" % "0.9.2" % "test" classifier "all"
  )
) ++ GraphSonatype.settings

