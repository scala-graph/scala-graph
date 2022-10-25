import sbt._
import Keys._

lazy val all = project
  .in(file("."))
  .settings(
    Seq(
      name      := "Graph for Scala 2",
      version   := Version.highest,
      publishTo := None
    )
  )
  .aggregate(core /*, constrained, dot, json*/ )

lazy val core = project
  .in(file("core"))
  .settings(
    defaultSettings ++ Seq(
      name    := "Graph Core",
      version := Version.core,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.15.4"
      )
    )
  )

/*
lazy val constrained = project
  .in(file("constrained"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Constrained",
      version := Version.constrained
    )
  )

lazy val dot = project
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph DOT",
      version := Version.dot
    )
  )

lazy val json = project
  .in(file("json"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph JSON",
      version := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.0"
    )
  )

lazy val misc = project
  .in(file("misc"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Miscellaneous",
      version := "unpublished"
    )
  )
 */

val unusedImports = "-Ywarn-unused:imports"
lazy val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion       := Version.compiler_2_13,
  crossScalaVersions := Seq(scalaVersion.value),
  organization       := "org.scala-graph",
  scalacOptions ++= Seq(
    unusedImports,
    "-Yrangepos",
    "-Ywarn-unused:privates",
    "-deprecation",
    "-feature",
    "-language:higherKinds"
  ),
  Compile / console / scalacOptions := (Compile / scalacOptions).value filterNot (_ eq unusedImports),
  addCompilerPlugin(scalafixSemanticdb),
  Test / parallelExecution := false,
  Compile / doc / scalacOptions ++=
    Opts.doc.title(name.value) ++
      Opts.doc.version(version.value),
  Compile / doc / scalacOptions ++= List("-diagrams", "-implicits"),
  Compile / doc / scalacOptions ++= (baseDirectory map { d =>
    Seq("-doc-root-content", (d / "rootdoc.txt").getPath)
  }).value,
  autoAPIMappings    := true,
  Test / testOptions := Seq(Tests.Filter(s => s.endsWith("Spec"))),
  libraryDependencies ++= Seq(
    "org.scalatest"     %% "scalatest"       % "3.2.13"   % "test",
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % "test"
  )
) ++ GraphSonatype.settings
