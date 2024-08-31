import sbt._
import Keys._

lazy val all = project
  .in(file("."))
  .settings(
    Seq(
      name               := "Graph for Scala 2",
      version            := Version.highest,
      publishTo          := None,
      crossScalaVersions := Nil
    )
  )
  .aggregate(core.jvm, dot.jvm, json)

// to publish as JS do "project coreJS", "fastOptJS", "package", "publishSigned"

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    defaultSettings_2_13 ++ Seq(
      name    := "Graph Core",
      version := Version.core,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.18.0"
      )
    )
  )

/*
lazy val gen = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("gen"))
  .dependsOn(core)
  .settings(
    defaultSettings_3 ++ Seq(
      name    := "Graph Gen",
      version := Version.gen
    )
  )
 */

lazy val coreTestScala3 = project
  .in(file("coreTestScala3"))
  .dependsOn(core.jvm)
  .settings(
    defaultSettings_3 ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test exclude (
          "org.scalacheck",
          "scalacheck_3"
        )
      )
    )
  )

lazy val dot = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings_2_13 ++ Seq(
      name    := "Graph DOT",
      version := Version.dot
    )
  )

lazy val json = project
  .in(file("json"))
  .dependsOn(core.jvm)
  .settings(
    defaultSettings_2_13 ++ Seq(
      name                                 := "Graph JSON",
      version                              := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.5.0"
    )
  )

/*
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

val unusedImports = "-Wunused:imports"

lazy val defaultSettings_2_13 = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := Version.compiler_2_13,
  scalacOptions ++= Seq(
    "-Xsource:3-cross"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
  ),
  addCompilerPlugin(scalafixSemanticdb),
  semanticdbEnabled := true
) ++ defaultSettings

lazy val defaultSettings_3 = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion   := Version.compiler_3,
  scalafmtConfig := baseDirectory.value / ".." / ".scalafmt-scala3.conf"
) ++ defaultSettings

lazy val defaultSettings = Seq(
  crossScalaVersions := Seq(scalaVersion.value),
  organization       := "org.scala-graph"
) ++
  defaultCompilerSettings ++
  defaultTestSettings ++
  defaultDocSettings ++
  GraphSonatype.settings

lazy val defaultCompilerSettings = Seq(
  scalacOptions ++= Seq(
    unusedImports,
    "-Wunused:privates",
    "-deprecation",
    "-feature",
    "-language:higherKinds"
  ),
  Compile / console / scalacOptions := (Compile / scalacOptions).value filterNot (_ eq unusedImports)
)

lazy val defaultTestSettings = Seq(
  Test / parallelExecution := false,
  Test / testOptions       := Seq(Tests.Filter(s => s.endsWith("Spec")))
)

lazy val defaultDocSettings = Seq(
  Compile / doc / scalacOptions ++=
    Opts.doc.title(name.value) ++
      Opts.doc.version(version.value),
  Compile / doc / scalacOptions ++= List("-diagrams", "-implicits"),
  Compile / doc / scalacOptions ++= (baseDirectory map { d =>
    Seq("-doc-root-content", (d / "rootdoc.txt").getPath)
  }).value,
  autoAPIMappings := true
)
