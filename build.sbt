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
  .aggregate(core.jvm, dot.jvm, json.jvm)

// to publish as JS run "project coreJS", "fastOptJS", "package", "publishSigned"

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    defaultSettings_cross ++ Seq(
      name    := "Graph Core",
      version := Version.core,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.18.1"
      )
    )
  )

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

lazy val dot = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings_cross ++ Seq(
      name    := "Graph DOT",
      version := Version.dot
    )
  )

lazy val json = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("json"))
  .dependsOn(core)
  .settings(
    defaultSettings_2 ++ Seq(
      name                                 := "Graph JSON",
      version                              := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.5.0" // not available for Scala 3
    )
  )

val unusedImports = "-Wunused:imports"

lazy val defaultSettings_cross = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion       := Version.compiler_2_13,
  crossScalaVersions := Seq(Version.compiler_2_13, Version.compiler_3_fallback)
) ++
  defaultSettings ++
  defaultTestLibSettings

lazy val defaultSettings_2 = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := Version.compiler_2_13
) ++
  defaultSettings ++
  defaultTestLibSettings

lazy val defaultTestLibSettings =
  libraryDependencies ++= Seq(
    "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
  )

lazy val defaultSettings_3 = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion   := Version.compiler_3,
  scalafmtConfig := baseDirectory.value / ".." / ".scalafmt-scala3.conf"
) ++ defaultSettings

lazy val defaultSettings = Seq(
  organization := "org.scala-graph",
  libraryDependencies ++= dependingOn(scalaVersion.value)(
    if_2 = Seq(compilerPlugin(scalafixSemanticdb)),
    if_3 = Nil
  ),
  semanticdbEnabled := dependingOn(scalaVersion.value)(
    if_2 = true,
    if_3 = false
  )
) ++
  defaultCompilerSettings ++
  defaultTestSettings ++
  defaultDocSettings ++
  GraphSonatype.settings

lazy val defaultCompilerSettings = Seq(
  scalacOptions ++= dependingOn(scalaVersion.value)(
    if_2 = Seq(
      "-Xsource:3-cross",
      "-Xsource-features:case-apply-copy-access"
    ),
    if_3 = Nil
  ),
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
  Compile / doc / scalacOptions ++= dependingOn(scalaVersion.value)(
    if_2 = List("-diagrams", "-implicits"),
    if_3 = Nil
  ),
  Compile / doc / scalacOptions ++= (baseDirectory map { d =>
    Seq("-doc-root-content", (d / "rootdoc.txt").getPath)
  }).value,
  autoAPIMappings := true
)

def dependingOn[A](version: String)(if_2: => A, if_3: => A): A =
  CrossVersion.partialVersion(version) match {
    case Some((2, _)) => if_2
    case Some((3, _)) => if_3
    case invalid      => sys.error(s"Invalid Scala version '$invalid'. Major must be one of 2 or 3.")
  }
