import sbt._
import Keys._

lazy val all = project
  .in(file("."))
  .settings(
    Seq(
      name               := "Graph for Scala 2",
      version            := Version.highest,
      crossScalaVersions := Nil
    )
  )
  .aggregate(core.jvm, dot.jvm, jsonLift.jvm, jsoniter.jvm)

// to publish as JS run "project coreJS", "fastOptJS", "package", "publishSigned"

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    defaultSettings_cross ++ Seq(
      name        := "Graph Core",
      description := "In-memory graph editing and algorithms with the look and feel of Scala Library collections.",
      version     := Version.core,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.19.0"
      )
    )
  )

lazy val dot = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings_cross ++ Seq(
      name        := "Graph DOT",
      description := "Configurable DOT export of Graphs provided by graph-core.",
      version     := Version.dot
    )
  )

lazy val jsonLift = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("jsonLift"))
  .dependsOn(core)
  .settings(
    defaultSettings_2 ++ Seq(
      name := "Graph lift-json",
      description := "Configurable JSON serialization and deserialization of Graphs, provided by graph-core, in terms of lift-json.",
      version                                         := Version.jsonLift,
      libraryDependencies += "net.liftweb"            %% "lift-json" % "3.5.0", // not available for Scala 3
      dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
    )
  )

lazy val jsoniter = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("jsoniter"))
  .dependsOn(core)
  .settings(
    defaultSettings_3 ++ Seq(
      name        := "Graph jsoniter",
      description := "Configurable JSON codecs for Graphs, provided by graph-core, in terms of jsoniter-scala.",
      version     := Version.jsoniter, {
        val jsoniterGroup = "com.github.plokhotnyuk.jsoniter-scala"
        libraryDependencies ++= Seq(
          jsoniterGroup %% "jsoniter-scala-core"   % "2.38.14",
          jsoniterGroup %% "jsoniter-scala-macros" % "2.38.14"
        )
      }
    )
  )

val unusedImports = "-Wunused:imports"

lazy val defaultSettings_cross = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion       := Version.compiler_2_13,
  crossScalaVersions := Seq(Version.compiler_2_13, Version.compiler_3)
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
    "org.scalatest"     %% "scalatest"       % "3.2.20"   % Test,
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
  )

lazy val defaultTestLibSettings_3 =
  libraryDependencies ++= Seq(
    "org.scalatest"     %% "scalatest"       % "3.2.20"   % Test,
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test exclude (
      "org.scalacheck",
      "scalacheck_3"
    )
  )

lazy val defaultSettings_3 = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion   := Version.compiler_3,
  scalafmtConfig := baseDirectory.value / "../.." / ".scalafmt-scala3.conf"
) ++
  defaultSettings ++
  defaultTestLibSettings_3

lazy val defaultSettings = Seq(
  versionScheme := Some("pvp"),
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
  defaultDocSettings

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
