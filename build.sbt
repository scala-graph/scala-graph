import sbt._
import Keys._

lazy val all = project
  .in(file("."))
  .settings(
    Seq(
      name := "Graph for Scala",
      version := Version.highest,
      publishTo := None
    )
  )
  .aggregate(core, constrained, dot, json)

lazy val core = project
  .in(file("core"))
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Core",
      version := Version.core,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck"   % "1.13.4" % "optional;provided",
        "org.gephi"      % "gephi-toolkit" % "0.9.2"  % "test" classifier "all",
      ),
      dependencyOverrides ++= {
        val release                        = "RELEASE90"
        def netbeansModule(module: String) = "org.netbeans.modules" % module % release % "test"
        def netbeansApi(module: String)    = "org.netbeans.api" % module % release % "test"
        Seq(
          netbeansModule("org-netbeans-core"),
          netbeansModule("org-netbeans-core-startup-base"),
          netbeansModule("org-netbeans-modules-masterfs"),
          netbeansApi("org-openide-util-lookup"),
          netbeansApi("org-openide-filesystems"),
          netbeansApi("org-openide-util-ui"),
          netbeansApi("org-openide-dialogs"),
          netbeansApi("org-openide-nodes"),
          netbeansApi("org-netbeans-api-annotations-common")
        )
      }
    )
  )

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
    defaultSettings ++ Seq(name := "Graph DOT", version := Version.dot)
  )

lazy val json = project
  .in(file("json"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph JSON",
      version := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.1"
    )
  )

lazy val misc = project
  .in(file("misc"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Miscellaneous",
      version := Version.misc
    )
  )

ThisBuild / resolvers ++= Seq(
  "gephi-thirdparty" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

ThisBuild / scalafmtConfig := Some(file(".scalafmt.conf"))

lazy val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := Version.compiler_2_12,
  crossScalaVersions := Seq(scalaVersion.value, Version.compiler_2_11),
  organization := "org.scala-graph",
  scalacOptions ++= Seq(
    "-Ywarn-unused:imports",
    "-Yrangepos"
  ),
  Compile / scalacOptions in compile += "-Ywarn-unused:privates",
  addCompilerPlugin(scalafixSemanticdb),
  Test / parallelExecution := false,
  Compile / doc / scalacOptions ++=
    Opts.doc.title(name.value) ++
      Opts.doc.version(version.value),
  // prevents sbteclipse from including java source directories
  Compile / unmanagedSourceDirectories := (Compile / scalaSource)(Seq(_)).value,
  Test / unmanagedSourceDirectories := (Test / scalaSource)(Seq(_)).value,
  Compile / doc / scalacOptions ++= List("-diagrams", "-implicits"),
  Compile / doc / scalacOptions ++= (baseDirectory map { d =>
    Seq("-doc-root-content", (d / "rootdoc.txt").getPath)
  }).value,
  autoAPIMappings := true,
  Test / testOptions := Seq(Tests.Filter(s => s.endsWith("Test"))),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
) ++ GraphSonatype.settings
