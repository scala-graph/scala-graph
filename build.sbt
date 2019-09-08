import sbt._
import Keys._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val all = project
  .in(file("."))
  .dependsOn(coreJVM, coreJS, constrainedJVM, constrainedJS, dotJVM, dotJS, jsonJVM, jsonJS)
  .aggregate(coreJVM, coreJS, constrainedJVM, constrainedJS, dotJVM, dotJS, jsonJVM, jsonJS)
  .settings(
    Seq(
      name := "Graph for Scala",
      version := Version.highest,
      crossPaths := true,
      publishTo := None
    )
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CompatCrossType) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("core"))
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Core",
      version := Version.core,
      crossPaths := true,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck"   % "1.13.4" % "optional;provided",
        "org.gephi"      % "gephi-toolkit" % "0.9.2"  % "test" classifier "all",
      ),
      dependencyOverrides ++= {
        val release                        = "RELEASE90"
        def netbeansModule(module: String) = "org.netbeans.modules" % module % release
        def netbeansApi(module: String)    = "org.netbeans.api" % module % release
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

lazy val constrainedJVM = constrained.jvm
lazy val constrainedJS = constrained.js
lazy val constrained = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("constrained"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Constrained",
      version := Version.constrained
    )
  )

lazy val dotJVM = dot.jvm
lazy val dotJS = dot.js
lazy val dot = crossProject(JSPlatform, JVMPlatform)
  .crossType(CompatCrossType) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(name := "Graph DOT", version := Version.dot)
  )

lazy val jsonJVM = json.jvm
lazy val jsonJS = json.js
lazy val json = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("json"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph JSON",
      version := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.1"
    )
  )

lazy val miscJVM = misc.jvm
lazy val miscJS = misc.js
lazy val misc = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("misc"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Miscellaneous",
      version := Version.misc
    )
  )

ThisBuild / resolvers ++= Seq(
  "NetBeans" at "http://bits.netbeans.org/maven2/",
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
  Compile / doc / scalacOptions ++= List("-diagrams", "-implicits"),
  Compile / doc / scalacOptions ++= (baseDirectory map { d =>
    Seq("-doc-root-content", (d / "rootdoc.txt").getPath)
  }).value,
  autoAPIMappings := true,
  Test / testOptions := Seq(Tests.Filter(s => s.endsWith("Test"))),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
) ++ GraphSonatype.settings
