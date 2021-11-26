import sbt._
import Keys._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// project coreJS, fastOptJS, package, publishSigned

lazy val all = project
  .in(file("."))
  .aggregate(coreJVM, coreJS, constrainedJVM, constrainedJS, dotJVM, dotJS, json)
  .settings(
    name := "Graph for Scala",
    version := Version.highest,
    crossPaths := true,
    publishTo := None,
    crossScalaVersions := Nil
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("core"))
  .settings(
    defaultSettings ++ Seq(
      name := "Graph Core",
      version := Version.core,
      crossPaths := true,
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "optional;provided",
      libraryDependencies ++= {
        val netbeansModulesOrg = "org.netbeans.modules"
        val netbeansApiOrg     = "org.netbeans.api"
        val release            = "RELEASE123"

        def netbeansModule(module: String) = netbeansModulesOrg % module % release
        def netbeansApi(module: String)    = netbeansApiOrg     % module % release
        Seq(
          "org.gephi" % "gephi-toolkit" % "0.9.2" % "test" classifier "all" excludeAll (
            ExclusionRule(organization = netbeansModulesOrg),
            ExclusionRule(organization = netbeansApiOrg)
          ),
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
  .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("dot"))
  .dependsOn(core)
  .settings(
    defaultSettings ++ Seq(name := "Graph DOT", version := Version.dot)
  )

lazy val json = project
  .in(file("json"))
  .dependsOn(coreJVM)
  .settings(
    defaultSettings ++ Seq(
      name := "Graph JSON",
      version := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.5.0"
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
      version := "unpublished"
    )
  )

ThisBuild / resolvers ++= Seq(
  ("NetBeans" at "http://bits.netbeans.org/nexus/content/groups/netbeans/").withAllowInsecureProtocol(true),
  "gephi-thirdparty" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

ThisBuild / scalafmtConfig := file(".scalafmt.conf")

val unusedImports = "-Ywarn-unused:imports"
lazy val defaultSettings = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := Version.compiler_2_13,
  crossScalaVersions := Seq(Version.compiler_2_12, scalaVersion.value),
  organization := "org.scala-graph",
  scalacOptions ++= Seq(
    unusedImports,
    "-Yrangepos",
    "-Ywarn-unused:privates"
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
  autoAPIMappings := true,
  Test / testOptions := Seq(Tests.Filter(s => s.endsWith("Test"))),
  libraryDependencies ++= Seq(
    "org.scalatest"     %% "scalatest"       % "3.2.10"   % "test",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"
  )
) ++ GraphSonatype.settings
