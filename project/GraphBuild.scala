import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object GraphBuild extends Build {

  lazy val all = Project(
    id = "Graph-all",
    base = file("."),
    settings = Seq(
      name      := "Graph for Scala",
      version   := Version.all,
      publishTo := None
	  ),
    aggregate = Seq(core, corejs, constrained, dot, json)
  )

  lazy val coreCross = crossProject.crossType(CrossType.Pure).in(file("core"))
    .settings(defaultCrossSettings:_*)
    .jvmSettings(defaultSettings:_*)
    .settings(
      name      := "Graph Core",
      version   := Version.core,
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.5"
    )

  lazy val core   = coreCross.jvm
  lazy val corejs = coreCross.js

  lazy val constrained = Project(
    id = "Graph-constrained",
    base = file("constrained"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Constrained",
      version   := Version.constrained
    )
  ) dependsOn (core % "compile->compile;test->test")

  lazy val dot = Project(
    id = "Graph-dot",
    base = file("dot"),
    settings = defaultSettings ++ Seq(
      name      := "Graph DOT",
      version   := Version.dot
    )
  ) dependsOn (core)

  lazy val json = Project(
    id = "Graph-json",
    base = file("json"),
    settings = defaultSettings ++ Seq(
      name      := "Graph JSON",
      version   := Version.json,
      libraryDependencies += "net.liftweb" %% "lift-json" % "2.6.2"
    )
  ) dependsOn (core)

  lazy val misc = Project(
    id = "Graph-misc",
    base = file("misc"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Miscellaneous",
      version   := Version.misc,
	  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.7"
    )
  ) dependsOn (core)

  private lazy val defaultCrossSettings = Seq(
    scalaVersion := Version.compiler,
    organization := "org.scala-graph"
  ) ++ GraphSonatype.settings
    
  private lazy val defaultSettings = defaultCrossSettings ++ Seq(
  	crossScalaVersions  := Seq(scalaVersion.value, Version.compiler_2),
    parallelExecution in Test := false,
    scalacOptions in (Compile, doc) <++= (name, version) map {
      Opts.doc.title(_) ++ Opts.doc.version(_)
    },
    // prevents sbteclipse from including java source directories
    // TODO: the following setting causes an empty crossProject.jvm build
    // unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test    <<= (scalaSource in Test)   (Seq(_)),
    scalacOptions in (Compile, doc) ++= List("-diagrams", "-implicits"),
    scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
      Seq("-doc-root-content", d / "rootdoc.txt" getPath)
    },
    autoAPIMappings := true,
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.8.2"                       % "test",
      "org.scalatest"  %% "scalatest"  % "2.2.6"        % "test",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5" % "test"),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => Nil
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>  Seq(
          "org.scala-lang.modules" %% "scala-xml" % "1.0.5" % "test" // required by ScalaTest
        )
      case _ => Nil
    })
  )
}
