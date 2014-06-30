import sbt._
import Keys._

object GraphBuild extends Build {

  lazy val all = Project(
    id = "Graph-all",
    base = file("."),
    settings = defaultSettings ++ Seq(
      name      := "Graph for Scala",
      version   := Version.all,
      publishTo := None
	  ),
    aggregate = Seq(core, constrained, dot, json)
  )

  lazy val core = Project(
    id = "Graph-core",
    base = file("core"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Core",
      version   := Version.core,
	  libraryDependencies += "com.assembla.scala-incubator" %% "graph-test" % "1.9.0" % "test"
    )
  )

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
      libraryDependencies += "net.liftweb" %% "lift-json" % "2.6-M4"
    )
  ) dependsOn (core)

  lazy val test = Project(
    id = "Graph-test",
    base = file("testutil"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Test",
      version   := Version.test,
	  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3"
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

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := Version.compiler,
	crossScalaVersions  := Seq(scalaVersion.value, Version.compiler_2),
    organization := "com.assembla.scala-incubator",
    parallelExecution in Test := false,
    scalacOptions in (Compile, doc) <++= (name, version) map {
      Opts.doc.title(_) ++ Opts.doc.version(_)
    },
    // prevents sbteclipse from including java source directories
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test    <<= (scalaSource in Test)   (Seq(_)),
    scalacOptions in (Compile, doc) ++= List("-diagrams", "-implicits"),
    scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
      Seq("-doc-root-content", d / "rootdoc.txt" getPath)
    },
	autoAPIMappings := true,
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.8.2" % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest"  %% "scalatest"  % "2.1.3"  % "test",
      "org.scalacheck" %% "scalacheck" % "1.11.3" % "test") /*
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) => 
      case Some((2, scalaMajor)) if scalaMajor >= 11 => 
      case _ =>
    }) */
  ) ++ GraphSonatype.settings ++ (
    if (Version.compilerIsRC) Seq(
      // https://groups.google.com/forum/?fromgroups=#!topic/scala-internals/h2YhIEg8lMc
      scalaBinaryVersion := Version.compiler)
    else Seq()
  )
}