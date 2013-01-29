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
    settings = defaultSettings ++ (
	  Seq(
      name      := "Graph Core",
      version   := Version.core
      ) ++
      ( if (Version.scala_2_9) Seq()
        else 
          Seq(
            libraryDependencies ++= Seq(
              "org.scala-lang" % "scala-actors"  % Version.compiler)
          )
      )
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
	  libraryDependencies ++= Seq(
	    "net.liftweb" % "lift-json_2.9.1" % "2.4"
      )
	)
  ) dependsOn (core)
  lazy val misc = Project(
    id = "Graph-misc",
    base = file("misc"),
    settings = defaultSettings ++ Seq(
      name      := "Graph Miscellaneous",
      version   := Version.misc,
	  libraryDependencies ++= Seq(
	    "ch.qos.logback" % "logback-classic" % "1.0.7"
      )
    )
  ) dependsOn (core)

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := Version.compiler,
    organization := "com.assembla.scala-incubator",
    parallelExecution in Test := false,
    scalacOptions ++= Seq(Opts.compile.deprecation,
                          Opts.compile.unchecked),
    scalacOptions in (Compile, doc) <++= (name, version) map {
      Opts.doc.title(_) ++ Opts.doc.version(_)
    },
    // prevents sbteclipse from including java source directories
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test    <<= (scalaSource in Test)   (Seq(_)),
//    scalacOptions in doc ++= Seq(
//      "-doc-root-content", "src/main/scala/rootdoc.txt"
//    ),
    testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.8.2" % "test",
      if (Version.scala_2_9)
           "org.scalatest" %% "scalatest" % "1.8" % "test"
      else "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    )
  ) ++ GraphSonatype.settings ++ (
    if (Version.compilerIsRC) Seq(
      // https://groups.google.com/forum/?fromgroups=#!topic/scala-internals/h2YhIEg8lMc
      scalaBinaryVersion := Version.compiler)
    else Seq()
  )
}