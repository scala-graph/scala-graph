package scalax.collection.constrained

import org.scalatest.Suites

import scalax.collection.{ConfigWrapper, Editing}

class TEditRootTest
    extends Suites(
      new Editing[Graph](new ConfigWrapper[Graph] {
        val companion = Graph
        val config    = Graph.defaultConfig
      }),
      new Editing[immutable.Graph](new ConfigWrapper[immutable.Graph] {
        val companion = immutable.Graph
        val config    = Graph.defaultConfig
      }),
      new Editing[mutable.Graph](new ConfigWrapper[mutable.Graph] {
        val companion = mutable.Graph
        val config    = Graph.defaultConfig
      })
    )
