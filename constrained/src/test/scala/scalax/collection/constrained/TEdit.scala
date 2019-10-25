package scalax.collection.constrained

import org.scalatest.Suites

import scalax.collection.{ConfigWrapper, TEdit}

class TEditRootTest
    extends Suites(
      new TEdit[Graph](new ConfigWrapper[Graph] {
        val companion = Graph
        val config    = Graph.defaultConfig
      }),
      new TEdit[immutable.Graph](new ConfigWrapper[immutable.Graph] {
        val companion = immutable.Graph
        val config    = Graph.defaultConfig
      }),
      new TEdit[mutable.Graph](new ConfigWrapper[mutable.Graph] {
        val companion = mutable.Graph
        val config    = Graph.defaultConfig
      })
    )
