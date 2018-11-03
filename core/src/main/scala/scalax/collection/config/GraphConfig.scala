package scalax.collection
package config

import mutable.ArraySet

/** Base trait for `Graph` configuration and optimization hints. This type is used by
  *  the implicit parameter of `Graph` factory methods. */
trait GraphConfig {

  /** Indicates the expected number of nodes in the graph. */
  def orderHint: Int
}
object GraphConfig {
  val defaultOrder = 4000
}

/** To be mixed in by any concrete class extending `GraphConfig` if the `Graph`
  *  implementation is based on adjacency lists using `ArraySet`. */
trait AdjacencyListArrayConfig {
  this: GraphConfig =>

  def adjacencyListHints: ArraySet.Hints
}

/** Configuration options for `Graph` factory methods in the core module. */
case class CoreConfig(override val orderHint: Int = GraphConfig.defaultOrder,
                      override val adjacencyListHints: ArraySet.Hints = ArraySet.Hints.Default)
    extends GraphConfig
    with AdjacencyListArrayConfig
