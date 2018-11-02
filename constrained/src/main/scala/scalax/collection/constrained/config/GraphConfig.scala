package scalax.collection.constrained
package config

import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}
import scalax.collection.mutable.ArraySet

import constraints.NoneConstraint

/** To be mixed in by any concrete class extending `GraphConfig` if the `Graph`
  *  is constrained. */
trait GenConstrainedConfig extends GraphConfig {
  def constraintCompanion: ConstraintCompanion[Constraint]
}

/** Configuration options for `Graph` factory methods in the constrained module. */
case class ConstrainedConfig(override val orderHint: Int = GraphConfig.defaultOrder,
                             override val adjacencyListHints: ArraySet.Hints = ArraySet.Hints.Default,
                             override val constraintCompanion: ConstraintCompanion[Constraint] = NoneConstraint)
    extends GenConstrainedConfig
    with AdjacencyListArrayConfig
