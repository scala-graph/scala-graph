package scalax.collection.constrained
package config

import scalax.collection.config.{GraphConfig, AdjacencyListArrayConfig}
import scalax.collection.mutable.ArraySet

import constraints.NoneConstraint

/** To be mixed in by any concrete class extending `GraphConfig` if the `Graph`
 *  is constrained. */
trait GenConstrainedConfig {
  this: GraphConfig =>
  def constraintCompanion: ConstraintCompanion[Constraint] 
}
/** Configuration options for `Graph` factory methods in the constrained module. */
case class ConstrainedConfig
    (override val adjacencyListHints : ArraySet.Hints                  = ArraySet.Hints.Default,
     override val constraintCompanion: ConstraintCompanion[Constraint] = NoneConstraint)
  extends GraphConfig
  with    GenConstrainedConfig
  with    AdjacencyListArrayConfig