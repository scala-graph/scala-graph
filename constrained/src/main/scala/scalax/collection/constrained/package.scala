package scalax.collection

import language.{higherKinds, implicitConversions}

import GraphPredef._
import GraphEdge._
import config.GraphConfig

/** Traits enabling to implement constraints and use constrained graphs.
  *
  * Graphs may be constrained dynamically or statically.
  *
  * ''Dynamically constrained'' means that a constraint is bound to a constrained `Graph`
  * instance at initialization time. The constrained `Graph` will then delegate all calls
  * to the methods of `ConstraintMethods` and `ConstraintHandlerMethods` to the
  * corresponding methods of the constraint bound to it.
  * The immutable and mutable factories `Graph` in this package yield dynamically
  * constrained graphs.
  *
  * To make use of dynamically constrained graphs you may make use of the predefined
  * constraints or provide an own implementation of `Constraint` along with its companion
  * object. To initialize a graph with one or several combined constraints just call
  * the graph factory methods of the `constraint` package passing.
  *
  * ''Statically constrained'' means that the graph class directly implements
  * the methods declared in `ConstraintMethods`.
  *
  * @author Peter Empen
  */
package object constrained {
  import scalax.collection.mutable.ArraySet
  import constraints._
  import generic._
  import config.ConstrainedConfig

  /** Aims defining a constraint valid for `Graph` instances in the scope:
    *{{{
    *implicit val config: Config = Acyclic
    *val g = Graph(0 ~> 3) // g is constrained to Acyclic
    *}}}
    */
  type Config = ConstrainedConfig

  /** Companion object to configure `Graph` instances in the scope including `ArraySet.Hints`:
    *{{{
    *implicit val config = Config(Acyclic)(ArraySet.Hints(64, 0, 64, 75))
    *val g = Graph(0 ~> 3) // g is constrained to Acyclic using the above optimization hints
    *}}}
    */
  def Config = ConstrainedConfig

  /** Converts `constraint` to an instance of `config.ConstrainedConfig`. */
  implicit def constraintToConfig(constraint: ConstraintCompanion[Constraint])(
      implicit orderHint: Int = GraphConfig.defaultOrder,
      adjacencyListHints: ArraySet.Hints = ArraySet.Hints()) =
    Config(orderHint, adjacencyListHints, constraint)

  /** Enables to quickly assemble immutable constrained graph companion modules. Example:
    *  {{{
    *  import scalax.collection.constrained.CompanionAlias
    *  import scalax.collection.constrained.constraints.Acyclic
    *
    *  object DAG extends CompanionAlias[DiEdge](Acyclic withStringPrefix "DAG")
    *  }}}
    */
  abstract class CompanionAlias[E[X] <: EdgeLikeIn[X]](constraintCompanion: ConstraintCompanion[Constraint])(
      implicit adjacencyListHints: ArraySet.Hints = ArraySet.Hints())
      extends GraphConstrainedCompanionAlias[Graph, E](Graph, constraintCompanion)(adjacencyListHints)

  /** Constraint representing a DAG. */
  def dagConstraint = Acyclic withStringPrefix "DAG"

  /** Default (immutable) directed acyclic `Graph`. */
  type DAG[N] = Graph[N, DiEdge]

  /** Companion module for default (immutable) directed acyclic `Graph`. */
  object DAG extends CompanionAlias[DiEdge](dagConstraint)

  /** Constraint representing a forest. */
  def forestConstraint = Acyclic withStringPrefix "Forest"

  /** Default (immutable) undirected acyclic `Graph`. */
  type Forest[N] = Graph[N, UnDiEdge]

  /** Companion module for default (immutable) undirected acyclic `Graph`. */
  object Forest extends CompanionAlias[UnDiEdge](forestConstraint)

  /** Constraint representing an undirected tree. */
  def treeConstraint = Connected && Acyclic withStringPrefix "Tree"

  /** Default (immutable) undirected connected acyclic `Graph`. */
  type Tree[N] = Graph[N, UnDiEdge]

  /** Companion module for default (immutable) undirected connected acyclic `Graph`. */
  object Tree extends CompanionAlias[UnDiEdge](treeConstraint)
}
