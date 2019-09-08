package scalax.collection.constrained

import scala.language.higherKinds

/** Mutable constrained graph templates.
  *
  * @author Peter Empen
  */
package object mutable {
  import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
  import scalax.collection.mutable.ArraySet
  import generic._

  /** Enables to quickly assemble mutable constrained graph companion modules. Example:
    *  {{
    *  import scalax.collection.constrained.mutable.CompanionAlias
    *  import scalax.collection.constrained.constraints.Acyclic
    *
    *  object DAG extends CompanionAlias[DiEdge](Acyclic withStringPrefix "DAG")
    *  }}
    */
  abstract class CompanionAlias[E[X] <: EdgeLikeIn[X]](constraintCompanion: ConstraintCompanion[Constraint])(
      implicit adjacencyListHints: ArraySet.Hints = ArraySet.Hints())
      extends GraphConstrainedCompanionAlias[Graph, E](Graph, constraintCompanion)(adjacencyListHints)

  /** Mutable directed acyclic `Graph`. */
  type DAG[N] = Graph[N, DiEdge]

  /** Companion module for mutable directed acyclic `Graph`. */
  object DAG extends CompanionAlias[DiEdge](dagConstraint)

  /** Mutable undirected acyclic `Graph`. */
  type Forest[N] = Graph[N, UnDiEdge]

  /** Companion module for mutable undirected acyclic `Graph`. */
  object Forest extends CompanionAlias[UnDiEdge](forestConstraint)

  /** Mutable undirected connected acyclic `Graph`. */
  type Tree[N] = Graph[N, UnDiEdge]

  /** Companion module for mutable undirected connected acyclic `Graph`. */
  object Tree extends CompanionAlias[UnDiEdge](treeConstraint)
}
