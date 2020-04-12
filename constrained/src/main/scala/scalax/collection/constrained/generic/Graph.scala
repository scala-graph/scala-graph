package scalax.collection.constrained
package generic

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.{EdgeLike, InParam, Param}
import scalax.collection.generic.GraphCompanion
import scalax.collection.mutable.ArraySet
import scalax.collection.config.GraphConfig

import mutable.GraphBuilder
import config.ConstrainedConfig

/** Methods common to `Graph` companion objects in the constrained module. */
trait GraphConstrainedCompanion[+GC[N, E] <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, GC]]
    extends GraphCompanion[GC] {
  type Config = ConstrainedConfig
  def defaultConfig = ConstrainedConfig()

  /** Same as `from` except for constraint being suppressed. */
  protected[collection] def fromWithoutCheck[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(

      config: Config): GC[N, E]
  override def newBuilder[N, E <: EdgeLike[N]](
                                                     config: Config): Builder[Param[N, E], GC[N, E]] =
    new GraphBuilder[N, E, GC](this)(config)
}

abstract class GraphConstrainedCompanionAlias[GC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, GC],
                                              E <: EdgeLike[N]](
    companion: GraphConstrainedCompanion[GC],
    constraintCompanion: ConstraintCompanion[Constraint])(
    implicit adjacencyListHints: ArraySet.Hints = ArraySet.Hints()) {

  def empty[N](implicit config: GraphConfig): Graph[N, E] =
    companion.empty( constraintCompanion)

  def apply[N](elems: InParam[N, E]*)(implicit config: GraphConfig): Graph[N, E] =
    companion(elems: _*)( constraintCompanion)

  def from[N](nodes: Iterable[N], edges: Iterable[E])(
                                                         config: GraphConfig): Graph[N, E] =
    companion.from(nodes, edges)( constraintCompanion)
}

trait MutableGraphCompanion[+GC[N, E <: EdgeLike[N]] <: mutable.Graph[N, E] with mutable.GraphLike[N, E, GC]]
    extends GraphConstrainedCompanion[GC] {
  override def newBuilder[N, E <: EdgeLike[N]](

      config: Config): Builder[Param[N, E], GC[N, E] @uncheckedVariance] =
    new GraphBuilder[N, E, GC](this)(config)
}

trait ImmutableGraphCompanion[+GC[N, E <: EdgeLike[N]] <: immutable.Graph[N, E] with GraphLike[N, E, GC]]
    extends GraphConstrainedCompanion[GC]
