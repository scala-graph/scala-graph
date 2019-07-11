package scalax.collection.constrained
package generic

import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.{EdgeLikeIn, InParam, Param}
import scalax.collection.generic.GraphCompanion
import scalax.collection.mutable.ArraySet
import scalax.collection.config.GraphConfig

import mutable.GraphBuilder
import config.ConstrainedConfig

/** Methods common to `Graph` companion objects in the constrained module. */
trait GraphConstrainedCompanion[+GC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, GC]]
    extends GraphCompanion[GC] {
  type Config = ConstrainedConfig
  def defaultConfig = ConstrainedConfig()

  /** Same as `from` except for constraint being suppressed. */
  protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): GC[N, E]
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                    config: Config): Builder[Param[N, E], GC[N, E]] =
    new GraphBuilder[N, E, GC](this)(edgeT, config)
}

abstract class GraphConstrainedCompanionAlias[GC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, GC],
                                              E[X] <: EdgeLikeIn[X]](
    companion: GraphConstrainedCompanion[GC],
    constraintCompanion: ConstraintCompanion[Constraint])(
    implicit adjacencyListHints: ArraySet.Hints = ArraySet.Hints()) {

  def empty[N](implicit edgeT: ClassTag[E[N]], config: GraphConfig): Graph[N, E] =
    companion.empty(edgeT, constraintCompanion)

  def apply[N](elems: InParam[N, E]*)(implicit edgeT: ClassTag[E[N]], config: GraphConfig): Graph[N, E] =
    companion(elems: _*)(edgeT, constraintCompanion)

  def from[N](nodes: Traversable[N], edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]],
                                                               config: GraphConfig): Graph[N, E] =
    companion.from(nodes, edges)(edgeT, constraintCompanion)
}

trait MutableGraphCompanion[+GC[N, E[X] <: EdgeLikeIn[X]] <: mutable.Graph[N, E] with mutable.GraphLike[N, E, GC]]
    extends GraphConstrainedCompanion[GC] {
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                    config: Config): Builder[Param[N, E], GC[N, E] @uncheckedVariance] =
    new GraphBuilder[N, E, GC](this)(edgeT, config)
}

trait ImmutableGraphCompanion[+GC[N, E[X] <: EdgeLikeIn[X]] <: immutable.Graph[N, E] with GraphLike[N, E, GC]]
    extends GraphConstrainedCompanion[GC]
