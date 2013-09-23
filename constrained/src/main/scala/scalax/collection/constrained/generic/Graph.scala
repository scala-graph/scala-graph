package scalax.collection.constrained
package generic

import scala.language.{higherKinds, postfixOps}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Iterable
import scala.collection.mutable.{Builder, ListBuffer}
import scala.collection.generic.CanBuildFrom

import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParamIn}
import scalax.collection.generic.GraphCompanion
import scalax.collection.mutable.ArraySet
import scalax.collection.config.GraphConfig
import scalax.collection.io._

import constraints.NoneConstraint
import mutable.GraphBuilder
import config.ConstrainedConfig

/** Methods common to `Graph` companion objects in the constrained module. */
trait GraphConstrainedCompanion[+GC[N,E[X]<:EdgeLikeIn[X]] <:
                                 Graph[N,E] with GraphLike[N,E,GC]]
  extends GraphCompanion[GC]
{
  type Config = ConstrainedConfig
  def defaultConfig = ConstrainedConfig()
  /** Same as `from` except for constraint being suppressed. */
  protected[collection] def fromUnchecked[N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config) : GC[N,E]
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]]
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Builder[GraphParamIn[N,E], GC[N,E]] =
    new GraphBuilder[N,E,GC](this)(edgeManifest, config)
}
abstract class GraphConstrainedCompanionAlias
              [GC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,GC],
               E[X] <: EdgeLikeIn[X]]
    (companion: GraphConstrainedCompanion[GC],
     constraintCompanion: ConstraintCompanion[Constraint])
    (implicit adjacencyListHints: ArraySet.Hints = ArraySet.Hints())
{
  def empty[N](implicit edgeManifest: Manifest[E[N]],
                        config: GraphConfig): Graph[N,E] =
    companion.empty(edgeManifest, constraintCompanion)

  def apply[N](elems: GraphParamIn[N,E]*)
                       (implicit edgeManifest: Manifest[E[N]],
                        config: GraphConfig): Graph[N,E] =
    companion(elems: _*)(edgeManifest, constraintCompanion)

  def from[N](nodes: Iterable[N],
                       edges: Iterable[E[N]])
                      (implicit edgeManifest: Manifest[E[N]],
                       config: GraphConfig): Graph[N,E] =
    companion.from(nodes, edges)(edgeManifest, constraintCompanion)

  def fromStream[N]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: GraphConfig): Graph[N,E] =
    companion.fromStream(nodeStreams, nodes, edgeStreams, edges)(
                         edgeManifest, constraintCompanion)
}
trait MutableGraphCompanion[+GC[N,E[X]<:EdgeLikeIn[X]] <:
                             mutable.Graph[N,E] with mutable.GraphLike[N,E,GC]]
  extends GraphConstrainedCompanion[GC]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]]
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Builder[GraphParamIn[N,E], GC[N,E] @uncheckedVariance] =
    new GraphBuilder[N,E,GC](this)(edgeManifest, config)
}
trait ImmutableGraphCompanion[+GC[N,E[X]<:EdgeLikeIn[X]] <:
                               immutable.Graph[N,E] with GraphLike[N,E,GC]]
  extends GraphConstrainedCompanion[GC]
