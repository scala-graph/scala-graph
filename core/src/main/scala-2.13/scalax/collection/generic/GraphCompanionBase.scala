package scalax.collection.generic

import language.higherKinds
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder
import scala.reflect.ClassTag
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.config.GraphConfig
import scalax.collection.{Graph, GraphLike}

import scala.collection.BuildFrom

private[collection]
trait GraphCompanionBase[+CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] {

  /** Type of configuration required for a specific `Graph` companion. */
  type Config <: GraphConfig

  private type Coll = CC[Nothing, Nothing] @uncheckedVariance

  def newBuilder[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                            config: Config): Builder[Param[N, E], CC[N, E]]

  class GraphCanBuildFrom[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config)
    extends BuildFrom[Coll @uncheckedVariance, Param[N, E], CC[N, E]] {

    def fromSpecific(from: Coll @uncheckedVariance)(it: IterableOnce[Param[N, E]]) = newBuilder(from).addAll(it).result()
    def newBuilder(from: Coll @uncheckedVariance) = GraphCompanionBase.this.newBuilder(edgeT, config)
  }
}
