package scalax.collection.generic

import language.higherKinds

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.config.GraphConfig
import scalax.collection.{Graph, GraphLike}

private[collection]
trait GraphCompanionBase[+CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] {

  /** Type of configuration required for a specific `Graph` companion. */
  type Config <: GraphConfig

  private type Coll = CC[Nothing, Nothing] @uncheckedVariance

  def newBuilder[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                            config: Config): Builder[Param[N, E], CC[N, E]]

  class GraphCanBuildFrom[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config)
    extends CanBuildFrom[Coll @uncheckedVariance, Param[N, E], CC[N, E]] {

    def apply(from: Coll @uncheckedVariance) = newBuilder[N, E]
    def apply()                              = newBuilder[N, E]
  }
}
