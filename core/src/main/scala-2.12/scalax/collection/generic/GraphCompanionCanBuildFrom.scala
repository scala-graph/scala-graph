package scalax.collection.generic

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.{Graph, GraphLike}

private[collection]
trait GraphCompanionCanBuildFrom[+CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] {
  this: GraphCompanion[CC] =>

  class GraphCanBuildFrom[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config)
    extends CanBuildFrom[Coll @uncheckedVariance, Param[N, E], CC[N, E]] {

    def apply(from: Coll @uncheckedVariance) = newBuilder[N, E]
    def apply()                              = newBuilder[N, E]
  }
}
