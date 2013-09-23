package scalax.collection.centrality

import language.higherKinds
import math.{ceil, min}

import scalax.collection.GraphPredef._
import scalax.collection.Graph

/** Calculation of node centrality based on Katz centrality.
 */
object Katz {
  // Minimum 0.5, increasing with higher order
  private def defaultAttenuation(order: Int): Float =
    1f - (1f / (ceil(order.toFloat / 10).toFloat + 1))
      
  implicit class Centrality[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N, E]) {
  
    /** Calculates the centrality of each node contained in `nodes`.
     */
    def centralities[G <: Graph[N, E] with Singleton]
                    (nodes: G#NodeSetT = g.nodes.asInstanceOf[G#NodeSetT],
                     maxDepth: Int = 0)
                    (implicit attenuationFactor: Int => Float = defaultAttenuation)
        : Map[G#NodeT, Float] = {
      
      assert(nodes.headOption map (_.containingGraph eq g) getOrElse true)

      val factor: Float = attenuationFactor(nodes.size) 
      val degrees: Map[G#NodeT, Int] = {
        val b = Map.newBuilder[G#NodeT, Int]
        nodes foreach ((n: G#NodeT) => b += ((n, n.degree)))
        b.result
      }
      object Factor {
        private val limit = min(g.order, 5000)
        private val factors = {
          var i = 0
          var lastFactor = 1f
          Array.fill(limit) {
            val thisFactor = lastFactor 
            lastFactor *= factor
            thisFactor
          }
        }
        private val minFactor = factors(limit - 1)
        def apply(index: Int) =
          if (index < limit) factors(index) else minFactor
      }

      val weightBuilder = Map.newBuilder[G#NodeT, Float]
      nodes.asInstanceOf[g.NodeSetT] foreach { n =>
        import scalax.collection.GraphTraversal.VisitorReturn._
        import scalax.collection.GraphTraversalImpl._
        import g.ExtendedNodeVisitor

        var weight = 0f
        def visitor = ExtendedNodeVisitor((node, count, depth, informer) =>
          if (depth > maxDepth) Cancel
          else {
            weight += degrees(node.asInstanceOf[G#NodeT]) * Factor(depth)
            Continue
          }
        )
        n.traverse()(nodeVisitor = visitor)
        weightBuilder += ((n.asInstanceOf[G#NodeT], weight))
      }
      weightBuilder.result
    }
  }

  def centralityMapOrdering[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E] with Singleton]
     (centralities: Map[G#NodeT, Float]): Ordering[(G#NodeT, Float)] =
    new Ordering[(G#NodeT, Float)] {
      def compare(x: (G#NodeT, Float), y: (G#NodeT, Float)) = x._2 compare y._2
    }

  type ProjectionNodeCentrality[N, E[X] <: EdgeLikeIn[X]] = (Graph[N,E]#NodeT, Float)

  def centralityProjectionMapOrdering[N, E[X] <: EdgeLikeIn[X]]
     (centralities: Map[_ <: Graph[N,E]#NodeT, Float]): Ordering[ProjectionNodeCentrality[N,E]] =
    new Ordering[ProjectionNodeCentrality[N,E]] {
      def compare(x: ProjectionNodeCentrality[N,E], y: ProjectionNodeCentrality[N,E]) =
        x._2  compare y._2
    }
}